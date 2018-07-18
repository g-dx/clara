package main

import (
	"fmt"
	"strconv"
	"math"
)

var stringOps = make(map[string]operand)

/*

 System V AMD64 ABI:
 -------------------
 - Parameters passed RTL in RDI, RSI, RDX, RCX, R8, R9 followed by stack
 - Caller cleans up
 - Callee must restore RBX, RBP, and R12–R15 if used otherwise all other registers are available for use.
 - Stack should be aligned on 16-byte boundary
 - 128 byte red zone below stack

  https://en.wikipedia.org/wiki/X86_calling_conventions#System_V_AMD64_ABI

*/

var claralloc = "clara_claralloc.int" // NOTE: keep in sync with ASM name generation!

var regs = []reg{rdi, rsi, rdx, rcx, r8, r9}

// Current function being compiled
type function struct {
	AstName string
	Type    *FunctionType
	gcRoots *GcState
	gcFns   map[string]GcRoots
}

func (f *function) AsmName() string {
	return f.Type.AsmName(f.AstName)
}

func (f *function) NewGcFunction() string {
	roots := f.gcRoots.Snapshot()
	if len(roots) == 0 {
		return noGc
	}
	name := fmt.Sprintf("%v_gc%v", f.AsmName(), roots.String())
	f.gcFns[name] = roots
	return name
}

func codegen(symtab *SymTab, tree []*Node, asm asmWriter) error {

	// ---------------------------------------------------------------------------------------
	// Assembly Generation Start
	// ---------------------------------------------------------------------------------------

	asm.tab(".data")

	// Output strings
	// TODO: Delay writing string literals until end of assembly generation
	symtab.Walk(func(s *Symbol) {
		if s.Type.Is(String) && s.IsLiteral {
			stringOps[s.Name] = asm.stringLit(s.Name)
		}
	})

	asm.spacer()

	// Output func calls
	asm.tab(".text")
	for _, n := range tree {
		if n.isFuncDcl() {
			genFunc(asm, n)
		}
	}

	genIoobHandler(asm);
	asm.spacer()
	genFramePointerAccess(asm)
	asm.spacer()
	genTypeGcFuncs(asm, symtab.allTypes()) 	// Generate per-type GC functions
	asm.spacer()
	genNoTraceGcFunc(asm) 	// No trace GC function
	asm.spacer()
	genInvokeDynamic(asm) // Closure invocation support
	asm.spacer()
	genClosureGc(asm) // Closure GC tracing support
	return nil
}

func genFunc(asm asmWriter, n *Node) {

	fn := &function{ AstName: n.sym.Name, Type: n.sym.Type.AsFunction(), gcRoots: &GcState{}, gcFns: make(map[string]GcRoots) }

	// Ensure we only generate code for "our" functions
	if !fn.Type.IsExternal() {

		// Assign stack offsets for temporaries
		temps := len(fn.Type.Args)
		walk(postOrder, n, n.symtab, n, func(root *Node, symTab *SymTab, n *Node) error {
			// Look for symbols which should be on the stack but have no address
			if n.sym != nil && n.sym.IsStack && n.sym.Addr == 0 {
				n.sym.Addr = ptrSize * (temps + 1) // Assign a stack slot for temporary
				temps++
			}
			return nil
		})

		// If this function is a closure output the address of the tracing function before it
		if fn.Type.IsClosure() {
			asm.addr(fnOp(fn.Type.AsClosure().gcFunc))
		}

		// Generate standard entry sequence
		genFnEntry(asm, fn.AsmName(), temps)

		// Copy register values into stack slots
		for i, param := range n.params {
			addr := ptrSize * (i + 1) // Assign a stack slot for var
			param.sym.Addr = addr
			param.sym.IsStack = true
			asm.ins(movq, regs[i], rbp.displace(-addr))
			fn.gcRoots.Add(addr, param.typ)
		}

		// Generate functions
		if fn.Type.IsStructCons() {
			genConstructor(asm, fn, n.params)
		} else {
			// Generate code for all statements
			genStmtList(asm, n.stmts, fn)

			// Handle result in expression func
			if n.op == opExprFnDcl {
				genPrepareResult(asm, n.stmts[0], fn)
			}
		}

		// Ensure stack cleanup for functions which do not explicitly terminate via a `return`
		if !n.IsReturnLastStmt() {
			genFnExit(asm, n.sym.Name == "main")
		}
		asm.spacer()

		// Generate stack frame GC functions
		genStackFrameGcFuncs(asm, fn)
	}
}

func genNoTraceGcFunc(asm asmWriter) {
	genFnEntry(asm, noGc, 0)
	genFnExit(asm, false)
}

func genStackFrameGcFuncs(asm asmWriter, fn *function) {
	for name, roots := range fn.gcFns {

		// TODO: Delay writing string literals until end of assembly generation
		asm.tab(".data")
		debug := asm.stringLit(fmt.Sprintf("\"\\n%v\\n\"", fn.Type.Describe(fn.AstName)))
		asm.tab(".text")

		genFnEntry(asm, name, 1)
		asm.ins(movq, rdi, rbp.displace(-ptrSize)) // Copy frame pointer into local slot

		genDebugGcPrintf(asm, debug)

		// Invoke GC function for type of each root
		for _, root := range roots {
			asm.ins(movq, rbp.displace(-ptrSize), rax)  // load frame pointer
			asm.ins(leaq, rax.displace(-root.off), rdi) // load slot stack address
			if root.typ.Is(Function) {
				asm.ins(call, fnOp(closureGc))
			} else {
				asm.ins(call, fnOp(root.typ.GcName()))
			}
		}
		genFnExit(asm, false) // Called from Clara code -
		asm.spacer()
	}
}

func genTypeGcFuncs(asm asmWriter, types []*Type) {
	for _, typ := range types {
		if typ.IsPointer() && !typ.Is(Function) { // closureGc handles functions
			asm.spacer()
			genTypeGcFunc(asm, typ)
		}
	}
}

func genTypeGcFunc(asm asmWriter, t *Type) {

	//
	// GC functions take a single parameter which is the stack address of the slot to trace
	//

	// TODO: Delay writing string literals until end of assembly generation
	asm.tab(".data")
	debug := asm.stringLit(fmt.Sprintf("\"  - (0x%%lx) '%v' \\n\"", t))
	asm.tab(".text")

	genFnEntry(asm, t.GcName(), 1)
	asm.ins(movq, rdi.deref(), rdi)            // Deref stack slot to get pointer into heap
	asm.ins(movq, rdi, rbp.displace(-ptrSize)) // Copy into local slot

	// Calculate pointer to block from type
	asm.ins(leaq, rdi.displace(-16), rsi) // *type-16 -> *block TODO: Is this safe if the pointer is NULL?
	genDebugGcPrintf(asm, debug)

	// Labels
	exit := asm.newLabel("exit")

	// Dereference pointer & load header into rax.
	asm.ins(movq, rbp.displace(-ptrSize), rax) // Copy stack slot
	asm.ins(subq, intOp(gcHeaderSize), rax)    // *type-8 -> *header

	// if header & 1 == 1 (i.e. already marked)
	asm.ins(movq, rax.deref(), rbx)
	asm.ins(andq, intOp(1), rbx)
	asm.ins(cmpq, _true, rbx)
	asm.ins(je, labelOp(exit))

	// || header & 2 == 2 (i.e. is ready only)
	asm.ins(movq, rax.deref(), rbx)
	asm.ins(andq, intOp(2), rbx)
	asm.ins(cmpq, intOp(2), rbx)
	asm.ins(je, labelOp(exit))

	// header.marked = true
	asm.ins(orq, intOp(1), rax.deref())

	switch t.Kind {
	case String, Array:
		// Nothing to do...
	case Struct:
		// Invoke GC for pointer fields
		for _, f := range t.AsStruct().Fields {
			if f.Type.IsPointer() {
				asm.ins(movq, rbp.displace(-ptrSize), rdi)
				asm.ins(addq, intOp(f.Addr), rdi) // Increment pointer to offset of field
				if f.Type.Is(Function) {
					asm.ins(call, fnOp(closureGc))
				} else {
					asm.ins(call, fnOp(f.Type.GcName()))
				}
			}
		}
	default:
		panic(fmt.Sprintf("Unexpected type for GC: %v\n", t.AsmName()))
	}

	// Exit
	asm.label(exit)
	genFnExit(asm, true) // assembly defined caller
}

// printf args must already be setup in appropriate registers
func genDebugGcPrintf(asm asmWriter, template operand) {

	exit := asm.newLabel("exit")
	asm.ins(movabs, symOp("debugGc"), rax) // Defined in runtime.c!
	asm.ins(movq, rax.deref(), rax)       // Get value
	asm.ins(cmpq, _true, rax)
	asm.ins(jne, labelOp(exit))
	asm.ins(movabs, template, rdi)
	asm.ins(leaq, rdi.displace(8), rdi) // Skip past length!
	asm.ins(movq, intOp(0), rax)
	asm.ins(call, fnOp("printf"))
	asm.label(exit)

}

func genClosureGc(asm asmWriter) {
	genFnEntry(asm, closureGc, 1)

	exit := asm.newLabel("exit")

	// Deref stack slot to get pointer
	asm.ins(movq, rdi.deref(), rax)

	// Check for read-only (header & 2 == 2)
	asm.ins(movq, rax.displace(-ptrSize), rbx)
	asm.ins(andq, intOp(2), rbx)
	asm.ins(cmpq, intOp(2), rbx)
	asm.ins(je, labelOp(exit))

	// Get function pointer, load & call GC address (stored 16 bytes _behind_)
	asm.ins(movq, rax.deref(), rax)
	asm.ins(movq, rax.displace(-16), rax)
	asm.ins(call, rax.indirect())

	asm.label(exit)
	genFnExit(asm, true) // assembly defined caller
}

func genInvokeDynamic(asm asmWriter) {
	genFnEntry(asm, fnPrefix + "invokeDynamic", 1)
	callLabel := asm.newLabel("call")
	fnPtrLabel := asm.newLabel("fnPtr")

	// Check for read-only (header & 2 == 2)
	asm.ins(movq, rdi.displace(-ptrSize), rax)
	asm.ins(andq, intOp(2), rax)
	asm.ins(cmpq, intOp(2), rax)
	asm.ins(je, labelOp(fnPtrLabel))

	// ----------------------------------------------------------------------------
	// Closure
	asm.ins(movq, rdi.deref(), rax) // Deref closure to get func pointer
	asm.ins(movq, rax, rbp.displace(-ptrSize)) // Copy func pointer to stack slot
	asm.ins(movq, rdi.displace(ptrSize), rdi) // Deref closure to get env
	asm.ins(jmp, labelOp(callLabel))

	// ----------------------------------------------------------------------------
	// Function Pointer
	asm.label(fnPtrLabel)
	asm.ins(movq, rdi, rbp.displace(-ptrSize)) // Copy *function to stack
	asm.ins(movq, rsi, rdi)
	asm.ins(movq, rdx, rsi)
	asm.ins(movq, rcx, rdx)
	asm.ins(movq, r8, rcx)
	asm.ins(movq, r9, r8)
	// ----------------------------------------------------------------------------

	// Make call
	asm.label(callLabel)
	asm.ins(call, rbp.displace(-ptrSize).indirect())
	asm.addr(fnOp(noGc))
	genFnExit(asm, false) // Called from Clara code
}

func genFramePointerAccess(asm asmWriter) {
	// Requires non-standard entry & exit!
	asm.function("getFramePointer")
	asm.ins(movq, rbp, rax)
	asm.ins(ret, )
}

func genFnEntry(asm asmWriter, name string, temps int) {
	if temps % 2 != 0 {
		temps += 1 // ALIGNMENT: Ensure even $rsp increment
	}
	asm.function(name)
	asm.ins(enter, intOp(temps*8), intOp(0))
}

func genFnExit(asm asmWriter, skipGc bool) {
	asm.ins(leave, )
	if skipGc {
		asm.ins(ret, )
		return
	}
	// Jump over frame GC function address
	// TODO: Is there any way to compress this?
	asm.ins(popq, rbx)
	asm.ins(addq, intOp(ptrSize), rbx)
	asm.ins(jmp, rbx.indirect())
}

func genIoobHandler(asm asmWriter) {

	// rbx is index register. See: codegen.go:647
	asm.label("ioob")
	asm.ins(movq, rbx, rdi) // NOTE: When stack machine changes to single reg machine or linear scan this must change too!
	asm.ins(call, fnOp("indexOutOfBounds"))
}

func genConstructor(asm asmWriter, f *function, params []*Node) {

	// Malloc memory of appropriate size
	asm.ins(movq, intOp(f.Type.ret.AsStruct().Size()), rdi)
	asm.ins(call, fnOp(claralloc)) // Implemented in lib/mem.clara
	asm.addr(fnOp(f.NewGcFunction()))

	// Copy stack values into fields
	off := 0
	for _, param := range params {
		id := param.sym
		// Can't move mem -> mem. Must go through a register.
		asm.ins(movq, rbp.displace(-id.Addr), rbx)
		asm.ins(movq, rbx, rax.displace(off))
		off += ptrSize
	}

	// Pointer is already in rax so nothing to do...
}

func genStmtList(asm asmWriter, stmts []*Node, fn *function) {
	fn.gcRoots.OpenScope()
	for _, stmt := range stmts {

		switch stmt.op {
		case opReturn:
			genReturnStmt(asm, stmt, fn)

		case opIf:
			genIfElseIfElseStmts(asm, stmt, fn)

		case opDas, opAs:
			genAssignStmt(asm, stmt, fn)

		case opWhile:
			genWhileStmt(asm, stmt, fn)

		default:
			genExpr(asm, stmt, 0, false, fn)
		}
	}
	fn.gcRoots.CloseScope()
}

func genWhileStmt(asm asmWriter, n *Node, fn *function) {

	// Create new labels
	exit := asm.newLabel("while_end")
	start := asm.newLabel("while_start")
	asm.label(start)

	// Generate condition
	genExpr(asm, n.left, 0, false, fn) // Left stores condition

	asm.ins(cmpq, _true, rax)    // Pop result from stack to rax
	asm.ins(jne, labelOp(exit))  // Jump over block if not true
	genStmtList(asm, n.stmts, fn)        // Generate stmts block
	asm.ins(jmp, labelOp(start)) // Jump to start of loop
	asm.label(exit)                      // Declare exit point
}

func genAssignStmt(asm asmWriter, n *Node, fn *function) {

	// Evaluate expression & save result to rcx
	genExpr(asm, n.right, 0, false, fn)
	asm.ins(movq, rax, rcx) // TODO: This is not safe because this register is used to pass parameters!

	// Evaluate mem location to store
	slot := n.left
	genExpr(asm, slot, 0, true, fn)

	// Check if int -> byte cast required
	if slot.typ.Is(Byte) && n.right.typ.Is(Integer) {
		asm.ins(movsbq, rcx._8bit(), rcx) // rcx = cl (sign extend lowest 8-bits into same reg)
	}

	// SPECIAL CASE: If destination is byte array only move a single byte. Byte values elsewhere (on stack & in structs) are in 8-byte slots
	if slot.sym.Type.IsArray(Byte) && (n.right.typ.Is(Byte) || n.right.typ.Is(Integer)) {
		asm.ins(movb, cl, rax.deref()) // [rax] = cl
	} else {
		asm.ins(movq, rcx, rax.deref()) // [rax] = rcx
	}

	// If decl & assign start tracking as gc root
	if n.op == opDas {
		fn.gcRoots.Add(slot.sym.Addr, slot.sym.Type)
	}
}

func genIfElseIfElseStmts(asm asmWriter, n *Node, fn *function) {

	// Generate exit label
	exit := asm.newLabel("if_end")

	cur := n
	for cur != nil {
		if cur.left != nil {
			// Generate condition
			genExpr(asm, cur.left, 0, false, fn) // Left stores condition

			// Create new label
			next := asm.newLabel("else")

			asm.ins(cmpq, _true, rax)   // Compare (true) to rax
			asm.ins(jne, labelOp(next)) // Jump over block if not equal

			genStmtList(asm, cur.stmts, fn)     // Generate if (true) stmt block
			asm.ins(jmp, labelOp(exit)) // Exit if/elseif/else block completely
			asm.label(next)                     // Label to jump if false
		} else {
			genStmtList(asm, cur.stmts, fn) // Generate block without condition (else block)
		}
		cur = cur.right // Move down tree
	}
	asm.label(exit) // Declare exit point
}

func genReturnStmt(asm asmWriter, retn *Node, fn *function) {

	// If return has expression evaluate it
	if retn.left != nil {
		genExpr(asm, retn.left, 0, false, fn)
		genPrepareResult(asm, retn.left, fn)
	}

	// Clean stack & return
	genFnExit(asm, false)
}

func genPrepareResult(asm asmWriter, n *Node, fn *function) {

	// Check if int -> byte cast required
	if n.typ.Is(Integer) && fn.Type.ret.Is(Byte) {
		asm.ins(movsbq, rax._8bit(), rax)
	}
}

func genFnCall(asm asmWriter, n *Node, f *function, regsInUse int) {

	// Determine how function is referenced
	var s *Symbol
	var fn *FunctionType
	if n.sym != nil {
		s = n.sym
		fn = s.Type.AsFunction()
	} else {
		asm.ins(movq, rax, r15) // TODO: This is a callee saved register. If we use it, we should save it!
		fn = n.left.typ.AsFunction()
	}

	spill(asm, regsInUse) // Spill any in use registers to the stack

	// Generate arg code
	for i, arg := range n.stmts {
		if i >= 6 {
			panic("Calling functions with more than 6 parameters not yet implemented")
		}
		genExpr(asm, arg, i, false, f) // Evaluate expression

		// Move result into reg

		// SPECIAL CASE: Modify the pointer to a string or array to point "past" the length to the data. This
		// means the value can be directly supplied to other libc functions without modification.
		if (arg.typ.Is(String) || arg.typ.Is(Array)) && fn.IsExternal() {
			asm.ins(leaq, rax.displace(8), regs[i])
		} else {
			asm.ins(movq, rax, regs[i])
		}

		// Check if int -> byte cast required
		if i < len(fn.Args) && arg.typ.Is(Integer) && fn.Args[i].Is(Byte) {
			asm.ins(movsbq, regs[i]._8bit(), regs[i])
		}
	}

	// Variadic functions must set rax to number of floating point parameters
	if fn.isVariadic {
		asm.ins(movq, intOp(0), rax) // No floating-point register usage yet...
	}

	// Call function
	if s == nil {
		asm.ins(call, r15.indirect()) // anonymous func call: register indirect
	} else if s.IsStack {
		asm.ins(call, rbp.displace(-s.Addr).indirect()) // Parameter func call: memory indirect
	} else {
		asm.ins(call, fnOp(fn.AsmName(s.Name))) // Named func call
	}

	// Only generate GC function addresses for Clara functions
	if !fn.IsExternal() {
		asm.addr(fnOp(f.NewGcFunction()))
	}

	// Restore registers previously in use
	restore(asm, regsInUse)
}

func genExpr(asm asmWriter, expr *Node, regsInUse int, takeAddr bool, fn *function) {

	// Post-fix, depth first search!
	if expr.right != nil {
		genExpr(asm, expr.right, regsInUse, false, fn)
		asm.ins(pushq, rax) // Push acc to stack

		// ALIGNMENT: Ensure alignment is correct before evaluating left (as it may contain fn calls)
		asm.ins(subq, intOp(8), rsp)
	}
	if expr.left != nil {
		genExpr(asm, expr.left, regsInUse, false, fn)

		// ALIGNMENT: Correct alignment now 'left' has been calculated
		if expr.right != nil {
			asm.ins(addq, intOp(8), rsp)
		}
	}

	// Implements stack machine

	switch expr.op {

	case opLit:
		switch expr.sym.Type.Kind {
		case String:
			asm.ins(movabs, stringOps[expr.sym.Name], rax)

		case Byte, Integer:
			// Parse
			i, err := strconv.ParseInt(expr.sym.Name, 10, 64)
			if err != nil {
				panic(err) // NOTE: Should never happen as has been checked on front end
			}
			// Check if we need a true 64-bit load
			ins := movq
			if i > math.MaxInt32 {
				ins = movabs
			}
			asm.ins(ins, strOp(expr.sym.Name), rax) // Push onto top of stack

		case Boolean:
			v := _false
			if expr.token.Val == "true" {
				v = _true
			}
			asm.ins(movq, v, rax) // Push onto top of stack

		default:
			panic(fmt.Sprintf("Unknown type for literal: %v", expr.sym.Type.Kind))
		}

	case opAdd:

		asm.ins(addq, rsp.deref(), rax)
		asm.ins(addq, intOp(8), rsp)

	case opSub:

		asm.ins(subq, rsp.deref(), rax)
		asm.ins(addq, intOp(8), rsp)

	case opMul:

		// Result is: rdx(high-64 bits):rax(low 64-bits) TODO: We ignore high bits!
		asm.ins(imulq, rsp.deref(), rax)
		asm.ins(addq, intOp(8), rsp)

	case opDiv:

		// Result is: rdx(remainder):rax(quotient) TODO: We ignore remainder!
		asm.ins(movq, _false, rdx)
		asm.ins(idivq, rsp.deref(), rax)
		asm.ins(addq, intOp(8), rsp)

	case opNot:

		asm.ins(notq, rax)
		asm.ins(andq, _true, rax)

	case opNeg:

		asm.ins(negq, rax)

	case opGt:

		asm.ins(cmpq, rsp.deref(), rax)
		asm.ins(setg, al)
		asm.ins(andq, _true, rax) // Clear top bits
		asm.ins(addq, intOp(8), rsp) // Pop

	case opLt:

		asm.ins(cmpq, rsp.deref(), rax)
		asm.ins(setl, al)
		asm.ins(andq, _true, rax) // Clear top bits
		asm.ins(addq, intOp(8), rsp) // Pop

	case opEq:

		asm.ins(cmpq, rsp.deref(), rax)
		asm.ins(sete, al)
		asm.ins(andq, _true, rax) // Clear top bits
		asm.ins(addq, intOp(8), rsp) // Pop

	case opAnd:

		asm.ins(andq, rsp.deref(), rax)
		asm.ins(addq, intOp(8), rsp) // Pop

	case opOr:

		asm.ins(orq, rsp.deref(), rax)
		asm.ins(addq, intOp(8), rsp) // Pop

	case opIdentifier:

		v := expr.sym
		switch {
		case v.IsStack: // Var operand
			inst := movq
			if takeAddr {
				inst = leaq
			}
			asm.ins(inst, rbp.displace(-v.Addr), rax)

		case v.Type.Is(Function) && v.IsGlobal: // Named function operand
			asm.ins(movabs, symOp(v.Type.AsFunction().AsmName(v.Name)), rax)

		default: // Struct field operand
			asm.ins(movq, intOp(v.Addr), rax)
		}

	case opFuncCall:

		genFnCall(asm, expr, fn, regsInUse)

	case opDot:

		inst := movq
		if takeAddr {
			inst = leaq
		}
		asm.ins(popq, rbx)
		asm.ins(inst, rax.offset(rbx), rax)

	case opArray:

		width := expr.typ.Width()

		asm.ins(popq, rbx) // stack(index) -> rbx

		// Bounds check
		// https://blogs.msdn.microsoft.com/clrcodegeneration/2009/08/13/array-bounds-check-elimination-in-the-clr/
		asm.ins(cmpq, rax.deref(), rbx) // index - array.length
		asm.ins(jae, labelOp("ioob"))   // Defined in runtime.c

		// Displace + 8 to skip over length
		if takeAddr {
			asm.ins(leaq, rax.offset(rbx).multiplier(width).displace(8), rax) // rax = [rax + rbx * width + 8]
		} else {

			// Read value according to width
			switch width {
			case 1:
				asm.ins(movsbq, rax.offset(rbx).multiplier(width).displace(8), rax) // rax = load[rax(*array) + (rbx(index) * width + 8)]
			case 8:
				asm.ins(movq, rax.offset(rbx).multiplier(width).displace(8), rax) // rax = load[rax(*array) + (rbx(index) * width + 8)]
			default:
				panic(fmt.Sprintf("Array access for element of width (%d) not yet implemented", width))
			}
		}

	default:
		panic(fmt.Sprintf("Can't generate expr code for op: %v", nodeTypes[expr.op]))
	}
}

func restore(asm asmWriter, regPos int) {
	if regPos > 0 {
		asm.raw("#----------------------------- Restore")
		for i := regPos; i > 0; i-- {
			asm.ins(popq, regs[i-1]) // Pop stack into reg
		}
		// ALIGNMENT: Restore $rsp after function call
		if regPos % 2 != 0 {
			asm.ins(addq, intOp(8), rsp)
		}
		asm.raw("#------------------------------------#")
	}
}

func spill(asm asmWriter, regPos int) {
	if regPos > 0 {
		asm.raw("#------------------------------- Spill")
		// ALIGNMENT: Ensure even $rsp before function call
		if regPos % 2 != 0 {
			asm.ins(subq, intOp(8), rsp)
		}
		for i := 0; i < regPos; i++ {
			asm.ins(pushq, regs[i]) // Push reg onto stack
		}
		asm.raw("#------------------------------------#")
	}
}
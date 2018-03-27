// +build linux

package main

import (
	"fmt"
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

var claralloc = "clara·claralloc.int" // NOTE: keep in sync with ASM name generation!

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

	asm.tab(".section", ".rodata")

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
		if n.op == opFuncDcl {
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
	return nil
}

func genFunc(asm asmWriter, n *Node) {

	fn := &function{ AstName: n.sym.Name, Type: n.sym.Type.AsFunction(), gcRoots: &GcState{}, gcFns: make(map[string]GcRoots) }

	// Ensure we only generate code for "our" functions
	if !fn.Type.IsExternal {

		// Assign stack offsets for temporaries
		temps := len(fn.Type.Args)
		walk(n, n.symtab, n, func(root *Node, symTab *SymTab, n *Node) error {
			// Look for symbols which should be on the stack but have no address
			if n.sym != nil && n.sym.IsStack && n.sym.Addr == 0 {
				n.sym.Addr = ptrSize * (temps + 1) // Assign a stack slot for temporary
				temps++
			}
			return nil
		})

		// Generate standard entry sequence
		genFnEntry(asm, fn.AsmName(), temps)

		// Copy register values into stack slots
		for i, param := range n.params {
			addr := ptrSize * (i + 1) // Assign a stack slot for var
			param.sym.Addr = addr
			param.sym.IsStack = true
			asm.op(movq, regs[i], rbp.displace(-addr))
			fn.gcRoots.Add(addr, param.typ)
		}

		// Generate functions
		if fn.Type.isConstructor {
			genConstructor(asm, fn, n.params)
		} else {
			// Generate code for all statements
			genStmtList(asm, n.stmts, fn)
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
		asm.tab(".section", ".rodata")
		debug := asm.stringLit(fmt.Sprintf("\"\\n%v\\n\"", fn.Type.Describe(fn.AstName)))
		asm.tab(".text")

		genFnEntry(asm, name, 1)
		asm.op(movq, rdi, rbp.displace(-ptrSize)) // Copy frame pointer into local slot

		genDebugGcPrintf(asm, debug)

		// Invoke GC function for type of each root
		for _, root := range roots {
			asm.op(movq, rbp.displace(-ptrSize), rax)    // load frame pointer
			asm.op(leaq, rax.displace(-root.off), rdi)   // load slot stack address
			asm.op(call, labelOp(root.typ.GcName()))
		}
		genFnExit(asm, false) // Called from Clara code -
		asm.spacer()
	}
}

func genTypeGcFuncs(asm asmWriter, types []*Type) {
	for _, typ := range types {
		if typ.IsPointer() {
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
	asm.tab(".section", ".rodata")
	debug := asm.stringLit(fmt.Sprintf("\"  - (0x%%lx) '%v' \\n\"", t))
	asm.tab(".text")

	genFnEntry(asm, t.GcName(), 1)
	asm.op(movq, rdi.deref(), rdi)            // Deref stack slot to get pointer into heap
	asm.op(movq, rdi, rbp.displace(-ptrSize)) // Copy into local slot

	// Calculate pointer to block from type
	asm.op(leaq, rdi.displace(-16), rsi) // *type-16 -> *block TODO: Is this safe if the pointer is NULL?
	genDebugGcPrintf(asm, debug)

	// Labels
	exit := asm.newLabel("exit")

	// Dereference pointer & load header into rax.
	asm.op(movq, rbp.displace(-ptrSize), rax)     // Copy stack slot
	asm.op(subq, intOp(gcHeaderSize), rax)        // *type-8 -> *header

	// if header & 1 == 1 (i.e. already marked)
	asm.op(movq, rax.deref(), rbx)
	asm.op(andq, intOp(1), rbx)
	asm.op(cmpq, _true, rbx)
	asm.op(je, labelOp(exit))

	// || header & 2 == 2 (i.e. is ready only)
	asm.op(movq, rax.deref(), rbx)
	asm.op(andq, intOp(2), rbx)
	asm.op(cmpq, intOp(2), rbx)
	asm.op(je, labelOp(exit))

	// header.marked = true
	asm.op(orq, intOp(1), rax.deref())

	switch t.Kind {
	case String, Array:
		// Nothing to do...
	case Struct:
		// Invoke GC for pointer fields
		for _, f := range t.AsStruct().Fields {
			if f.Type.IsPointer() {
				asm.op(movq, rbp.displace(-ptrSize), rdi)
				asm.op(addq, intOp(f.Addr), rdi)  // Increment pointer to offset of field
				asm.op(call, labelOp(f.Type.GcName()))
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
	asm.op(movq, strOp("debugGc"), rax) // Defined in runtime.c!
	asm.op(movq, rax.deref(), rax) // Get value
	asm.op(cmpq, _true, rax)
	asm.op(jne, labelOp(exit))
	asm.op(movq, template, rdi)
	asm.op(leaq, rdi.displace(8), rdi) // Skip past length!
	asm.op(movq, intOp(0), rax)
	asm.op(call, labelOp("printf"))
	asm.label(exit)

}

func genFramePointerAccess(asm asmWriter) {
	// Requires non-standard entry & exit!
	asm.function("getFramePointer")
	asm.op(movq, rbp, rax)
	asm.op(ret)
}

func genFnEntry(asm asmWriter, name string, temps int) {
	asm.function(name)
	asm.op(enter, intOp(temps*8), intOp(0))
}

func genFnExit(asm asmWriter, skipGc bool) {
	asm.op(leave)
	if skipGc {
		asm.op(ret)
		return
	}
	// Jump over frame GC function address
	// TODO: Is there any way to compress this?
	asm.op(popq, rbx)
	asm.op(addq, intOp(ptrSize), rbx)
	asm.op(jmp, rbx.indirect())
}

func genIoobHandler(asm asmWriter) {

	asm.label("ioob")
	asm.op(movq, rbx, rdi) // NOTE: When stack machine changes to single reg machine or linear scan this must change too!
	asm.op(call, labelOp("indexOutOfBounds"))
}

func genConstructor(asm asmWriter, f *function, params []*Node) {

	// Malloc memory of appropriate size
	asm.op(movq, intOp(f.Type.ret.AsStruct().Size()), rdi)
	asm.op(call, labelOp(claralloc)) // Implemented in lib/mem.clara
	asm.addr(f.NewGcFunction())

	// Copy stack values into fields
	off := 0
	for _, param := range params {
		id := param.sym
		// Can't move mem -> mem. Must go through a register.
		asm.op(movq, rbp.displace(-id.Addr), rbx)
		asm.op(movq, rbx, rax.displace(off))
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

	asm.op(popq, rax)          // Pop result from stack to rax
	asm.op(cmpq, _true, rax)   // Compare (true) to rax
	asm.op(jne, labelOp(exit)) // Jump over block if not true
	genStmtList(asm, n.stmts, fn) // Generate stmts block
	asm.op(jmp, labelOp(start))   // Jump to start of loop
	asm.label(exit)               // Declare exit point
}

func genAssignStmt(asm asmWriter, n *Node, fn *function) {

	// Evaluate expression & save result to rbx
	genExpr(asm, n.right, 0, false, fn)
	asm.op(popq, rcx) // TODO: Stack machine only uses rax & rbx. If changed revisit this!

	// Evaluate mem location to store
	slot := n.left
	genExpr(asm, slot, 0, true, fn)

	// Pop location to rax and move rbx there
	asm.op(popq, rax)              // stack -> rax

	// Check if int -> byte cast required
	if slot.typ.Is(Byte) && n.right.typ.Is(Integer) {
		asm.op(movsbq, rcx._8bit(), rcx) // rcx = cl (sign extend lowest 8-bits into same reg)
	}

	// SPECIAL CASE: If destination is byte array only move a single byte. Byte values elsewhere (on stack & in structs) are in 8-byte slots
	if slot.sym.Type.IsArray(Byte) && (n.right.typ.Is(Byte) || n.right.typ.Is(Integer)) {
		asm.op(movb, cl, rax.deref()) // [rax] = cl
	} else {
		asm.op(movq, rcx, rax.deref()) // [rax] = rcx
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

			asm.op(popq, rax)          // Pop result from stack to rax
			asm.op(cmpq, _true, rax)   // Compare (true) to rax
			asm.op(jne, labelOp(next)) // Jump over block if not equal

			genStmtList(asm, cur.stmts, fn) // Generate if (true) stmt block
			asm.op(jmp, labelOp(exit))      // Exit if/elseif/else block completely
			asm.label(next)                 // Label to jump if false
		} else {
			genStmtList(asm, cur.stmts, fn) // Generate block without condition (else block)
		}
		cur = cur.right // Move down tree
	}
	asm.label(exit) // Declare exit point
}

func genReturnStmt(asm asmWriter, retn *Node, fn *function) {

	// If return has expression evaluate it & pop result to rax
	if retn.left != nil {
		genExpr(asm, retn.left, 0, false, fn)
		asm.op(popq, rax)

		// Check if int -> byte cast required
		if retn.left.typ.Is(Integer) && fn.Type.ret.Is(Byte) {
			asm.op(movsbq, rax._8bit(), rax)
		}
	}

	// Clean stack & return
	genFnExit(asm, false)
}

func genFnCall(asm asmWriter, n *Node, f *function) {

	// Determine how function is referenced
	var s *Symbol
	var fn *FunctionType
	if n.sym != nil {
		s = n.sym
		fn = s.Type.AsFunction()
	} else {
		fn = n.left.typ.AsFunction() // Func is returned from subexpression
	}

	// Generate arg code
	for i, arg := range n.stmts {
		if i >= 6 {
			panic("Calling functions with more than 6 parameters not yet implemented")
		}
		genExpr(asm, arg, i, false, f) // Evaluate expression
		asm.op(popq, regs[i])                   // Pop result from stack into correct reg

		// SPECIAL CASE: Modify the pointer to a string or array to point "past" the length to the data. This
		// means the value can be directly supplied to other libc functions without modification.
		if (arg.typ.Is(String) || arg.typ.Is(Array)) && fn.IsExternal {
			asm.op(leaq, regs[i].displace(8), regs[i])
		}

		// Check if int -> byte cast required
		if i < len(fn.Args) && arg.typ.Is(Integer) && fn.Args[i].Is(Byte) {
			asm.op(movsbq, regs[i]._8bit(), regs[i])
		}
	}

	// Variadic functions must set rax to number of floating point parameters
	if fn.isVariadic {
		asm.op(movq, intOp(0), rax) // No floating-point register usage yet...
	}

	// Call function
	if s == nil {
		asm.op(call, rax.indirect()) // anonymous func call: register indirect
	} else if s.IsStack {
		asm.op(call, rbp.displace(-s.Addr).indirect()) // Parameter func call: memory indirect
	} else {
		asm.op(call, labelOp(fn.AsmName(s.Name))) // Named func call
	}

	// Only generate GC function addresses for Clara functions
	if !fn.IsExternal {
		asm.addr(f.NewGcFunction())
	}
}

func genExpr(asm asmWriter, expr *Node, regsInUse int, takeAddr bool, fn *function) {

	// Post-fix, depth first search!
	if expr.right != nil {
		genExpr(asm, expr.right, regsInUse, false, fn)
	}
	if expr.left != nil {
		genExpr(asm, expr.left, regsInUse, false, fn)
	}

	// Implements stack machine

	switch expr.op {

	case opLit:
		switch expr.sym.Type.Kind {
		case String:
			asm.op(pushq, stringOps[expr.sym.Name])

		case Byte, Integer:
			asm.op(pushq, strOp(expr.sym.Name)) // Push onto top of stack

		case Boolean:
			// TODO: Seems a bit hacky. Maybe a bool symbol with Val (1|0)?
			v := _false
			if expr.token.Val == "true" {
				v = _true
			}
			asm.op(pushq, v) // Push onto top of stack

		default:
			panic(fmt.Sprintf("Unknown type for literal: %v", expr.sym.Type.Kind))
		}

	case opAdd:

		asm.op(popq, rbx)      // Pop from stack to rbx
		asm.op(popq, rax)      // Pop from stack to rax
		asm.op(addq, rbx, rax) // rax = (rbx + rax)
		asm.op(pushq, rax)     // Push rax onto stack

	case opMin:

		asm.op(popq, rax)      // Pop from stack to rbx
		asm.op(popq, rbx)      // Pop from stack to rax
		asm.op(subq, rbx, rax) // rax = (rbx - rax)
		asm.op(pushq, rax)     // Push rax onto stack

	case opMul:
		asm.op(popq, rbx)       // Pop from stack to rbx
		asm.op(popq, rax)       // Pop from stack to rax
		asm.op(imulq, rbx, rax) // rdx(high-64 bits):rax(low 64-bits) = (rbx * rax) TODO: We ignore high bits!
		asm.op(pushq, rax)      // Push rax onto stack

	case opDiv:

		asm.op(popq, rax)         // Pop from stack to rbx
		asm.op(popq, rbx)         // Pop from stack to rax
		asm.op(movq, _false, rdx) // rdx = 0 (remainder). Prevents overflow from concatenation of rdx:rax
		asm.op(idivq, rbx, rax)   // rdx(remainder):rax(quotient) = (rbx / rax) TODO: We ignore remainder!
		asm.op(pushq, rax)        // Push rax onto stack

	case opNot:
		asm.op(popq, rax)        // Pop from stack to rax
		asm.op(notq, rax)        // rax = ~rax
		asm.op(andq, _true, rax) // rax = rax & 0x01
		asm.op(pushq, rax)       // Push result onto stack

	case opNeg:
		asm.op(popq, rax)             // Pop from stack to rax
		asm.op(negq, rax)             // rax = -rax
		asm.op(pushq, rax)            // Push result onto stack

	case opGt: // TODO: Other comparisons can get added here as they all share the same code!

		// NOTE: The order we pop from the stack here important
		// TODO: Consider changing opAdd to have the same order as addition is associative
		asm.op(popq, rax)         // Pop from stack to rax
		asm.op(popq, rbx)         // Pop from stack to rbx
		asm.op(cmpq, rbx, rax)    // Compare rax <-> rbx
		asm.op(movq, _true, rbx)  // Load true into rbx
		asm.op(movq, _false, rax) // Load false into rax
		asm.op(cmovg, rbx, rax)   // Conditionally move rbx (true) into rax (false) if previous comparison was greater than
		asm.op(pushq, rax)        // Push result onto stack

	case opLt:

		asm.op(popq, rax)         // Pop from stack to rax
		asm.op(popq, rbx)         // Pop from stack to rbx
		asm.op(cmpq, rbx, rax)    // Compare rax <-> rbx
		asm.op(movq, _true, rbx)  // Load true into rbx
		asm.op(movq, _false, rax) // Load false into rax
		asm.op(cmovl, rbx, rax)   // Conditionally move rbx (true) into rax (false) if previous comparison was less./ than
		asm.op(pushq, rax)        // Push result onto stack

	case opEq:

		asm.op(popq, rax)         // Pop from stack to rax
		asm.op(popq, rbx)         // Pop from stack to rbx
		asm.op(cmpq, rbx, rax)    // Compare rax <-> rbx
		asm.op(movq, _true, rbx)  // Load true into rbx
		asm.op(movq, _false, rax) // Load false into rax
		asm.op(cmove, rbx, rax)   // Conditionally move rbx (true) into rax (false) if previous comparison was equal
		asm.op(pushq, rax)        // Push result onto stack

	case opAnd:

		asm.op(popq, rax)      // Pop from stack to rax
		asm.op(popq, rbx)      // Pop from stack to rbx
		asm.op(andq, rbx, rax) // rax = rbx & rax
		asm.op(pushq, rax)     // Push result onto stack

	case opOr:

		asm.op(popq, rax)     // Pop from stack to rax
		asm.op(popq, rbx)     // Pop from stack to rbx
		asm.op(orq, rbx, rax) // rax = rbx | rax
		asm.op(pushq, rax)    // Push result onto stack

	case opIdentifier:

		v := expr.sym
		if v.IsStack {
			if takeAddr {
				asm.op(leaq, rbp.displace(-v.Addr), rax) // rax = [rbp - offset]
				asm.op(pushq, rax)                       // stack <- rax
			} else {
				asm.op(pushq, rbp.displace(-v.Addr)) // stack <- rbp - offset
			}
		} else {
			if takeAddr {
				// TODO: We don't have global variables yet which can be written to so this case is never executed!
				//asm.op(leaq, intOp(v.Addr).mem(), rax)   // rax = [addr]
				asm.op(pushq, rax) // stack <- rax
			} else {
				// Check for named function
				if v.Type.Is(Function) && v.IsGlobal {
					asm.op(pushq, strOp(v.Type.AsFunction().AsmName(v.Name))) // Push address of function
				} else {
					asm.op(pushq, intOp(v.Addr)) // stack <- addr
				}
			}
		}

	case opFuncCall:

		spill(asm, regsInUse) // Spill any in use registers to the stack
		genFnCall(asm, expr, fn)
		restore(asm, regsInUse) // Restore registers previously in use
		asm.op(pushq, rax)      // Push result (rax) onto stack

	case opDot:

		asm.op(popq, rax) // Pop from stack to rax
		asm.op(popq, rbx) // Pop from stack to rbx
		if takeAddr {
			asm.op(leaq, rax.offset(rbx), rax) // rax = [rax + rbx
		} else {
			asm.op(movq, rax.offset(rbx), rax) // rax = [rax + rbx]
		}
		asm.op(pushq, rax) // Push result (rax) onto stack

	case opArray:

		width := expr.typ.Width()

		asm.op(popq, rax)                  // stack(*array) -> rax
		asm.op(popq, rbx)                  // stack(index) -> rbx

		// Bounds check
		// https://blogs.msdn.microsoft.com/clrcodegeneration/2009/08/13/array-bounds-check-elimination-in-the-clr/
		asm.op(cmpq, rax.deref(), rbx) // index - array.length
		asm.op(jae, labelOp("ioob"))   // Defined in runtime.c

		// Displace + 8 to skip over length
		if takeAddr {
			asm.op(leaq, rax.offset(rbx).multiplier(width).displace(8), rax) // rax = [rax + rbx * width + 8]
		} else {

			// Read value according to width
			switch width {
			case 1:
				asm.op(movsbq, rax.offset(rbx).multiplier(width).displace(8), rax) // rax = load[rax(*array) + (rbx(index) * width + 8)]
			case 8:
				asm.op(movq, rax.offset(rbx).multiplier(width).displace(8), rax) // rax = load[rax(*array) + (rbx(index) * width + 8)]
			default:
				panic(fmt.Sprintf("Array access for element of width (%d) not yet implemented", width))
			}
		}

		asm.op(pushq, rax) // rax(int/byte/...) -> stack

	default:
		panic(fmt.Sprintf("Can't generate expr code for op: %v", nodeTypes[expr.op]))
	}
}

func restore(asm asmWriter, regPos int) {
	if regPos > 0 {
		asm.raw("#----------------------------- Restore")
		for i := regPos; i > 0; i-- {
			asm.op(popq, regs[i-1]) // Pop stack into reg
		}
		asm.raw("#------------------------------------#")
	}
}

func spill(asm asmWriter, regPos int) {
	if regPos > 0 {
		asm.raw("#------------------------------- Spill")
		for i := 0; i < regPos; i++ {
			asm.op(pushq, regs[i]) // Push reg onto stack
		}
		asm.raw("#------------------------------------#")
	}
}

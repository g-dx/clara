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

var na = "<debug>"
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
		if n.op == opBlockFnDcl || n.op == opExprFnDcl {
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
			asm.ins(movq, op(regs[i], rbp.displace(-addr)), na)
			fn.gcRoots.Add(addr, param.typ)
		}

		// Generate functions
		if fn.Type.isConstructor {
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
		asm.tab(".section", ".rodata")
		debug := asm.stringLit(fmt.Sprintf("\"\\n%v\\n\"", fn.Type.Describe(fn.AstName)))
		asm.tab(".text")

		genFnEntry(asm, name, 1)
		asm.ins(movq, op(rdi, rbp.displace(-ptrSize)), na) // Copy frame pointer into local slot

		genDebugGcPrintf(asm, debug)

		// Invoke GC function for type of each root
		for _, root := range roots {
			asm.ins(movq, op(rbp.displace(-ptrSize), rax), na)  // load frame pointer
			asm.ins(leaq, op(rax.displace(-root.off), rdi), na) // load slot stack address
			asm.ins(call, op(labelOp(root.typ.GcName())), na)
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
	asm.ins(movq, op(rdi.deref(), rdi), na)            // Deref stack slot to get pointer into heap
	asm.ins(movq, op(rdi, rbp.displace(-ptrSize)), na) // Copy into local slot

	// Calculate pointer to block from type
	asm.ins(leaq, op(rdi.displace(-16), rsi), na) // *type-16 -> *block TODO: Is this safe if the pointer is NULL?
	genDebugGcPrintf(asm, debug)

	// Labels
	exit := asm.newLabel("exit")

	// Dereference pointer & load header into rax.
	asm.ins(movq, op(rbp.displace(-ptrSize), rax), na) // Copy stack slot
	asm.ins(subq, op(intOp(gcHeaderSize), rax), na)    // *type-8 -> *header

	// if header & 1 == 1 (i.e. already marked)
	asm.ins(movq, op(rax.deref(), rbx), na)
	asm.ins(andq, op(intOp(1), rbx), na)
	asm.ins(cmpq, op(_true, rbx), na)
	asm.ins(je, op(labelOp(exit)), na)

	// || header & 2 == 2 (i.e. is ready only)
	asm.ins(movq, op(rax.deref(), rbx), na)
	asm.ins(andq, op(intOp(2), rbx), na)
	asm.ins(cmpq, op(intOp(2), rbx), na)
	asm.ins(je, op(labelOp(exit)), na)

	// header.marked = true
	asm.ins(orq, op(intOp(1), rax.deref()), na)

	switch t.Kind {
	case String, Array:
		// Nothing to do...
	case Struct:
		// Invoke GC for pointer fields
		for _, f := range t.AsStruct().Fields {
			if f.Type.IsPointer() {
				asm.ins(movq, op(rbp.displace(-ptrSize), rdi), na)
				asm.ins(addq, op(intOp(f.Addr), rdi), na) // Increment pointer to offset of field
				asm.ins(call, op(labelOp(f.Type.GcName())), na)
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
	asm.ins(movq, op(strOp("debugGc"), rax), na) // Defined in runtime.c!
	asm.ins(movq, op(rax.deref(), rax), na)      // Get value
	asm.ins(cmpq, op(_true, rax), na)
	asm.ins(jne, op(labelOp(exit)), na)
	asm.ins(movq, op(template, rdi), na)
	asm.ins(leaq, op(rdi.displace(8), rdi), na) // Skip past length!
	asm.ins(movq, op(intOp(0), rax), na)
	asm.ins(call, op(labelOp("printf")), na)
	asm.label(exit)

}

func genFramePointerAccess(asm asmWriter) {
	// Requires non-standard entry & exit!
	asm.function("getFramePointer")
	asm.ins(movq, op(rbp, rax), na)
	asm.ins(ret, op(), na)
}

func genFnEntry(asm asmWriter, name string, temps int) {
	asm.function(name)
	asm.ins(enter, op(intOp(temps*8), intOp(0)), na)
}

func genFnExit(asm asmWriter, skipGc bool) {
	asm.ins(leave, op(), na)
	if skipGc {
		asm.ins(ret, op(), na)
		return
	}
	// Jump over frame GC function address
	// TODO: Is there any way to compress this?
	asm.ins(popq, op(rbx), na)
	asm.ins(addq, op(intOp(ptrSize), rbx), na)
	asm.ins(jmp, op(rbx.indirect()), na)
}

func genIoobHandler(asm asmWriter) {

	// rbx is index register. See: codegen.go:647
	asm.label("ioob")
	asm.ins(movq, op(rbx, rdi), na) // NOTE: When stack machine changes to single reg machine or linear scan this must change too!
	asm.ins(call, op(labelOp("indexOutOfBounds")), na)
}

func genConstructor(asm asmWriter, f *function, params []*Node) {

	// Malloc memory of appropriate size
	asm.ins(movq, op(intOp(f.Type.ret.AsStruct().Size()), rdi), na)
	asm.ins(call, op(labelOp(claralloc)), na) // Implemented in lib/mem.clara
	asm.addr(f.NewGcFunction())

	// Copy stack values into fields
	off := 0
	for _, param := range params {
		id := param.sym
		// Can't move mem -> mem. Must go through a register.
		asm.ins(movq, op(rbp.displace(-id.Addr), rbx), na)
		asm.ins(movq, op(rbx, rax.displace(off)), na)
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

	asm.ins(cmpq, op(_true, rax), na)    // Pop result from stack to rax
	asm.ins(jne, op(labelOp(exit)), na)  // Jump over block if not true
	genStmtList(asm, n.stmts, fn)        // Generate stmts block
	asm.ins(jmp, op(labelOp(start)), na) // Jump to start of loop
	asm.label(exit)                      // Declare exit point
}

func genAssignStmt(asm asmWriter, n *Node, fn *function) {

	// Evaluate expression & save result to rcx
	genExpr(asm, n.right, 0, false, fn)
	asm.ins(movq, op(rax, rcx), na) // TODO: This is not safe because this register is used to pass parameters!

	// Evaluate mem location to store
	slot := n.left
	genExpr(asm, slot, 0, true, fn)

	// Check if int -> byte cast required
	if slot.typ.Is(Byte) && n.right.typ.Is(Integer) {
		asm.ins(movsbq, op(rcx._8bit(), rcx), na) // rcx = cl (sign extend lowest 8-bits into same reg), na)
	}

	// SPECIAL CASE: If destination is byte array only move a single byte. Byte values elsewhere (on stack & in structs) are in 8-byte slots
	if slot.sym.Type.IsArray(Byte) && (n.right.typ.Is(Byte) || n.right.typ.Is(Integer)) {
		asm.ins(movb, op(cl, rax.deref()), na) // [rax] = cl
	} else {
		asm.ins(movq, op(rcx, rax.deref()), na) // [rax] = rcx
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

			asm.ins(cmpq, op(_true, rax), na)   // Compare (true), na) to rax
			asm.ins(jne, op(labelOp(next)), na) // Jump over block if not equal

			genStmtList(asm, cur.stmts, fn)     // Generate if (true) stmt block
			asm.ins(jmp, op(labelOp(exit)), na) // Exit if/elseif/else block completely
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
		asm.ins(movsbq, op(rax._8bit(), rax), na)
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
		asm.ins(movq, op(rax, r15), na) // TODO: This is a callee saved register. If we use it, we should save it!
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
		if (arg.typ.Is(String) || arg.typ.Is(Array)) && fn.IsExternal {
			asm.ins(leaq, op(rax.displace(8), regs[i]), na)
		} else {
			asm.ins(movq, op(rax, regs[i]), na)
		}

		// Check if int -> byte cast required
		if i < len(fn.Args) && arg.typ.Is(Integer) && fn.Args[i].Is(Byte) {
			asm.ins(movsbq, op(regs[i]._8bit(), regs[i]), na)
		}
	}

	// Variadic functions must set rax to number of floating point parameters
	if fn.isVariadic {
		asm.ins(movq, op(intOp(0), rax), na) // No floating-point register usage yet...
	}

	// Call function
	if s == nil {
		asm.ins(call, op(r15.indirect()), na) // anonymous func call: register indirect
	} else if s.IsStack {
		asm.ins(call, op(rbp.displace(-s.Addr).indirect()), na) // Parameter func call: memory indirect
	} else {
		asm.ins(call, op(labelOp(fn.AsmName(s.Name))), na) // Named func call
	}

	// Only generate GC function addresses for Clara functions
	if !fn.IsExternal {
		asm.addr(f.NewGcFunction())
	}

	// Restore registers previously in use
	restore(asm, regsInUse)
}

func genExpr(asm asmWriter, expr *Node, regsInUse int, takeAddr bool, fn *function) {

	// Post-fix, depth first search!
	if expr.right != nil {
		genExpr(asm, expr.right, regsInUse, false, fn)
		asm.ins(pushq, op(rax), na) // Push acc to stack
	}
	if expr.left != nil {
		genExpr(asm, expr.left, regsInUse, false, fn)
	}

	// Implements stack machine

	switch expr.op {

	case opLit:
		switch expr.sym.Type.Kind {
		case String:
			asm.ins(movq, op(stringOps[expr.sym.Name], rax), na)

		case Byte, Integer:
			asm.ins(movq, op(strOp(expr.sym.Name), rax), na) // Push onto top of stack

		case Boolean:
			v := _false
			if expr.token.Val == "true" {
				v = _true
			}
			asm.ins(movq, op(v, rax), na) // Push onto top of stack

		default:
			panic(fmt.Sprintf("Unknown type for literal: %v", expr.sym.Type.Kind))
		}

	case opAdd:

		asm.ins(addq, op(rsp.deref(), rax), na)
		asm.ins(addq, op(intOp(8), rsp), na)

	case opSub:

		asm.ins(subq, op(rsp.deref(), rax), na)
		asm.ins(addq, op(intOp(8), rsp), na)

	case opMul:

		// Result is: rdx(high-64 bits):rax(low 64-bits) TODO: We ignore high bits!
		asm.ins(imulq, op(rsp.deref(), rax), na)
		asm.ins(addq, op(intOp(8), rsp), na)

	case opDiv:

		// Result is: rdx(remainder):rax(quotient) TODO: We ignore remainder!
		asm.ins(movq, op(_false, rdx), na)
		asm.ins(idivq, op(rsp.deref(), rax), na)
		asm.ins(addq, op(intOp(8), rsp), na)

	case opNot:

		asm.ins(notq, op(rax), na)
		asm.ins(andq, op(_true, rax), na)

	case opNeg:

		asm.ins(negq, op(rax), na)

	case opGt:

		asm.ins(cmpq, op(rsp.deref(), rax), na)
		asm.ins(setg, op(al), na)
		asm.ins(andq, op(_true, rax), na) // Clear top bits
		asm.ins(addq, op(intOp(8), rsp), na) // Pop

	case opLt:

		asm.ins(cmpq, op(rsp.deref(), rax), na)
		asm.ins(setl, op(al), na)
		asm.ins(andq, op(_true, rax), na) // Clear top bits
		asm.ins(addq, op(intOp(8), rsp), na) // Pop

	case opEq:

		asm.ins(cmpq, op(rsp.deref(), rax), na)
		asm.ins(sete, op(al), na)
		asm.ins(andq, op(_true, rax), na) // Clear top bits
		asm.ins(addq, op(intOp(8), rsp), na) // Pop

	case opAnd:

		asm.ins(andq, op(rsp.deref(), rax), na)
		asm.ins(addq, op(intOp(8), rsp), na) // Pop

	case opOr:

		asm.ins(orq, op(rsp.deref(), rax), na)
		asm.ins(addq, op(intOp(8), rsp), na) // Pop

	case opIdentifier:

		v := expr.sym
		switch {
		case v.IsStack: // Var operand
			inst := movq
			if takeAddr {
				inst = leaq
			}
			asm.ins(inst, op(rbp.displace(-v.Addr), rax), na)

		case v.Type.Is(Function) && v.IsGlobal: // Named function operand
			asm.ins(movq, op(strOp(v.Type.AsFunction().AsmName(v.Name)), rax), na)

		default: // Struct field operand
			asm.ins(movq, op(intOp(v.Addr), rax), na)
		}

	case opFuncCall:

		genFnCall(asm, expr, fn, regsInUse)

	case opDot:

		inst := movq
		if takeAddr {
			inst = leaq
		}
		asm.ins(popq, op(rbx), na)
		asm.ins(inst, op(rax.offset(rbx), rax), na)

	case opArray:

		width := expr.typ.Width()

		asm.ins(popq, op(rbx), na) // stack(index), na) -> rbx

		// Bounds check
		// https://blogs.msdn.microsoft.com/clrcodegeneration/2009/08/13/array-bounds-check-elimination-in-the-clr/
		asm.ins(cmpq, op(rax.deref(), rbx), na) // index - array.length
		asm.ins(jae, op(labelOp("ioob")), na)   // Defined in runtime.c

		// Displace + 8 to skip over length
		if takeAddr {
			asm.ins(leaq, op(rax.offset(rbx).multiplier(width).displace(8), rax), na) // rax = [rax + rbx * width + 8]
		} else {

			// Read value according to width
			switch width {
			case 1:
				asm.ins(movsbq, op(rax.offset(rbx).multiplier(width).displace(8), rax), na) // rax = load[rax(*array) + (rbx(index) * width + 8)]
			case 8:
				asm.ins(movq, op(rax.offset(rbx).multiplier(width).displace(8), rax), na) // rax = load[rax(*array) + (rbx(index) * width + 8)]
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
			asm.ins(popq, op(regs[i-1]), na) // Pop stack into reg
		}
		asm.raw("#------------------------------------#")
	}
}

func spill(asm asmWriter, regPos int) {
	if regPos > 0 {
		asm.raw("#------------------------------- Spill")
		for i := 0; i < regPos; i++ {
			asm.ins(pushq, op(regs[i]), na) // Push reg onto stack
		}
		asm.raw("#------------------------------------#")
	}
}

func op(ops ...operand) []operand {
	return ops
}
package main

import (
	"fmt"
	"math"
	"strconv"
)

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

var noGc = labelOp("_noGc")

var regs = []reg{rdi, rsi, rdx, rcx, r8, r9}

// Current function being compiled
type function struct {
	attrs   attributes
	AstName string
	Type    *FunctionType
	gcRoots *GcState
	gcMaps  map[string]GcRoots
	id      int
	sp      int
	reg     [][]*Type // Stack to track register types in use across calls
}

func (f *function) reset(n *Node) {
	f.attrs = n.attrs
	f.AstName = n.sym.Name
	f.Type = n.sym.Type.AsFunction()
	f.gcRoots = &GcState{}
	f.gcMaps = make(map[string]GcRoots)
	f.sp = 0
	f.reg = nil
}
func (f *function) incSp(i int) { f.sp += i }
func (f *function) decSp(i int) { f.sp -= i }
func (f *function) isSpAligned() bool { return f.sp % 2 == 0 }

func (f *function) AsmName() string {
	return f.Type.AsmName(f.AstName)
}

func (f *function) NewGcMap() operand {
	roots := f.gcRoots.Snapshot()
	if len(roots) == 0 {
		return noGc
	}
	name := fmt.Sprintf(".SM%v", f.id)
	f.gcMaps[name] = roots
	f.id += 1
	return labelOp(name)
}

func (f *function) SpillRegisters(asm asmWriter) {
	// Spill any in-use registers to stack
	if len(f.reg) > 0 {
		call := f.reg[len(f.reg)-1]
		for i := len(call)-1; i >=0; i-- {
			asm.ins(pushq, regs[i])
			f.incSp(1)
			f.gcRoots.Add(f.sp * ptrSize, call[i])
		}
	}
	// Track new register set
	f.reg = append(f.reg, []*Type(nil))
}

func (f *function) RestoreRegisters(asm asmWriter) {
	// Remove register tracking
	f.reg = f.reg[:len(f.reg)-1]

	// Restore previous register values from stack
	if len(f.reg) > 0 {
		call := f.reg[len(f.reg)-1]
		for i := 0; i < len(call); i++ {
			asm.ins(popq, regs[i])
			f.gcRoots.Remove(f.sp * ptrSize, call[i])
			f.decSp(1)
		}
	}
}

func (f *function) RegisterInUse(t *Type) {
	f.reg[(len(f.reg)-1)] = append(f.reg[(len(f.reg)-1)], t)
}

func codegen(symtab *SymTab, tree []*Node, asm asmWriter) error {

	// ---------------------------------------------------------------------------------------
	// Assembly Generation Start
	// ---------------------------------------------------------------------------------------

	// Runtime functions declared in Clara code
	ioob := symtab.MustResolve("indexOutOfBounds")
	alloc := symtab.MustResolve("claralloc")
	entrypoint := symtab.MustResolve("entrypoint")

	// Holds compilation state for current function
	fn := &function{}

	gt := &GcTypes{}
	gt.AddBuiltins(symtab)

	for _, n := range tree {
		if n.isFuncDcl() {
			fn.reset(n)
			genFunc(asm, n, fn, gt, alloc)
		}
	}

	genIoobTrampoline(asm, fnOp(ioob.Type.AsFunction().AsmName(ioob.Name)));
	asm.spacer()
	genFramePointerAccess(asm)
	asm.spacer()
	genUnsafe(asm)
	asm.spacer()
	genToTaggedInt(asm)
	asm.spacer()
	genAsmEntrypoint(asm, fnOp(entrypoint.Type.AsFunction().AsmName(entrypoint.Name)))
	asm.spacer()
	genNoGc(asm)
	asm.spacer()
	genInvokeDynamic(asm, noGc) // Closure invocation support
	asm.spacer()
	genTypeInfoTable(asm, gt)
	asm.spacer()
	asm.flush() // Write final values
	return nil
}

func genFunc(asm asmWriter, n *Node, fn *function, gt *GcTypes, alloc *Symbol) {

	// Ensure we only generate code for "our" functions
	if !fn.Type.IsExternal() {

		// Assign stack offsets for temporaries
		temps := len(fn.Type.Params)
		WalkPostOrder(n, func(n *Node) {
			// Look for symbols which should be on the stack but have no address
			if n.sym != nil && n.sym.IsStack && n.sym.Addr == 0 {
				n.sym.Addr = ptrSize * (temps + 1) // Assign a stack slot for temporary
				temps++
			}
		})

		// Generate standard entry sequence
		fn.incSp(genFnEntry(asm, fn.AsmName(), temps))

		// Copy register values into stack slots
		for i, param := range n.params {
			addr := ptrSize * (i + 1) // Assign a stack slot for var
			param.sym.Addr = addr
			param.sym.IsStack = true
			asm.ins(movq, regs[i], rbp.displace(-addr))
			fn.gcRoots.Add(addr, param.typ)
		}

		// Generate functions
		switch fn.Type.Kind {
		case StructCons, EnumCons:
			genConstructor(asm, fn, n.params, n.token.Val, gt.AssignId(fn.Type.ret), alloc)
		case Normal, Closure:
			switch n.op {
			case opBlockFnDcl:
				genStmtList(asm, n.stmts, fn)
				if fn.Type.ret.Is(Nothing) && !n.IsReturnLastStmt() {
					genFnExit(asm, fn.attrs.isExternalReturn())
				}
			case opExprFnDcl:
				genReturnStmt(asm, n.stmts[0], fn)
			}
		case External:
			panic(fmt.Sprintf("Cannot generate code for external functions"))
		}

		// Generate function GC maps
		asm.spacer()
		asm.tab(".data")
		for name, roots := range fn.gcMaps {
			var off []int
			for _, root := range roots {
				off = append(off, root.off/ptrSize)
			}
			asm.gcMap(name, off)
		}
	}
}

func genTypeInfoTable(asm asmWriter, gt *GcTypes) {

	asm.raw(".data")
	var roots []labelOp // Collect root maps to output at next stage
	tag := 0
	var et *Type
	for _, t := range gt.types {
		switch t.Kind {
		case String, Array:
			roots = append(roots, noGc)
			tag = 0
		case Struct:

			// Gather field offsets
			var off []int
			for _, f := range t.AsStruct().Fields {
				if f.Type.IsPointer() {
					off = append(off, f.Addr/ptrSize)
				}
			}

			// Skip if no pointers
			if len(off) == 0 {
				roots = append(roots, noGc)
				continue
			}

			// Output GC map
			roots = append(roots, asm.gcMap("struct_" + t.AsmName(), off))
			tag = 0

		case Enum:
			// TODO: Clean this up!
			// HACK! HACk!
			if t != et {
				et = nil
				tag = 0
			}
			et = t
			cons := t.AsEnum().Members[tag]
			if cons.AsEnumCons().Tag != tag {
				panic("out of order!")
			}

			// Gather field offsets
			var off []int
			for i, tp := range cons.Params {
				if tp.IsPointer() {
					off = append(off, i + 1) // Skip tag!
				}
			}

			// Skip if no pointers
			if len(off) == 0 {
				roots = append(roots, noGc)
				tag += 1
				continue
			}

			// Output GC map
			roots = append(roots, asm.gcMap(fmt.Sprintf("enum_%v_tag_%v", t.AsmName(),  tag), off))
			tag += 1

		default:
			panic(fmt.Sprintf("Unexpected type for GC: %v (%v)\n", t.AsmName(), typeKindNames[t.Kind]))
		}
	}

	// Generate typeInfo enum values
	var infos []operand
	for i, t := range gt.types {
		infos = append(infos, asm.roSymbol("typeInfo_"+strconv.Itoa(i), func(w asmWriter) {
			// TODO: Hack! Find a better way of returning a label to a string literal
			s := []byte(w.stringLit(fmt.Sprintf("\"%v\"", t)).Print())

			// NOTE: The IDs used here must match the enum definition in gc.clara!
			switch t.Kind {
			case Struct, Enum:
				w.taggedInt(0)
				w.addr(labelOp(s[1:])) // TODO: Clean this up!
				w.addr(roots[i])
			case String:
				w.taggedInt(1)
			case Array:
				w.taggedInt(2)
				w.addr(labelOp(s[1:])) // TODO: Clean this up!
				elemIsPointer := "0x0"
				if t.AsArray().Elem.IsPointer() {
					elemIsPointer = "0x1"
				}
				w.tab(".8byte", elemIsPointer)
			}
		}))
	}

	// Generate []typeInfo
	typeInfoArray := asm.roSymbol("typeInfoArray", func(w asmWriter) {
		w.taggedInt(len(infos))
		for _, i := range infos {
			w.addr(i)
		}
	})

	// typeInfoTable()
	asm.spacer()
	genFnEntry(asm, "typeInfoTable", 0)
	asm.ins(movabs, typeInfoArray, rax)
	genFnExit(asm, true) // NOTE: Defined in Clara code as external function so no GC
}

func genInvokeDynamic(asm asmWriter, noGc operand) {
	genFnEntry(asm, fnPrefix + "invokeDynamic.pointer.pointer.pointer.pointer.pointer.pointer", 1)
	callLabel := asm.newLabel("call")
	fnPtrLabel := asm.newLabel("fnPtr")

	// Check for read-only (header & 2 == 2)
	asm.ins(movq, rdi.displace(-ptrSize), rax)
	asm.ins(andq, taggedIntOp(2), rax)
	asm.ins(cmpq, taggedIntOp(2), rax)
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
	asm.addr(noGc)
	genFnExit(asm, false) // Called from Clara code
}

func genFramePointerAccess(asm asmWriter) {
	// Requires non-standard entry & exit!
	asm.fnStart("getFramePointer")
	asm.ins(movq, rbp, rax)
	asm.ins(ret)
	asm.fnEnd()
}

func genUnsafe(asm asmWriter) {
	genFnEntry(asm, "unsafe", 0) // NOTE: Lie! This function takes 3 parameters!
	asm.ins(leaq, rdi.index(rsi), rax)
	genFnExit(asm, true) // NOTE: Defined in Clara code as external function so no GC
}

func genToTaggedInt(asm asmWriter) {
	genFnEntry(asm, "toTaggedInt", 0) // NOTE: Lie! This function takes 1 parameters!
	// rdi + rdi + 1 = add int tag
	asm.ins(leaq, rdi.displace(1).index(rdi), rax)
	genFnExit(asm, true) // NOTE: Defined in Clara code as external function so no GC
}

func genAsmEntrypoint(asm asmWriter, entrypoint fnOp) {
	genFnEntry(asm, "clara_asm_entrypoint", 0)
	tagAs(asm, Integer, rdi) // Tag argc as int
	asm.ins(call, entrypoint)
	genFnExit(asm, true) // NOTE: Stubbed in Clara code & called from C main() so no GC
}

func genNoGc(asm asmWriter) {
	asm.tab(".data")
	asm.label("_noGc")
	asm.taggedInt(0)
	asm.tab(".text")
}

func genFnEntry(asm asmWriter, name string, temps int) int {
	// Ensure an even number of slack slots. This means $rsp is 16 byte
	// aligned as part of the function prologue. Wasting an extra 8-bytes
	// of space here means less $rsp manipulation in the generated code
	if temps % 2 != 0 {
		temps += 1
	}
	asm.fnStart(name)
	asm.ins(enter, intOp(temps*8), intOp(0))
	return temps
}

func genFnExit(asm asmWriter, skipGc bool) {
	asm.ins(leave)
	if skipGc {
		asm.ins(ret)
	} else {
		// Jump over frame GC function address
		// TODO: Is there any way to compress this?
		asm.ins(popq, rbx)
		asm.ins(addq, intOp(ptrSize), rbx)
		asm.ins(jmp, rbx.indirect())
	}
	asm.fnEnd()
}

func genIoobTrampoline(asm asmWriter, ioob operand) {

	// rbx is index register. See: codegen.go:647
	asm.tab(".text")
	asm.label("ioob")
	asm.ins(andq, intOp(-16), rsp) // Destructively align stack
	asm.ins(movq, rbx, rdi) // Load index, NOTE: Depends on current register usage!
	tagAs(asm, Integer, rdi) // Retag index
	asm.ins(movq, rax.deref(), rsi) // Load array length, NOTE: Depends on current register usage!
	asm.ins(call, ioob)
	// NOTE: Never returns so no need for GC word, return, etc
}

func genConstructor(asm asmWriter, f *function, params []*Node, name string, id int, alloc *Symbol) {

	size := ptrSize * len(params)
	if f.Type.IsEnumCons() {
		size += ptrSize // space for tag
	}

	// Malloc memory of appropriate size
	asm.ins(movq, taggedIntOp(size), rdi)
	asm.ins(movabs, asm.stringLit(fmt.Sprintf("\"%v\"", f.Type.Describe(name))), rsi)
	asm.ins(movabs, taggedIntOp(id), rdx)
	asm.ins(call, fnOp(alloc.Type.AsFunction().AsmName(alloc.Name))) // Implemented in lib/mem.clara
	asm.addr(f.NewGcMap())

	off := 0

	// Set tag (if required)
	if f.Type.IsEnumCons() {
		asm.ins(movq, taggedIntOp(f.Type.AsEnumCons().Tag), rax.displace(off))
		off += ptrSize
	}

	// Copy stack values into fields
	for _, param := range params {
		id := param.sym
		// Can't move mem -> mem. Must go through a register.
		asm.ins(movq, rbp.displace(-id.Addr), rbx)
		asm.ins(movq, rbx, rax.displace(off))
		off += ptrSize
	}

	// Pointer is already in rax so nothing to do...
	genFnExit(asm, false)
}

func genStmtList(asm asmWriter, stmts []*Node, fn *function) {
	fn.gcRoots.OpenScope()
	for _, stmt := range stmts {

		switch stmt.op {
		case opReturn:
			genReturnStmt(asm, stmt.left, fn)

		case opIf:
			genIfElseIfElseStmts(asm, stmt, fn)

		case opDas, opAs:
			genAssignStmt(asm, stmt, fn)

		case opWhile:
			genWhileStmt(asm, stmt, fn)

		case opBlock:
			genStmtList(asm, stmt.stmts, fn)

		default:
			genExpr(asm, stmt, false, fn)
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
	genExpr(asm, n.left, false, fn) // Left stores condition

	asm.ins(cmpq, _true, rax)    // Pop result from stack to rax
	asm.ins(jne, labelOp(exit))  // Jump over block if not true
	genStmtList(asm, n.stmts, fn)        // Generate stmts block
	asm.ins(jmp, labelOp(start)) // Jump to start of loop
	asm.label(exit)                      // Declare exit point
}

func genAssignStmt(asm asmWriter, n *Node, fn *function) {

	// Evaluate expression & push result to stack
	genExpr(asm, n.right, false, fn)
	save(asm, fn, rax)

	// Evaluate mem location to store
	slot := n.left
	genExpr(asm, slot, true, fn)

	// Pop result
	restore(asm, fn, rbx)
	src := rbx

	if slot.op == opArray && slot.typ.Is(Byte) && n.right.typ.IsAny(Byte, Integer) {
		// Move a single, untagged byte to array slot
		untag(asm, n.right, src)
		asm.ins(movb, src._8bit(), rax.deref()) // [rax] = src (8-bits);
	} else if slot.typ.Is(Byte) && n.right.typ.Is(Integer) {
		// Cast int -> byte
		int2Byte(asm, rax)
		asm.ins(movq, src, rax.deref()) // [rax] = src;
	} else {
		// Simply write slot
		asm.ins(movq, src, rax.deref()) // [rax] = src;
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
			genExpr(asm, cur.left, false, fn) // Left stores condition

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

func genReturnStmt(asm asmWriter, expr *Node, fn *function) {

	// If return has expression evaluate it
	if expr != nil {
		genExpr(asm, expr, false, fn)
		// Handle int/byte automatic conversion
		switch {
		case expr.typ.Is(Integer) && fn.Type.ret.Is(Byte):
			int2Byte(asm, rax)
		case expr.typ.Is(Byte) && fn.Type.ret.Is(Integer):
			byte2Int(asm, rax)
		}
	}

	// Clean stack & return
	genFnExit(asm, fn.attrs.isExternalReturn())
}

func genFnCall(asm asmWriter, n *Node, f *function) {

	// Determine how function is referenced
	var s *Symbol
	var fn *FunctionType
	if n.left.sym != nil {
		s = n.left.sym
		fn = s.Type.AsFunction()
	} else {
		genExpr(asm, n.left, false, f)
		save(asm, f, rax)
		fn = n.left.typ.AsFunction()
	}

	f.SpillRegisters(asm)

	// Generate arg code
	for i, arg := range n.stmts {
		// Evaluate expression, move into arg reg & record in use
		genExpr(asm, arg, false, f)
		asm.ins(movq, rax, regs[i])
		f.RegisterInUse(arg.typ)

		// TODO: Mark libc functions using directives & process parameter here
		if fn.IsExternal() {
			switch arg.typ.Kind {
			case String, Array:
				// Modify pointer to point past array length
				asm.ins(leaq, regs[i].displace(8), regs[i])
			case Byte, Integer:
				// Ensure valid "C" value
				untag(asm, arg, regs[i])
			}
		} else {
			// Handle int/byte automatic conversion
			switch {
			case arg.typ.Is(Integer) && fn.Params[i].Is(Byte):
				int2Byte(asm, regs[i])
			case arg.typ.Is(Byte) && fn.Params[i].Is(Integer):
				byte2Int(asm, regs[i])
			}
		}
	}

	// Variadic functions must set rax to number of floating point parameters
	if fn.isVariadic {
		asm.ins(movq, intOp(0), rax) // No floating-point register usage yet...
	}

	// Recover fn address from stack, if required
	if s == nil {
		restore(asm, f, rax)
	}

	// Align stack as per ABI
	if !f.isSpAligned() {
		asm.ins(subq, intOp(8), rsp)
	}

	// Call function
	if s == nil {
		asm.ins(call, rax.indirect()) // anonymous func call: register indirect
	} else if s.IsStack {
		asm.ins(call, rbp.displace(-s.Addr).indirect()) // Parameter func call: memory indirect
	} else {
		asm.ins(call, fnOp(fn.AsmName(s.Name))) // Named func call
	}

	// TODO: Mark libc functions using directives & process return value here

	// Only generate GC function addresses for Clara functions
	if !fn.IsExternal() {
		asm.addr(f.NewGcMap())
	}

	// Correct stack pointer (if required)
	if !f.isSpAligned() {
		asm.ins(addq, intOp(8), rsp)
	}

	f.RestoreRegisters(asm)
}

func genExpr(asm asmWriter, expr *Node, takeAddr bool, fn *function) {

	switch expr.op {

	case opLit:
		switch expr.sym.Type.Kind {
		case String:
			asm.ins(movabs, asm.stringLit(expr.sym.Name), rax)

		case Byte, Integer:
			// Parse
			i, err := strconv.ParseInt(expr.sym.Name, 0, 64)
			if err != nil {
				panic(err) // NOTE: Should never happen as has been checked on front end
			}
			// Check if we need a true 64-bit load
			ins := movq
			if i > math.MaxInt32 || i < math.MinInt32 {
				ins = movabs
			}
			asm.ins(ins, strOp(expr.sym.Name), rax) // Push onto top of stack
			tag(asm, expr, rax)

		case Boolean:
			v := _false
			if expr.sym.Name == "true" {
				v = _true
			}
			asm.ins(movq, v, rax) // Push onto top of stack

		default:
			panic(fmt.Sprintf("Unknown type for literal: %v", expr.sym.Type.Kind))
		}

	case opOr, opAnd:

		genExpr(asm, expr.left, false, fn)
		save(asm, fn, rax)
		genExpr(asm, expr.right, false, fn)
		asm.ins(movq, rax, rbx)
		restore(asm, fn, rax)
		asm.ins(ins[expr.op], rbx, rax)

	case opAdd, opSub, opMul, opDiv, opBOr, opBAnd, opBXor:

		genExpr(asm, expr.left, false, fn)
		untag(asm, expr.left, rax)
		save(asm, fn, rax)
		genExpr(asm, expr.right, false, fn)
		untag(asm, expr.right, rax)
		asm.ins(movq, rax, rbx)
		restore(asm, fn, rax)
		if expr.op == opDiv {
			asm.ins(cqo) // Sign-extend rax into rdx
		}
		asm.ins(ins[expr.op], rbx, rax)
		tag(asm, expr, rax)

		// NOTES:
		// For imul, result is: rdx(high-64 bits):rax(low 64-bits)
		// For idiv, result is: rdx(remainder):rax(quotient)

	case opNot, opBNot:

		genExpr(asm, expr.left, false, fn)
		asm.ins(notq, rax)
		switch expr.op {
		case opNot:
			asm.ins(andq, _true, rax)
		case opBNot:
			asm.ins(orq, intOp(tagFor(expr.left.typ.Kind)), rax) // SPECIAL CASE: Set tag again
		}

	case opNeg:

		genExpr(asm, expr.left, false, fn)
		untag(asm, expr.left, rax)
		asm.ins(negq, rax)
		tag(asm, expr.left, rax)

	case opGt, opGte, opLt, opLte, opEq:

		genExpr(asm, expr.left, false, fn)
		untag(asm, expr.left, rax)
		save(asm, fn, rax)
		genExpr(asm, expr.right, false, fn)
		untag(asm, expr.right, rax)
		asm.ins(movq, rax, rbx)
		restore(asm, fn, rax)
		asm.ins(cmpq, rbx, rax)
		asm.ins(ins[expr.op], al)
		asm.ins(andq, _true, rax) // Clear top bits

	case opBLeft, opBRight:

		genExpr(asm, expr.left, false, fn)
		untag(asm, expr.left, rax)
		save(asm, fn, rax)
		genExpr(asm, expr.right, false, fn)
		untag(asm, expr.right, rax)
		asm.ins(movq, rax, rcx)
		restore(asm, fn, rax)
		asm.ins(ins[expr.op], cl, rax)
		tag(asm, expr, rax)

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
			// HACK to workaround absolute addressing!
			// TODO: Figure out how to get a PIC relative address of an external function
			if v.Type.AsFunction().IsExternal() {
				asm.ins(movq, _false, rax)
			} else {
				asm.ins(movabs, symOp(v.Type.AsFunction().AsmName(v.Name)), rax)
			}

		default: // Struct field operand
			asm.ins(movq, intOp(v.Addr), rax)
		}

	case opTernary:

		genExpr(asm, expr.left, false, fn)

		elseLabel := asm.newLabel("else")
		exitLabel := asm.newLabel("exit")

		asm.ins(cmpq, _true, rax)
		asm.ins(jne, labelOp(elseLabel))
		genExpr(asm, expr.stmts[0], false, fn)
		asm.ins(jmp, labelOp(exitLabel))
		asm.label(elseLabel)
		genExpr(asm, expr.stmts[1], false, fn)
		asm.label(exitLabel)

	case opArrayLit:

		// Left has builder logic to create array & populate with elements
		genExpr(asm, expr.left, false, fn)

	case opFuncCall:
		genFnCall(asm, expr, fn)

	case opDot:

		genExpr(asm, expr.left, false, fn)
		save(asm, fn, rax)
		genExpr(asm, expr.right, false, fn)

		inst := movq
		if takeAddr {
			inst = leaq
		}
		asm.ins(movq, rax, rbx)
		restore(asm, fn, rax)
		asm.ins(inst, rax.index(rbx), rax)

	case opArray:

		width := expr.typ.Width()

		// Calculate array address
		genExpr(asm, expr.left, false, fn)
		save(asm, fn, rax)

		// Calculate index
		genExpr(asm, expr.right, false, fn)
		untag(asm, expr.right, rax) // Strip tag from index
		asm.ins(movq, rax, rbx)

		// Load array address (don't pop as we need it later)
		asm.ins(movq, rsp.deref(), rax)

		// Bounds check
		// https://blogs.msdn.microsoft.com/clrcodegeneration/2009/08/13/array-bounds-check-elimination-in-the-clr/
		asm.ins(movq, rax.deref(), rax)
		untagAs(asm, Integer, rax) // Strip tag from length
		asm.ins(cmpq, rax, rbx) // index - array.length

		// Restore array address before jump!
		restore(asm, fn, rax)
		asm.ins(jae, labelOp("ioob"))

		// Displace + 8 to skip over length
		if takeAddr {
			asm.ins(leaq, rax.index(rbx).scale(width).displace(8), rax) // rax = [rax + rbx * width + 8]
		} else {

			// Read value according to width
			switch width {
			case 1:
				asm.ins(movsbq, rax.index(rbx).scale(width).displace(8), rax) // rax = load[rax(*array) + (rbx(index) * width + 8)]
				tag(asm, expr, rax) // Bytes are stored without a tag
			case 8:
				asm.ins(movq, rax.index(rbx).scale(width).displace(8), rax) // rax = load[rax(*array) + (rbx(index) * width + 8)]
			default:
				panic(fmt.Sprintf("Array access for element of width (%d) not yet implemented", width))
			}
		}
	case opNamedType, opFuncType, opArrayType:
		// Nothing do to - yet!

	default:
		panic(fmt.Sprintf("Can't generate expr code for op: %v", nodeTypes[expr.op]))
	}
}

func tag(asm asmWriter, n *Node, r reg) {
	tagAs(asm, n.typ.Kind, r)
}

func tagAs(asm asmWriter, t TypeKind, r reg) {
	asm.ins(shlq, intOp(tagLenFor(t)), r)
	asm.ins(orq, intOp(tagFor(t)), r)
}

func untag(asm asmWriter, n *Node, r reg) {
	untagAs(asm, n.typ.Kind, r)
}

func untagAs(asm asmWriter, t TypeKind, r reg) {
	asm.ins(sarq, intOp(tagLenFor(t)), r)
}

func byte2Int(asm asmWriter, r reg) {
	asm.ins(orq, intOp(1), r)
}

func int2Byte(asm asmWriter, r reg) {
	// Strip tag, sign extend lowest 8-bits into same reg & add tag
	untagAs(asm, Integer, r)
	asm.ins(movsbq, r._8bit(), r)
	tagAs(asm, Byte, r)
}

func tagFor(tk TypeKind) int {
	switch tk {
	case Integer:
		return 0b1
	case Byte:
		return 0b0
	default:
		panic(fmt.Sprintf("TypeKind (%v) does not have a tag", typeKindNames[tk]))
	}
}

func tagLenFor(tk TypeKind) int {
	switch tk {
	case Integer:
		return 1
	case Byte:
		return 1
	default:
		panic(fmt.Sprintf("TypeKind (%v) does not have a tag", typeKindNames[tk]))
	}
}

func save(asm asmWriter, fn *function, r reg) {
	asm.ins(pushq, r)
	fn.incSp(1)
}

func restore(asm asmWriter, fn *function, r reg) {
	asm.ins(popq, r)
	fn.decSp(1)
}

var ins = make(map[int]inst)
func init() {
	ins[opAdd] = addq
	ins[opSub] = subq
	ins[opMul] = imulq
	ins[opDiv] = idivq
	ins[opOr]  = orq
	ins[opBOr] = orq
	ins[opAnd] = andq
	ins[opBAnd] = andq
	ins[opBXor] = xorq
	ins[opEq] = sete
	ins[opGt] = setg
	ins[opGte] = setge
	ins[opLt] = setl
	ins[opLte] = setle
	ins[opBLeft] = shlq
	ins[opBRight] = sarq
}

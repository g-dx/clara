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
	Type *FunctionType
}

func (f *function) AsmName() string {
	return f.Type.AsmName(f.AstName)
}

func codegen(symtab *SymTab, tree *Node, asm assembler) error {

	// ---------------------------------------------------------------------------------------
	// Assembly Generation Start
	// ---------------------------------------------------------------------------------------

	asm.tab(".section", ".rodata")

	// Output strings
	symtab.Walk(func(s *Symbol) {
		if s.Type.Is(String) && s.IsLiteral {
			stringOps[s.Name] = asm.stringLit(s.Name)
		}
	})

	asm.spacer()

	// Output func calls
	asm.tab(".text")

	tree.Walk(func(n *Node) {
		switch n.op {
		case opFuncDcl:
			fn := &function{ AstName: n.sym.Name, Type: n.sym.Type.AsFunction() }

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
				genFnEntry(asm, fn, temps)

				// Copy register values into stack slots
				for i, param := range n.params {
					addr := ptrSize * (i + 1) // Assign a stack slot for var
					param.sym.Addr = addr
					param.sym.IsStack = true
					asm.op(movq, regs[i], rbp.displace(-addr))
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
					genFnExit(asm)
				}
				asm.spacer()
			}
		default:
			//			fmt.Printf("Skipping: %v\n", nodeTypes[n.op])
		}
	})

	genIoobHandler(asm);
	asm.spacer()
	genFramePointerAccess(asm)
	return nil
}

func genFramePointerAccess(asm assembler) {
	// Requires non-standard entry & exit!
	asm.function("getFramePointer")
	asm.op(movq, rbp, rax)
	asm.op(ret)
}

func genFnEntry(asm assembler, f *function, temps int) {
	asm.function(f.AsmName())
	asm.op(enter, intOp(temps*8), intOp(0))
}

func genFnExit(asm assembler) {
	asm.op(leave)
	asm.op(ret)
}

func genIoobHandler(asm assembler) {

	asm.label("ioob")
	asm.op(movq, rbx, rdi) // NOTE: When stack machine changes to single reg machine or linear scan this must change too!
	asm.op(call, labelOp("indexOutOfBounds"))
}

func genConstructor(asm assembler, f *function, params []*Node) {

	// Malloc memory of appropriate size
	asm.op(movq, intOp(f.Type.ret.AsStruct().Size()), rdi)
	asm.op(call, labelOp(claralloc)) // Implemented in lib/mem.clara

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

func genStmtList(asm assembler, stmts []*Node, fn *function) {
	for _, stmt := range stmts {

		switch stmt.op {
		case opReturn:
			genReturnExpression(asm, stmt, fn)

		case opIf:
			genIfElseIfElseStmts(asm, stmt, fn)

		case opDas, opAs:
			genAssignStmt(asm, stmt, fn)

		case opWhile:
			genWhileStmt(asm, stmt, fn)

		default:
			genExprWithoutAssignment(asm, stmt, 0, false, fn)
		}
	}
}

func genWhileStmt(asm assembler, n *Node, fn *function) {

	// Create new labels
	exit := asm.newLabel("while_end")
	start := asm.newLabel("while_start")
	asm.label(start)

	// Generate condition
	genExprWithoutAssignment(asm, n.left, 0, false, fn) // Left stores condition

	asm.op(popq, rax)          // Pop result from stack to rax
	asm.op(cmpq, _true, rax)   // Compare (true) to rax
	asm.op(jne, labelOp(exit)) // Jump over block if not true
	genStmtList(asm, n.stmts, fn) // Generate stmts block
	asm.op(jmp, labelOp(start))   // Jump to start of loop
	asm.label(exit)               // Declare exit point
}

func genAssignStmt(asm assembler, n *Node, fn *function) {

	// Evaluate expression & save result to rbx
	genExprWithoutAssignment(asm, n.right, 0, false, fn)
	asm.op(popq, rcx) // TODO: Stack machine only uses rax & rbx. If changed revisit this!

	// Evaluate mem location to store
	genExprWithoutAssignment(asm, n.left, 0, true, fn)

	// Pop location to rax and move rbx there
	asm.op(popq, rax)              // stack -> rax

	// Check if int -> byte cast required
	if (n.left.typ.IsArray(Byte) || n.left.typ.Is(Byte)) && n.right.typ.Is(Integer) {
		asm.op(movsbq, rcx._8bit(), rcx) // rcx = cl (sign extend lowest 8-bits into same reg)
	}

	// SPECIAL CASE: Stack & struct byte values are stored in 8-byte slots but in byte arrays they occupy a single byte - hence only move a single byte
	if n.left.typ.IsArray(Byte) && (n.right.typ.Is(Byte) || n.right.typ.Is(Integer)) {
		asm.op(movb, cl, rax.deref()) // [rax] = cl
	} else {
		asm.op(movq, rcx, rax.deref()) // [rax] = rcx
	}
}

func genIfElseIfElseStmts(asm assembler, n *Node, fn *function) {

	// Generate exit label
	exit := asm.newLabel("if_end")

	cur := n
	for cur != nil {
		if cur.left != nil {
			// Generate condition
			genExprWithoutAssignment(asm, cur.left, 0, false, fn) // Left stores condition

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

func genReturnExpression(asm assembler, retn *Node, fn *function) {

	// If return has expression evaluate it & pop result to rax
	if retn.left != nil {
		genExprWithoutAssignment(asm, retn.left, 0, false, fn)
		asm.op(popq, rax)

		// Check if int -> byte cast required
		if retn.left.typ.Is(Integer) && fn.Type.ret.Is(Byte) {
			asm.op(movsbq, rax._8bit(), rax)
		}
	}

	// Clean stack & return
	genFnExit(asm)
}

func genFuncCall(asm assembler, n *Node, f *function) {

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
		genExprWithoutAssignment(asm, arg, i, false, f) // Evaluate expression
		asm.op(popq, regs[i])                                  // Pop result from stack into correct reg

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
}

func restore(asm assembler, regPos int) {
	if regPos > 0 {
		asm.raw("#----------------------------- Restore")
		for i := regPos; i > 0; i-- {
			asm.op(popq, regs[i-1]) // Pop stack into reg
		}
		asm.raw("#------------------------------------#")
	}
}

func spill(asm assembler, regPos int) {
	if regPos > 0 {
		asm.raw("#------------------------------- Spill")
		for i := 0; i < regPos; i++ {
			asm.op(pushq, regs[i]) // Push reg onto stack
		}
		asm.raw("#------------------------------------#")
	}
}

func genExprWithoutAssignment(asm assembler, expr *Node, regsInUse int, takeAddr bool, fn *function) {

	// Post-fix, depth first search!
	if expr.right != nil {
		genExprWithoutAssignment(asm, expr.right, regsInUse, false, fn)
	}
	if expr.left != nil {
		genExprWithoutAssignment(asm, expr.left, regsInUse, false, fn)
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
		genFuncCall(asm, expr, fn)
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

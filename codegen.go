// +build linux

package main

import (
	"fmt"
)

var stringOps = make(map[string]operand)
var regs = []reg{rdi, rsi, rdx, rcx, r8, r9} // SysV calling convention

func codegen(symtab *SymTab, tree *Node, asm assembler) error {

	// ---------------------------------------------------------------------------------------
	// Assembly Generation Start
	// ---------------------------------------------------------------------------------------

	asm.tab(".section", ".rodata")

	// Output strings
	symtab.Walk(func(s *Symbol) {
		if s.Type.Kind == String && s.IsLiteral {
			stringOps[s.Name] = asm.stringLit(s.Name)
		}
	})

	asm.spacer()

	// Output func calls
	asm.tab(".text")

	tree.Walk(func(n *Node) {
		switch n.op {
		case opFuncDcl:
			// Ensure we only generate code for "our" functions
			fn := n.sym.Type.AsFunction()
			if !fn.IsExternal {
				// FN declaration
				// TODO: Add "clara_" namespacing to all generated functions
				if fn.Name == "main" {
					fn.Name = "clara_main" // Rewrite main
				}

				// Assign stack offsets for temporaries
				temps := len(fn.Args)
				walk(n, n.symtab, n, func(root *Node, symTab *SymTab, n *Node) error {
					// Look for symbols which should be on the stack but have no address
					if n.sym != nil && n.sym.IsStack && n.sym.Addr == 0 {
						n.sym.Addr = 8 * (temps + 1) // Assign a stack slot for temporary
						temps++
					}
					return nil
				})

				// Allocate space for temporaries
				asm.function(fn.Name, temps)

				// Copy register values into stack slots
				for i, param := range n.params {
					addr := 8 * (i + 1) // Assign a stack slot for var
					param.sym.Addr = addr
					param.sym.IsStack = true
					asm.op(movq, regs[i], rbp.displace(-addr))
				}

				// Generate functions
				if fn.isConstructor {
					genConstructor(asm, fn, n.params)
				} else {
					// Generate code for all statements
					genStmtList(asm, n.stmts, n.symtab)
				}

				// TODO: If last statement was a ReturnExpression - no need for this epilogue...
				asm.op(leave)
				asm.op(ret)
				asm.spacer()
			}
		default:
			//			fmt.Printf("Skipping: %v\n", nodeTypes[n.op])
		}
	})

	return nil
}

func genConstructor(asm assembler, fn *FunctionType, params []*Node) {

	// Malloc memory of appropriate size
	asm.op(movq, intOp(fn.ret.Width()), rdi)
	asm.op(call, labelOp("malloc"))

	// Copy stack values into fields
	offset := 0
	for _, param := range params {
		id := param.sym
		// Can't move mem -> mem. Must go through a register.
		// TODO: These values are in registers (rdi, rsi, etc) so mov from there to mem
		asm.op(movq, rbp.displace(-id.Addr), rbx)
		asm.op(movq, rbx, rax.displace(offset))
		offset += id.Type.Width()
	}

	// Pointer is already in rax so nothing to do...
}

func genStmtList(asm assembler, stmts []*Node, tab *SymTab) {
	for _, stmt := range stmts {

		switch stmt.op {
		case opFuncCall:
			genFuncCall(asm, stmt.stmts, stmt.sym.Type.AsFunction(), stmt.symtab)

		case opReturn:
			genReturnExpression(asm, stmt, stmt.symtab)

		case opIf:
			genIfElseIfElseStmts(asm, stmt, stmt.symtab)

		case opDas, opAs:
			genAssignStmt(asm, stmt, stmt.symtab)

		default:
			genExprWithoutAssignment(asm, stmt, stmt.symtab, 0, false)
		}
	}
}

func genAssignStmt(asm assembler, n *Node, tab *SymTab) {

	// Evaluate expression & save result to rbx
	genExprWithoutAssignment(asm, n.right, tab, 0, false)
	asm.op(popq, rcx) // TODO: Stack machine only uses rax & rbx. If changed revisit this!

	// Evaluate mem location to store
	genExprWithoutAssignment(asm, n.left, tab, 0, true)

	// Pop location to rax and move rbx there
	asm.op(popq, rax)              // stack -> rax
	asm.op(movq, rcx, rax.deref()) // [rax] = rcx // TODO: When byteArrays added this should be movb!
}

func genIfElseIfElseStmts(asm assembler, n *Node, tab *SymTab) {

	// Generate exit label
	exit := asm.newLabel("if_end")

	cur := n
	for cur != nil {
		if cur.left != nil {
			// Generate condition
			genExprWithoutAssignment(asm, cur.left, tab, 0, false) // Left stores condition

			// Create new label
			next := asm.newLabel("else")

			asm.op(popq, rax)          // Pop result from stack to rax
			asm.op(cmpq, _true, rax)   // Compare (true) to rax
			asm.op(jne, labelOp(next)) // Jump over block if not equal

			genStmtList(asm, cur.stmts, cur.symtab) // Generate if (true) stmt block
			asm.op(jmp, labelOp(exit))              // Exit if/elseif/else block completely
			asm.label(next)                         // Label to jump if false
		} else {
			genStmtList(asm, cur.stmts, cur.symtab) // Generate block without condition (else block)
		}
		cur = cur.right // Move down tree
	}
	asm.label(exit) // Declare exit point
}

func genReturnExpression(asm assembler, retrn *Node, tab *SymTab) {

	// If return has expression evaluate it & pop result to rax
	if retrn.left != nil {
		genExprWithoutAssignment(asm, retrn.left, tab, 0, false)
		asm.op(popq, rax)
	}

	// Clean stack & return
	asm.op(leave)
	asm.op(ret)
}

func genFuncCall(asm assembler, args []*Node, fn *FunctionType, symtab *SymTab) {

	// Generate arg code
	for i, arg := range args {
		if i >= 6 {
			panic("Calling functions with more than 6 parameters not yet implemented")
		}
		genExprWithoutAssignment(asm, arg, symtab, i, false) // Evaluate expression
		asm.op(popq, regs[i])                                // Pop result from stack into correct reg

		// SPECIAL CASE: We need to know when we are calling libc printf with strings. This is
		// so we can modify the pointer value to point "past" the length to the actual data
		if arg.typ.Is(String) && fn.IsExternal && fn.Name == "printf" {
			asm.op(leaq, regs[i].displace(8), regs[i])
		}
	}

	// If function is variadic - must set rax to number of parameters as part if SysV x64 calling convention
	if fn.isVariadic {
		// TODO: If this is not set to zero we get a core dump for some reason?
		asm.op(movq, intOp(0), rax /*len(args) - fn.fnArgCount*/)
	}

	asm.op(call, labelOp(fn.Name))
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

func genExprWithoutAssignment(asm assembler, expr *Node, syms *SymTab, regsInUse int, takeAddr bool) {

	// Post-fix, depth first search!
	if expr.right != nil {
		genExprWithoutAssignment(asm, expr.right, syms, regsInUse, false)
	}
	if expr.left != nil {
		genExprWithoutAssignment(asm, expr.left, syms, regsInUse, false)
	}

	// Implements stack machine

	switch expr.op {

	case opLit:
		switch expr.sym.Type.Kind {
		case String:
			asm.op(pushq, stringOps[expr.sym.Name])

		case Integer:
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
				asm.op(pushq, intOp(v.Addr)) // stack <- addr
			}
		}

	case opFuncCall:

		spill(asm, regsInUse) // Spill any in use registers to the stack
		genFuncCall(asm, expr.stmts, expr.sym.Type.AsFunction(), syms)
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

		// TODO: Bounds check!

		asm.op(popq, rax)                  // stack(*array) -> rax
		asm.op(popq, rbx)                  // stack(index) -> rbx
		asm.op(leaq, rax.displace(8), rax) // rbx = rbx(*array) + 8

		// TODO: Should take element width into account here!
		if takeAddr {
			// TODO: The LEAQ from above can be folded into this instruction using displacement
			asm.op(leaq, rax.offset(rbx).multiplier(width), rax) // rax = [rax + rbx * width]
		} else {
			asm.op(movq, rax.offset(rbx).multiplier(width), rax) // rax = load[rax(*array) + (rbx(index) * width)]

			// Handle masking other bytes out
			switch width {
			case 1:
				// TODO: Should really be 'movb' above to not require masking!
				asm.op(andq, intOp(0xFF), rax) // rax = rax & FF
			case 8:
				// Nothing to do...
			default:
				panic(fmt.Sprintf("Array access for element of width (%d) not yet implemented", width))
			}
		}

		asm.op(pushq, rax) // rax(int/byte/...) -> stack

	default:
		panic(fmt.Sprintf("Can't generate expr code for op: %v", nodeTypes[expr.op]))
	}
}

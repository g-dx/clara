package main

import (
	"fmt"
	"io"
	"bufio"
)

var spacer = "\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
var strIndex = 0
var strLabels = make(map[string]string)
var regs = []string{"rdi", "rsi", "rdx", "rcx", "r8", "r9"} // SysV calling convention

func codegen(symtab *SymTab, tree *Node, writer io.Writer, debug bool) error {

	// Simplify writing...
	bufW := bufio.NewWriter(writer)
	write := func(s string, a...interface{}) {
		asm := fmt.Sprintf(s + "\n", a...)
		if debug {
			fmt.Print(asm)
		}
		_, err := bufW.WriteString(asm)
		if err != nil {
			panic(err)
		}
		bufW.Flush()
	}

	// ---------------------------------------------------------------------------------------
	// Assembly Generation Start
	// ---------------------------------------------------------------------------------------

	write("\t.section\t.rodata")

	// Output strings
	symtab.Walk(func(s Symbol) {
		if str, ok := s.(*StringLiteralSymbol); ok {
			strLabels[str.Val()] = genStringLit(write, str.Val())
		}
	})

	write(spacer)

	// Output func calls
	write("\t.text")

	tree.Walk(func(n *Node) {
		switch n.op {
		case opFuncDcl:
			if n.token.Val == "printf" {
				// Skip as this in done in glibc
			} else {
				// FN declaration
				if (n.token.Val == "main") {
					n.token.Val = "clara_main" // Rewrite main
				}

				write("\t.globl\t%v", n.token.Val)
				write("\t.type\t%v, @function", n.token.Val)
				write("%v:", n.token.Val)

				// Allocate space for parameters
				write("\tenter\t$(8 * %v), $0", len(n.args))

				// Copy register values into stack slots
				for i := 0; i < len(n.args); i++ {
					addr := 8 * (i + 1) // Assign a stack slot for var
					n.args[i].sym.(*VarSymbol).addr = addr
					write("\tmovq\t%%%v, -%v(%%rbp)", regs[i], addr)
				}


				// Walk statement and generate calls
				for _, stmt := range n.stats {

					switch stmt.op {
					case opFuncCall:
						genFuncCall(write, stmt.stats, stmt.sym.(*Function), stmt.symtab)
					case opReturn:
						if len(stmt.stats) == 1 {
							genReturnExpression(write, stmt.stats[0], stmt.symtab)
						} else {
							// TODO: This is a "naked" return
						}

					default:
						panic(fmt.Sprintf("Can't generate code for op: %v", stmt.op))
					}
				}

				// TODO: If last statement was a ReturnExpression - no need for this epilogue...
				write("\tleave")
				write("\tret")
				write(spacer)
			}
		default:
			//			fmt.Printf("Skipping: %v\n", nodeTypes[n.op])
		}
	})

	return nil
}
func genReturnExpression(write func(s string, a ...interface{}), expr *Node, tab *SymTab) {

	// Result is on top of stack
	genExprWithoutAssignment(write, expr, tab)

	// Pop from stack to eax
	write("\tpopq\t%%rax")

	// Clean stack & return
	write("\tleave")
	write("\tret")
}

func genFuncCall(write func(string,...interface{}), args []*Node, fn *Function, symtab *SymTab) {

	// Generate arg code
	genCallArgs(write, args, symtab)

	// If function is variadic - must set EAX to number of parameters as part if SysV x64 calling convention
	if fn.isVariadic {
		// TODO: If this is not set to zero we get a core dump for some reason?
		write("\tmovq\t$%v, %%rax", 0 /*len(args) - fn.fnArgCount*/)
	}

	write("\tcall\t%v", fn.fnName)
}

func genCallArgs(write func(string,...interface{}), args []*Node, symtab *SymTab) {
	i := 0
	for _, arg := range args {

		// TODO: Update to support pushing > 6 arguments on the stack
		if i >= 6 {
			panic("Calling functions with more than 6 parameters not yet implemented")
		}

		switch arg.op {
		case opStrLit:

			// Move into correct reg
			write("\tmovq\t$%v, %%%v", strLabels[arg.sym.(*StringLiteralSymbol).Val()], regs[i])

		case opIntAdd:

			spill(write, i-1)                            // Spill in-use registers to stack
			genExprWithoutAssignment(write, arg, symtab) // Generate expression
			write("\tpopq\t%%%v", regs[i])               // Move result from top of stack into correct reg
			restore(write, i-1)                          // Restore in-use registers from stack

		case opIntLit:

			// Move into correct reg
			write("\tmovq\t$%v, %%%v", arg.sym.(*IntegerLiteralSymbol).val, regs[i])

		case opIdentifier:

			// Look up var symbol which should have a stack slot assigned and move into correct reg
			v := arg.sym.(*VarSymbol)
			write("\tmovq\t-%v(%%rbp), %%%v", v.addr, regs[i])

		case opFuncCall:

			spill(write, i-1)                                           // Spill in-use registers to stack
			genFuncCall(write, arg.stats, arg.sym.(*Function), symtab)  // Generate code for call and then
			write("\tmovq\t%%rax, %%%v", regs[i])                       // Move result from rax into correct reg
			restore(write, i-1)                                         // Restore in-use registers from stack

		default:

			// Can't generate code for this yet
			panic(fmt.Sprintf("Can't generate code for call argument: %v", nodeTypes[arg.op]))
		}
		i += 1
	}
}

func restore(write func(string, ...interface{}), regPos int) {
	for i := regPos; i >= 0; i-- {
		write("\tpopq\t%%%v", regs[i]) // Pop stack into reg
	}
}

func spill(write func(string, ...interface{}), regPos int) {
	for i := regPos; i >= 0; i-- {
		write("\tpushq\t%%%v", regs[i]) // Push reg onto stack
	}
}

func genExprWithoutAssignment(write func(string, ...interface{}), expr *Node, syms *SymTab) {

	// Post-fix, depth first search!
	if expr.right != nil {
		genExprWithoutAssignment(write, expr.right, syms)
	}
	if expr.left != nil {
		genExprWithoutAssignment(write, expr.left, syms)
	}

	// Implements stack machine

	switch expr.op {
	case opIntLit:

		write("\tpushq\t$%v", expr.sym.(*IntegerLiteralSymbol).val) // Push onto top of stack

	case opIntAdd:

		write("\tpopq\t%%rbx")       // Pop from stack to ebx
		write("\tpopq\t%%rax")       // Pop from stack to eax
		write("\taddq\t%%rbx, %%rax") // eax = (ebx + eax)
		write("\tpushq\t%%rax")      // Push eax onto stack

	case opIdentifier:

		write("\tpushq\t-%v(%%rbp)", expr.sym.(*VarSymbol).addr) // Push onto top of stack

	case opFuncCall:

		// TODO: No need for spilling here because we do not use rax and an accumlator for expression evaluation. If/when
		// we do we will need to spill rax out to stack before executing the function call as the return value is stored in rax
		fn := expr.sym.(*Function)
		genFuncCall(write, expr.stats, fn, syms)
		write("\tpushq\t%%rax")  // Push result (rax) onto stack

	default:
		panic(fmt.Sprintf("Can't generate expr code for op: %v", nodeTypes[expr.op]))
	}
}

func genStringLit(write func(string,...interface{}), s string) string {
	label := fmt.Sprintf(".LC%v", strIndex)
	write(label + ":")
	write("\t.string \"%v\"", s[1:len(s)-1])
	strIndex += 1
	return label
}
package main

import (
	"fmt"
	"io"
	"bufio"
)

var strIndex = 0
var strLabels = make(map[string]string)
var regs = []string{"rdi", "rsi", "rdx", "rcx", "r8", "r9"}

func codegen(symtab SymTab, tree *Node, writer io.Writer, debug bool) error {

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

	// Output func calls
	write("\t.text")

	tree.Walk(func(n *Node) {
		switch n.op {
		case opFuncDcl:
			if n.token.Val == "println" {
				// Skip as this in done in glibc
			} else {
				// FN declaration
				if (n.token.Val == "main") {
					n.token.Val = "clara_main" // Rewrite main
				}

				write("\t.globl\t%v", n.token.Val)
				write("\t.type\t%v, @function", n.token.Val)
				write("%v:", n.token.Val)
				write("\tenter\t$0, $0")

				// Walk statement and generate calls
				for _, stmt := range n.stats {

					switch x := stmt.sym.(type) {
					case *Function:
						genFuncCall(write, stmt.stats, x)
					default:
						panic(fmt.Sprintf("Can't generate code for op: %v", stmt.op))
					}
				}

				write("\tleave")
				write("\tret")
			}
		default:
			//			fmt.Printf("Skipping: %v\n", nodeTypes[n.op])
		}
	})

	return nil
}

func genFuncCall(write func(string,...interface{}), args []*Node, fn *Function) {

	// Generate arg code
	genCallArgs(write, args)

	// If function is variadic - must set EAX to number of parameters as part if SysV x64 calling convention
	if fn.isVariadic {
		write("\tmovq\t$%v, %%rax", len(args) - fn.fnArgCount)
	}

	// TODO: Remove this special case!!
	name := fn.fnName
	if name == "println" {
		name = "printf"
	}
	write("\tcall\t%v", name)
}

func genCallArgs(write func(string,...interface{}), args []*Node) {
	i := 0
	for _, arg := range args {
		switch arg.op {
		case opStrLit:

			// Move into correct reg
			write("\tmovq\t$%v, %%%v", strLabels[arg.sym.(*StringLiteralSymbol).Val()], regs[i])

		case opIntAdd:

			// Pop result off stack into correct reg
			genExprWithoutAssignment(write, arg)
			write("\tpopq\t%%%v", regs[i])

		case opIntLit:

			// Move into correct reg
			write("\tmovq\t$%v, %%%v", arg.sym.(*IntegerLiteralSymbol).val, regs[i])

		default:

			// Can't generate code for this yet
			panic(fmt.Sprintf("Can't generate code for call argument: %v", arg.op))
		}
		i += 1 // TODO: this will wrap round for methods with more than 6 args
	}
}

func genExprWithoutAssignment(write func(string, ...interface{}), expr *Node) {

	// Post-fix, depth first search!
	if expr.right != nil {
		genExprWithoutAssignment(write, expr.right)
	}
	if expr.left != nil {
		genExprWithoutAssignment(write, expr.left)
	}

	// Implement stack machine

	//print(fmt.Sprintf("Expr: Visiting Node(%v) - ", nodeTypes[expr.op]))
	switch expr.op {
	case opIntLit:
		v := expr.sym.(*IntegerLiteralSymbol).val
		//println(fmt.Sprintf("%v", v))
		write("\tpushq\t$%v", v) // Push onto top of stack

	case opIntAdd:
		//println("")

		write("\tpopq\t%%rbx")       // Pop from stack to ebx
		write("\tpopq\t%%rax")       // Pop from stack to eax
		write("\taddq\t%%rbx, %%rax") // eax = (ebx + eax)
		write("\tpushq\t%%rax")      // Push eax onto stack

	default:
		panic(fmt.Sprintf("Can't generate expr code for op: %v", expr.op))
	}
}

func genStringLit(write func(string,...interface{}), s string) string {
	label := fmt.Sprintf(".LC%v", strIndex)
	write(label + ":")
	write("\t.string \"%v\"", s[1:len(s)-1])
	strIndex += 1
	return label
}
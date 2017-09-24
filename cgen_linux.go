package main

import (
	"fmt"
	"io"
	"bufio"
)

var strIndex = 0

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
	strLabels := make(map[string]string)
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

					// Direct call to print
					if (stmt.token.Val == "println") {
						// Get string lit
						str, ok := stmt.stats[0].sym.(*StringLiteralSymbol)
						if !ok {
							panic(fmt.Sprintf("print function parameter not string literal! - %v", str))
						}
						genLibCPrintfOnlyStringLiterals(write, strLabels[str.Val()])
					} else {
						// User defined function
						write("\tcall\t%v", stmt.token.Val)
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

func genStringLit(write func(string,...interface{}), s string) string {
	label := fmt.Sprintf(".LC%v", strIndex)
	write(label + ":")
	write("\t.string \"%v\"", s[1:len(s)-1])
	strIndex += 1
	return label
}

func genLibCPrintfOnlyStringLiterals(write func(string,...interface{}), fmt string, args ...string) {
 	i := 0
	regs := []string{"esi", "edx", "ecx", "r8", "r9"}
	for _, label := range args {
	 	write("\t%v\t%%%v", label, regs[i])
	 	i += 1 // TODO: this will wrap round for methods with more than 6 args
	}
	write("\tmovl\t$%v, %%edi", fmt)
	write("\tmovl\t$%v, %%eax", len(args))
	write("\tcall\tprintf")
}
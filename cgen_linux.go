package main

import (
	"fmt"
	"io"
	"bufio"
	"strings"
)

var spacer = "\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
var strIndex = 0
var labelIndex = 0
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
				write("\tenter\t$(8 * %v), $0", len(n.params))

				// Copy register values into stack slots
				for i := 0; i < len(n.params); i++ {
					addr := 8 * (i + 1) // Assign a stack slot for var
					n.params[i].sym.(*VarSymbol).addr = addr
					write("\tmovq\t%%%v, -%v(%%rbp)", regs[i], addr)
				}

				// Generate code for all statments
				genStmtList(write, n.stmts, n.symtab)

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
func genStmtList(write func(s string, a ...interface{}), stmts []*Node, tab *SymTab) {
	for _, stmt := range stmts {

		switch stmt.op {
		case opFuncCall:
			genFuncCall(write, stmt.stmts, stmt.sym.(*Function), stmt.symtab)

		case opReturn:
			if len(stmt.stmts) == 1 {
				genReturnExpression(write, stmt.stmts[0], stmt.symtab)
			} else {
				// TODO: This is a "naked" return
			}

		case opIf:
			genIfStmt(write, stmt, stmt.symtab)

		default:
			panic(fmt.Sprintf("Can't generate code for op: %v", stmt.op))
		}
	}
}

func genIfStmt(write func(s string, a ...interface{}), n *Node, tab *SymTab) {

	// Generate condition
	genExprWithoutAssignment(write, n.left, tab, 0) // Left stores condition

	// Create new label
	label := fmt.Sprintf("if%v", labelIndex)
	labelIndex++

	write("\tpopq\t%%rax")       // Pop result from stack to rax
	write("\tcmpq\t$1, %%rax")   // Compare (true) to rax
	write("\tjne\t%v", label)    // Jump over block if not equal

	genStmtList(write, n.stmts, n.symtab) // Generate if (true) stmt block
	write("%v:", label)                // Label to jump if false
}

func genReturnExpression(write func(s string, a ...interface{}), expr *Node, tab *SymTab) {

	// Result is on top of stack
	genExprWithoutAssignment(write, expr, tab, 0)

	// Pop from stack to eax
	write("\tpopq\t%%rax")

	// Clean stack & return
	write("\tleave")
	write("\tret")
}

func genFuncCall(write func(string,...interface{}), args []*Node, fn *Function, symtab *SymTab) {

	// Generate arg code
	for i, arg := range args {
		if i >= 6 {
			panic("Calling functions with more than 6 parameters not yet implemented")
		}
		genExprWithoutAssignment(write, arg, symtab, i) // Evaluate expression
		write("\tpopq\t%%%v", regs[i])                  // Pop result from stack into correct reg
	}

	// If function is variadic - must set EAX to number of parameters as part if SysV x64 calling convention
	if fn.isVariadic {
		// TODO: If this is not set to zero we get a core dump for some reason?
		write("\tmovq\t$%v, %%rax", 0 /*len(args) - fn.fnArgCount*/)
	}

	write("\tcall\t%v", fn.fnName)
}

func restore(write func(string, ...interface{}), regPos int) {

	debug := fmt.Sprintf("DEBUG: Restore [%v]", strings.Join(regs[:regPos], ", "))
	for i := regPos; i > 0; i-- {
		write("\tpopq\t%%%v   # %v", regs[i-1], debug) // Pop stack into reg
	}
}

func spill(write func(string, ...interface{}), regPos int) {
	debug := fmt.Sprintf("DEBUG: Spill [%v]", strings.Join(regs[:regPos], ", "))
	for i := 0; i < regPos; i++ {
		write("\tpushq\t%%%v   # %v", regs[i], debug) // Push reg onto stack
	}
}

func genExprWithoutAssignment(write func(string, ...interface{}), expr *Node, syms *SymTab, regsInUse int) {

	// Post-fix, depth first search!
	if expr.right != nil {
		genExprWithoutAssignment(write, expr.right, syms, regsInUse)
	}
	if expr.left != nil {
		genExprWithoutAssignment(write, expr.left, syms, regsInUse)
	}

	// Implements stack machine

	switch expr.op {

	case opStrLit:

		write("\tpushq\t$%v", strLabels[expr.sym.(*StringLiteralSymbol).Val()])

	case opIntLit:

		write("\tpushq\t$%v", expr.sym.(*IntegerLiteralSymbol).val) // Push onto top of stack

	case opBoolLit:

		// TODO: Seems a bit hacky. Maybe a bool symbol with Val (1|0)?
		x := 0
		if expr.token.Val == "true" {
			x = 1
		}
		write("\tpushq\t$%v", x) // Push onto top of stack

	case opIntAdd:

		write("\tpopq\t%%rbx")       // Pop from stack to ebx
		write("\tpopq\t%%rax")       // Pop from stack to eax
		write("\taddq\t%%rbx, %%rax") // eax = (ebx + eax)
		write("\tpushq\t%%rax")      // Push eax onto stack

	case opGt: // TODO: Other comparisons can get added here as they all share the same code!

		// NOTE: The order we pop from the stack here important
		// TODO: Consider changing opIntAdd to have the same order as addition is associative
		write("\tpopq\t%%rax")          // Pop from stack to eax
		write("\tpopq\t%%rbx")          // Pop from stack to ebx
		write("\tcmpq\t%%rbx, %%rax")   // Compare rax <-> rbx
		write("\tmovq\t$1, %%rbx")      // Load true into rbx
		write("\tmovq\t$0, %%rax")      // Load false into rax
		write("\tcmovg\t%%rbx, %%rax")  // Conditionally move rbx (true) into rax (false) if previous comparison was greater than
		write("\tpushq\t%%rax")         // Push result onto stack

	case opIdentifier:

		write("\tpushq\t-%v(%%rbp)", expr.sym.(*VarSymbol).addr) // Push onto top of stack

	case opFuncCall:

		spill(write, regsInUse) // Spill any in use registers to the stack
		genFuncCall(write, expr.stmts, expr.sym.(*Function), syms)
		restore(write, regsInUse) // Restore registers previously in use
		write("\tpushq\t%%rax")   // Push result (rax) onto stack

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
package main

import (
	"fmt"
	"io"
	"bufio"
	"encoding/binary"
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
	symtab.Walk(func(s *Symbol) {
		if s.Type.Kind == String && s.IsLiteral {
			strLabels[s.Name] = genStringLit(write, s.Name)
		}
	})

	write(spacer)

	// Output func calls
	write("\t.text")

	tree.Walk(func(n *Node) {
		switch n.op {
		case opFuncDcl:
			// Ensure we only generate code for "our" functions
			fn := n.sym.Type.AsFunction()
			if !fn.IsExternal {
				// FN declaration
				// TODO: Add "clara_" namespacing to all generated functions
				if (n.token.Val == "main") {
					n.token.Val = "clara_main" // Rewrite main
				}

				write("\t.globl\t%v", n.token.Val)
				write("\t.type\t%v, @function", n.token.Val)
				write("%v:", n.token.Val)

				// Assign stack offsets for temporaries
				temps := len(n.params)
				walk(n, n.symtab, n, func(root *Node, symTab *SymTab, n *Node) error {
					// Look for symbols which should be on the stack but have no address
					if n.sym != nil && n.sym.IsStack && n.sym.Addr == 0 {
						n.sym.Addr = 8 * (temps + 1) // Assign a stack slot for temporary
						temps++
					}
					return nil
				})

				// Allocate space for temporaries
				write("\tenter\t$(8 * %v), $0", temps)

				// Copy register values into stack slots
				for i, param := range n.params {
					addr := 8 * (i + 1) // Assign a stack slot for var
					param.sym.Addr = addr
					param.sym.IsStack = true
					write("\tmovq\t%%%v, -%v(%%rbp)", regs[i], addr)
				}

				// Generate functions
				if fn.isConstructor {
					genConstructor(write, fn, n.params)
				} else {
					// Generate code for all statements
					genStmtList(write, n.stmts, n.symtab)
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

func genConstructor(write func(s string, a ...interface{}), fn *FunctionType, params []*Node) {

	// Malloc memory of appropriate size
	write("\tmovq\t$%v, %%rdi", fn.ret.Width())
	write("\tcall\tmalloc")

	// Copy stack values into fields
	offset := 0
	for _, param := range params {
		id := param.sym
		// Can't move mem -> mem. Must go through a register.
		// TODO: These values are in registers (rdi, rsi, etc) so mov from there to mem
		write("\tmovq\t-%v(%%rbp), %%rbx", id.Addr)
		write("\tmovq\t%%rbx, %v(%%rax)", offset)
		offset += id.Type.Width()
	}

	// Pointer is already in rax so nothing to do...
}

func genStmtList(write func(s string, a ...interface{}), stmts []*Node, tab *SymTab) {
	for _, stmt := range stmts {

		switch stmt.op {
		case opFuncCall:
			genFuncCall(write, stmt.stmts, stmt.sym.Type.AsFunction(), stmt.symtab)

		case opReturn:
			genReturnExpression(write, stmt, stmt.symtab)

		case opIf:
			genIfElseIfElseStmts(write, stmt, stmt.symtab)

		case opDas:
			genDeclAndAssign(write, stmt, stmt.symtab)

		default:
			genExprWithoutAssignment(write, stmt, stmt.symtab, 0)
		}
	}
}

func genDeclAndAssign(write func(s string, a ...interface{}), n *Node, tab *SymTab) {

	// Evaluate expression
	genExprWithoutAssignment(write, n.right, tab, 0)

	// Pop result to rax, calc addr of temp & mov rax there
	write("\tpopq\t%%rax")                               // stack -> rax
	write("\tleaq\t-%v(%%rbp), %%rbx", n.left.sym.Addr)  // rbx = [rbp - offset]
	write("\tmovq\t%%rax, (%%rbx)")                      // [rbx] = rax
}

func genIfElseIfElseStmts(write func(s string, a ...interface{}), n *Node, tab *SymTab) {

	// Generate exit label
	exit := label("if_end")

	cur := n
	for cur != nil {
		if cur.left != nil {
			// Generate condition
			genExprWithoutAssignment(write, cur.left, tab, 0) // Left stores condition

			// Create new label
			next := label("else")

			write("\tpopq\t%%rax")     // Pop result from stack to rax
			write("\tcmpq\t$1, %%rax") // Compare (true) to rax
			write("\tjne\t%v", next)   // Jump over block if not equal

			genStmtList(write, cur.stmts, cur.symtab) // Generate if (true) stmt block
			write("\tjmp\t%v", exit)               // Exit if/elseif/else block completely
			write("%v:", next)                     // Label to jump if false
		} else {
			genStmtList(write, cur.stmts, cur.symtab) // Generate block without condition (else block)
		}
		cur = cur.right // Move down tree
	}
	write("%v:", exit) // Declare exit point
}

func genReturnExpression(write func(s string, a ...interface{}), ret *Node, tab *SymTab) {

	// If return has expression evaluate it & pop result to rax
	if ret.left != nil {
		genExprWithoutAssignment(write, ret.left, tab, 0)
		write("\tpopq\t%%rax")
	}

	// Clean stack & return
	write("\tleave")
	write("\tret")
}

func genFuncCall(write func(string,...interface{}), args []*Node, fn *FunctionType, symtab *SymTab) {

	// Generate arg code
	for i, arg := range args {
		if i >= 6 {
			panic("Calling functions with more than 6 parameters not yet implemented")
		}
		genExprWithoutAssignment(write, arg, symtab, i) // Evaluate expression
		write("\tpopq\t%%%v", regs[i])                  // Pop result from stack into correct reg

		// SPECIAL CASE: We need to know when we are calling libc printf with strings. This is
		// so we can modify the pointer value to point "past" the length to the actual data
		if arg.typ.Is(String) && fn.IsExternal && fn.Name == "printf" {
			write("\tleaq\t%v(%%%v), %%%v", 8, regs[i], regs[i])
		}
	}

	// If function is variadic - must set rax to number of parameters as part if SysV x64 calling convention
	if fn.isVariadic {
		// TODO: If this is not set to zero we get a core dump for some reason?
		write("\tmovq\t$%v, %%rax", 0 /*len(args) - fn.fnArgCount*/)
	}

	write("\tcall\t%v", fn.Name)
}

func restore(write func(string, ...interface{}), regPos int) {
	if regPos > 0 {
		write("#----------------------------- Restore")
		for i := regPos; i > 0; i-- {
			write("\tpopq\t%%%v", regs[i-1]) // Pop stack into reg
		}
		write("#------------------------------------#")
	}
}

func spill(write func(string, ...interface{}), regPos int) {
	if regPos > 0 {
		write("#------------------------------- Spill")
		for i := 0; i < regPos; i++ {
			write("\tpushq\t%%%v", regs[i]) // Push reg onto stack
		}
		write("#------------------------------------#")
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

	case opLit:
		switch expr.sym.Type.Kind {
		case String:
			write("\tpushq\t$%v", strLabels[expr.sym.Name])

		case Integer:
			write("\tpushq\t$%v", expr.sym.Name) // Push onto top of stack

		case Boolean:
			// TODO: Seems a bit hacky. Maybe a bool symbol with Val (1|0)?
			x := 0
			if expr.token.Val == "true" {
				x = 1
			}
			write("\tpushq\t$%v", x) // Push onto top of stack

		default:
			panic(fmt.Sprintf("Unknown type for literal: %v", expr.sym.Type.Kind))
		}

	case opAdd:

		write("\tpopq\t%%rbx")       // Pop from stack to rbx
		write("\tpopq\t%%rax")       // Pop from stack to rax
		write("\taddq\t%%rbx, %%rax") // rax = (rbx + rax)
		write("\tpushq\t%%rax")      // Push rax onto stack

	case opMin:

		write("\tpopq\t%%rax")       // Pop from stack to rbx
		write("\tpopq\t%%rbx")       // Pop from stack to rax
		write("\tsubq\t%%rbx, %%rax") // rax = (rbx - rax)
		write("\tpushq\t%%rax")      // Push rax onto stack

	case opMul:

		write("\tpopq\t%%rbx")         // Pop from stack to rbx
		write("\tpopq\t%%rax")         // Pop from stack to rax
		write("\timulq\t%%rbx, %%rax") // rdx(high-64 bits):rax(low 64-bits) = (rbx * rax) TODO: We ignore high bits!
		write("\tpushq\t%%rax")        // Push rax onto stack

	case opDiv:

		write("\tpopq\t%%rax")         // Pop from stack to rbx
		write("\tpopq\t%%rbx")         // Pop from stack to rax
		write("\tmovq\t$0, %%rdx")     // rdx = 0 (remainder). Prevents overflow from concatenation of rdx:rax
		write("\tidivq\t%%rbx, %%rax") // rdx(remainder):rax(quotient) = (rbx / rax) TODO: We ignore remainder!
		write("\tpushq\t%%rax")        // Push rax onto stack

	case opNot:
		write("\tpopq\t%%rax")          // Pop from stack to rax
		write("\tnotq\t%%rax")          // rax = ~rax
		write("\tandq\t$1, %%rax")      // rax = rax & 0x01
		write("\tpushq\t%%rax")         // Push result onto stack

	case opGt: // TODO: Other comparisons can get added here as they all share the same code!

		// NOTE: The order we pop from the stack here important
		// TODO: Consider changing opAdd to have the same order as addition is associative
		write("\tpopq\t%%rax")          // Pop from stack to rax
		write("\tpopq\t%%rbx")          // Pop from stack to rbx
		write("\tcmpq\t%%rbx, %%rax")   // Compare rax <-> rbx
		write("\tmovq\t$1, %%rbx")      // Load true into rbx
		write("\tmovq\t$0, %%rax")      // Load false into rax
		write("\tcmovg\t%%rbx, %%rax")  // Conditionally move rbx (true) into rax (false) if previous comparison was greater than
		write("\tpushq\t%%rax")         // Push result onto stack

	case opLt:

		write("\tpopq\t%%rax")          // Pop from stack to rax
		write("\tpopq\t%%rbx")          // Pop from stack to rbx
		write("\tcmpq\t%%rbx, %%rax")   // Compare rax <-> rbx
		write("\tmovq\t$1, %%rbx")      // Load true into rbx
		write("\tmovq\t$0, %%rax")      // Load false into rax
		write("\tcmovl\t%%rbx, %%rax")  // Conditionally move rbx (true) into rax (false) if previous comparison was less./ than
		write("\tpushq\t%%rax")         // Push result onto stack

	case opEq:

		write("\tpopq\t%%rax")          // Pop from stack to rax
		write("\tpopq\t%%rbx")          // Pop from stack to rbx
		write("\tcmpq\t%%rbx, %%rax")   // Compare rax <-> rbx
		write("\tmovq\t$1, %%rbx")      // Load true into rbx
		write("\tmovq\t$0, %%rax")      // Load false into rax
		write("\tcmove\t%%rbx, %%rax")  // Conditionally move rbx (true) into rax (false) if previous comparison was equal
		write("\tpushq\t%%rax")         // Push result onto stack

	case opAnd:

		write("\tpopq\t%%rax")          // Pop from stack to rax
		write("\tpopq\t%%rbx")          // Pop from stack to rbx
		write("\tandq\t%%rbx, %%rax")   // rax = rbx & rax
		write("\tpushq\t%%rax")         // Push result onto stack

	case opOr:

		write("\tpopq\t%%rax")          // Pop from stack to rax
		write("\tpopq\t%%rbx")          // Pop from stack to rbx
		write("\torq\t%%rbx, %%rax")    // rax = rbx | rax
		write("\tpushq\t%%rax")         // Push result onto stack

	case opIdentifier:

		v := expr.sym
		if v.IsStack {
			write("\tpushq\t-%v(%%rbp)", v.Addr) // Push stack slot offset onto top of stack
		} else {
			write("\tpushq\t$%v", v.Addr) // Push struct field offset onto top of stack
		}

	case opFuncCall:

		spill(write, regsInUse) // Spill any in use registers to the stack
		genFuncCall(write, expr.stmts, expr.sym.Type.AsFunction(), syms)
		restore(write, regsInUse) // Restore registers previously in use
		write("\tpushq\t%%rax")   // Push result (rax) onto stack

	case opDot:

		write("\tpopq\t%%rax")                 // Pop from stack to rax
		write("\tpopq\t%%rbx")                 // Pop from stack to rbx
		write("\tmovq\t(%%rax,%%rbx), %%rax")  // rax = load[rax + rbx]
		write("\tpushq\t%%rax")                // Push result (rax) onto stack

	case opArray:

		width := expr.typ.Width()

		// TODO: Bounds check!

		write("\tpopq\t%%rax")                 // stack(index) -> rax
		write("\tpopq\t%%rbx")                 // stack(*array) -> rbx
		write("\tleaq\t8(%%rbx), %%rbx")       // rbx = rbx(*array) + 8

		// TODO: Should take element width into account here!
		write("\tmovq\t(%%rbx,%%rax), %%rax")  // rax = load[rax(index) + rbx(*array)]

		// TODO: This should dynamically create a mask for width < 8
		if width == 1 {
			write("\tandq\t$0xFF, %%rax")      // rax = rax & FF
		} else {
			panic(fmt.Sprintf("Array access for element of 1 < width < 8 not yet implemented"))
		}

		write("\tpushq\t%%rax")                // rax(int/byte/...) -> stack

	default:
		panic(fmt.Sprintf("Can't generate expr code for op: %v", nodeTypes[expr.op]))
	}
}

func genStringLit(write func(string,...interface{}), s string) string {
	label := fmt.Sprintf(".LC%v", strIndex)
	write(label + ":")

	// Encode length in little endian format
	b := make([]byte, 8)
	binary.LittleEndian.PutUint64(b, uint64(len(s)-2)) // Trim double quotes

	// Generate escaped Go string of raw hex values
	size := fmt.Sprintf("\\x%x\\x%x\\x%x\\x%x\\x%x\\x%x\\x%x\\x%x",
		b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7])

	write("\t.ascii \"%v\"\"%v\\x0\"", size, s[1:len(s)-1])
	strIndex += 1
	return label
}

func label(name string) string {
	label := fmt.Sprintf("%v_%v", name, labelIndex)
	labelIndex++
	return label
}
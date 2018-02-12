package main
import (
	"fmt"
	"github.com/g-dx/clarac/console"
	"github.com/g-dx/clarac/lex"
)

// AST

type Node struct {
	token  *lex.Token
	left   *Node
	right  *Node
	stmts  []*Node
	params []*Node // OpFuncDecl
	op     int
	sym    *Symbol
	typ    *Type   // Set after typeCheck()..
	symtab *SymTab // Enclosing scope
}

func (n *Node) Add(stmt *Node) {
	n.stmts = append(n.stmts, stmt)
}

func (n *Node) hasType() bool {
	return n.typ != nil
}

// n should be type checked before call!
func (n *Node) isAddressable() bool {
	switch n.op {
	case opArray: return true
	case opFuncCall: return n.typ.Is(Struct) || n.typ.Is(Array)
	case opIdentifier: return true
	case opDot: return true
	default:
		return false
	}
}

func (n *Node) isTerminating() bool {

	switch n.op {
	case opReturn:
		return true

	case opIf:

		// Walk all right nodes to gather if/elseif/else structure
		x := []*Node { n }
		for i := 0; i < len(x); i++ {
			if x[i].right != nil {
				x = append(x, x[i].right)
			}
		}
		// Check last element is else
		if x[len(x)-1].op != opElse {
			return false
		}
		// Check each block terminates
		for _, n := range x {
			if len(n.stmts) == 0 {
				return false
			}
			if !n.stmts[len(n.stmts)-1].isTerminating() {
				return false
			}
		}
		return true

	case opFuncDcl:
		if len(n.stmts) == 0 {
			return false
		}
		return n.stmts[len(n.stmts)-1].isTerminating()

	default:
		return false
	}
}

func (n *Node) IsReturnLastStmt() bool {

	x := []*Node { n }
	for i := 0; i < len(x); i++ {
		switch x[i].op {
		case opFuncDcl, opElse, opElseIf, opIf:
			if len(x[i].stmts) > 0 {
				x = append(x, x[i].stmts[len(x[i].stmts)-1])
			}
		case opReturn:
			return true
		}
	}
	return false
}

const (
	opFuncDcl = iota
	opFuncCall
	opLit
	opAdd
	opMin
	opMul
	opDiv
	opEq
	opNot
	opDot
	opAnd
	opIdentifier
	opArray
	opReturn
	opIf
	opWhile
	opDas
	opAs
	opElseIf
	opElse
	opGt
	opLt
	opOr
	opError
	opRoot
	opStruct
)

var nodeTypes = map[int]string{
	opFuncDcl:    "Func Decl",
	opFuncCall:   "Func Call",
	opLit:        "Literal",
	opAdd:        "Binary Op [Add]",
	opMin:        "Binary Op [Min]",
	opMul:        "Binary Op [Mul]",
	opDiv:        "Binary Op [Div]",
	opIdentifier: "Identifier",
	opArray:      "Array Access",
	opReturn:     "Return Expr",
	opIf:         "If Stmt",
	opDas:        "Decl & Assign Stmt",
	opAs:         "Assign Stmt",
	opElseIf:     "ElseIf Stmt",
	opElse:       "Else Stmt",
	opGt:         "Comparison Op [>]",
	opLt:         "Comparison Op [<]",
	opNot:        "Negation [not]",
	opDot:        "Dot Select",
	opEq:         "Equality [eq]",
	opAnd:        "Logical [and]",
	opOr:         "Logical [or]",
	opError:      "(error)",
	opRoot:       "<none>",
	opStruct:     "Struct",
	opWhile:      "While",
}

func (n * Node) Walk(fn func(*Node)) {
	fn(n)
	for _, node := range n.stmts {
		node.Walk(fn)
	}
}

func printTree(n *Node) {
	fmt.Println("\nAbstract Syntax Tree:\n")
	printTreeImpl(n, "    ", true)
	fmt.Println()
}

func printTreeImpl(n *Node, prefix string, isTail bool) {
	// Handle current node
	row := "├── "
	if isTail {
		row = "└── "
	}

    if n == nil {
        return
    }
    // Has token?
    val := "ROOT"
    if n.token != nil {
        val = n.token.Val
    }

    // Print node
	fmt.Printf("%v%v%v%v%v%v%v ", console.Yellow, prefix, row, console.Disable, console.NodeTypeColour, val, console.Disable)
	if n.sym != nil {
		fmt.Printf(": %v%v%v(%v%v - %v%v)", console.Red, nodeTypes[n.op], console.Disable, console.Green, n.sym.Name, n.sym.Type, console.Disable)
	} else {
		fmt.Printf(": %v%v%v", console.Red, nodeTypes[n.op], console.Disable)
	}
	fmt.Println("")

	// Handle 0..n-1 children
	row = "│    "
	if isTail {
		row = "     "
	}

	// TODO: Print parameters better. Currently it looks like they are block statments
	// Print parameters
	printNodeListImpl(n.params, prefix+row)

	// Print statements & left/right
	printTreeImpl(n.left, prefix + row, false)
	printTreeImpl(n.right, prefix + row, true)
	printNodeListImpl(n.stmts, prefix+row)
}

func printNodeListImpl(nodes []*Node, prefix string) {

	if len(nodes) == 0 {
		return
	}

	for i := 0; i < len(nodes)-1; i++ {
		printTreeImpl(nodes[i], prefix, false)
	}

	// Handle n child
	printTreeImpl(nodes[len(nodes)-1], prefix, true)
}
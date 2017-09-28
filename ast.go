package main
import (
	"fmt"
	"github.com/g-dx/clarac/console"
	"github.com/g-dx/clarac/lex"
)

// AST

type Node struct {
	token  *lex.Token
	typ    *lex.Token // OpIdentifier (typed parameter), opFuncDecl (function return type)
	left   *Node
	right  *Node
	stmts  []*Node
	params []*Node // OpFuncDecl
	op     int
	sym    Symbol
	symtab *SymTab // Enclosing scope
}

func (n *Node) Add(stmt *Node) {
	n.stmts = append(n.stmts, stmt)
}

const (
	opFuncDcl = iota
	opFuncCall
	opStrLit
	opIntLit
	opIntAdd
	opIdentifier
	opReturn
	opError
	opRoot
)

var nodeTypes = map[int]string{
	opFuncDcl:    "Func Decl",
	opFuncCall:   "Func Call",
	opStrLit:     "String Lit",
	opIntLit:     "Integer Lit",
	opIntAdd:     "Binary Op [Add]",
	opIdentifier: "Identifier",
	opReturn:     "Return Expr",
	opError:      "(error)",
	opRoot:       "<none>",
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
		fmt.Printf(": %v%v%v(%v%v%v)", console.Red, nodeTypes[n.op], console.Disable, console.Green, n.sym.name(), console.Disable)
	} else {
		fmt.Printf(": %v%v%v", console.Red, nodeTypes[n.op], console.Disable)
	}
	fmt.Println("")

	// Handle 0..n-1 children
	row = "|    "
	if isTail {
		row = "     "
	}

	// Print parameters
	printNodeListImpl(n.params, prefix+row)

	// Expression or list of statements
	if len(n.stmts) == 0 {

		printTreeImpl(n.left, prefix + row, false)
		printTreeImpl(n.right, prefix + row, true)

	} else {
		printNodeListImpl(n.stmts, prefix+row)
	}
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
package main
import (
	"fmt"
	"github.com/g-dx/clarac/console"
	"github.com/g-dx/clarac/lex"
)

// AST

type Node struct {
	token *lex.Token
	left  *Node
	right *Node
	stats []*Node
	op    int
	sym Symbol
}

func (n *Node) Add(stat *Node) {
	n.stats = append(n.stats, stat)
}

const (
	opFuncDcl = iota
	opFuncCall
	opStrLit
	opIntegerLit
	opRoot
)

var nodeTypes = map[int]string {
	opFuncDcl : "Func Decl",
	opFuncCall : "Func Call",
	opStrLit : "String Lit",
	opIntegerLit : "Integer Lit",
	opRoot : "<none>",
}

func (n * Node) Walk(fn func(*Node)) {
	fn(n)
	for _, node := range n.stats {
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

	fmt.Printf("%v%v%v%v%v%v%v (%v%v%v)\n", console.Yellow, prefix, row, console.Disable,
		console.NodeTypeColour, val, console.Disable, console.Red, nodeTypes[n.op], console.Disable)

	// Handle 0..n-1 children
	row = "|    "
	if isTail {
		row = "     "
	}
	for i := 0; i < len(n.stats)-1; i++ {
		printTreeImpl(n.stats[i], prefix + row, false)
	}

	// Handle n child
	if len(n.stats) > 0 {
		printTreeImpl(n.stats[len(n.stats)-1], prefix + row, true)
	}
}
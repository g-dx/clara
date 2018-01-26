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

	x := []*Node { n }
	for i := 0; i < len(x); i++ {
		switch x[i].op {
		case opFuncDcl, opElse, opElseIf, opIf:
			for _, stmt := range x[i].stmts {
				x = append(x, stmt)
			}
		case opReturn:
			return true
		}
	}
	return false
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
}

func copyNode(n *Node) *Node {

	x := &Node{token: n.token, typ: n.typ, symtab: n.symtab, op: n.op, sym: n.sym}

	// Visit left and right
	if n.left != nil {
		x.left = copyNode(n.left)
	}
	if n.right != nil {
		x.right = copyNode(n.right)
	}

	// Visit parameters
	var params []*Node
	for _, param := range n.params {
		if param != nil {
			params = append(params, copyNode(param))
		}
	}
	x.params = params

	// Visit statements
	var stmts []*Node
	for _, stmt := range n.stmts {
		if stmt != nil {
			stmts = append(stmts, copyNode(stmt))
		}
	}
	x.stmts = stmts
	return x
}

func replaceNode(n *Node, replacer func(*Node) *Node) *Node {

	// Depth First Search

	// Visit left and right
	if n.left != nil {
		if x := replaceNode(n.left, replacer); x != nil {
			n.left = x
		}
	}
	if n.right != nil {
		if x := replaceNode(n.right, replacer); x != nil {
			n.right = x
		}
	}

	// Visit parameters
	for i := 0; i < len(n.params); i++ {
		if n.params[i] != nil {
			if x := replaceNode(n.params[i], replacer); x != nil {
				n.params[i] = x
			}
		}
	}

	// Visit statement
	for i := 0; i < len(n.stmts); i++ {
		if n.stmts[i] != nil {
			if x := replaceNode(n.stmts[i], replacer); x != nil {
				n.stmts[i] = x
			}
		}
	}

	// Visit node
	if x := replacer(n); x != nil {
		n.op = x.op
		n.token = x.token
		n.sym = x.sym
		n.stmts = x.stmts
		n.params = x.params
		n.right = x.right
		n.left = x.left
		n.typ = x.typ
		return n
	}
	return nil
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
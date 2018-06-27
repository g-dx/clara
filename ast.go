package main
import (
	"fmt"
	"github.com/g-dx/clarac/console"
	"github.com/g-dx/clarac/lex"
	"strings"
	"bytes"
	"io"
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

// n should be type checked before call!
func (n *Node) isReadOnly() bool {
	switch n.op {
	case opDot:
		// Currently only array lengths are readonly
		t := n.left.sym.Type
		return (t.Is(Array) || t.Is(String)) && n.right.sym.Name == "length"
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

	case opBlockFnDcl:
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
		case opBlockFnDcl, opElse, opElseIf, opIf:
			if len(x[i].stmts) > 0 {
				x = append(x, x[i].stmts[len(x[i].stmts)-1])
			}
		case opReturn:
			return true
		}
	}
	return false
}

func (n *Node) isLocalFn() bool {
	return (n.op == opBlockFnDcl || n.op == opExprFnDcl) && n.token.Val == "fn"
}

func (n *Node) isNonGlobalFnCall() bool {
	return n.op == opFuncCall && (n.sym == nil || !n.sym.IsGlobal)
}

func (n *Node) typeName() string {
	switch n.op {
	case opStructDcl:
		return n.token.Val
		
	case opBlockFnDcl, opExternFnDcl, opExprFnDcl:
		w := bytes.NewBufferString(n.token.Val)
		w.WriteRune(lex.LParen)
		var paramTypes []string
		for _, p := range n.params {
			paramTypes = append(paramTypes, p.left.typeName())
		}
		w.WriteString(strings.Join(paramTypes, ", "))
		w.WriteRune(lex.RParen)
		return w.String()
		
	case opArrayType:
		return fmt.Sprintf("[]%v", n.left.typeName())

	case opFuncType:
		var typeParams []string
		for _, p := range n.stmts {
			typeParams = append(typeParams, p.typeName())
		}
		return fmt.Sprintf("fn(%v)", strings.Join(typeParams, ", "))

	case opNamedType:
		return n.token.Val

	default:
		panic(fmt.Sprintf("AST node [%v]does not represent a type!", nodeTypes[n.op]))
	}
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

const (
	opBlockFnDcl  = iota
	opExprFnDcl
	opExternFnDcl
	opFuncCall
	opLit
	opAdd
	opSub
	opMul
	opDiv
	opEq
	opNot
	opNeg
	opDot
	opAnd
	opIdentifier
	opNamedType
	opFuncType
	opArray
	opArrayType
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
	opStructDcl
)

var nodeTypes = map[int]string{
	opBlockFnDcl:  "Block Fn Decl",
	opExternFnDcl: "Extern Fn Decl",
	opExprFnDcl:   "Expr Fn Decl",
	opFuncCall:    "Func Call",
	opFuncType:    "Func Type",
	opArrayType:   "Array Type",
	opNamedType:   "Named Type",
	opLit:         "Literal",
	opAdd:         "Binary Op [Add]",
	opSub:         "Binary Op [Min]",
	opMul:         "Binary Op [Mul]",
	opDiv:         "Binary Op [Div]",
	opIdentifier:  "Identifier",
	opArray:       "Array Access",
	opReturn:      "Return Expr",
	opIf:          "If Stmt",
	opDas:         "Decl & Assign Stmt",
	opAs:          "Assign Stmt",
	opElseIf:      "ElseIf Stmt",
	opElse:        "Else Stmt",
	opGt:          "Comparison Op [>]",
	opLt:          "Comparison Op [<]",
	opNot:         "Bool Negation [not]",
	opNeg:         "Numeric Negation [not]",
	opDot:         "Dot Select",
	opEq:         "Equality [eq]",
	opAnd:        "Logical [and]",
	opOr:         "Logical [or]",
	opError:      "(error)",
	opRoot:       "<none>",
	opStructDcl:  "Struct",
	opWhile:      "While",
}

func (n * Node) Walk(fn func(*Node)) {
	fn(n)
	for _, node := range n.stmts {
		node.Walk(fn)
	}
}

func printTree(n *Node, out io.Writer) {
	fmt.Fprintln(out, "\nAbstract Syntax Tree:")
	printTreeImpl(n, "    ", true, out)
	fmt.Fprintln(out)
}

func printTreeImpl(n *Node, prefix string, isTail bool, out io.Writer) {
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
	fmt.Fprintf(out, "%v%v%v%v%v%v%v ", console.Yellow, prefix, row, console.Disable, console.NodeTypeColour, val, console.Disable)
	if n.sym != nil {
		fmt.Fprintf(out, ": %v%v%v(%v%v - %v%v)", console.Red, nodeTypes[n.op], console.Disable, console.Green, n.sym.Name, n.sym.Type, console.Disable)
	} else {
		fmt.Fprintf(out,": %v%v%v", console.Red, nodeTypes[n.op], console.Disable)
	}
	fmt.Fprintln(out, "")

	// Handle 0..n-1 children
	row = "│    "
	if isTail {
		row = "     "
	}

	// TODO: Print parameters better. Currently it looks like they are block statments
	// Print parameters
	printNodeListImpl(n.params, prefix+row, out)

	// Print statements & left/right
	printTreeImpl(n.left, prefix + row, false, out)
	printTreeImpl(n.right, prefix + row, true, out)
	printNodeListImpl(n.stmts, prefix+row, out)
}

func printNodeListImpl(nodes []*Node, prefix string, out io.Writer) {

	if len(nodes) == 0 {
		return
	}

	for i := 0; i < len(nodes)-1; i++ {
		printTreeImpl(nodes[i], prefix, false, out)
	}

	// Handle n child
	printTreeImpl(nodes[len(nodes)-1], prefix, true, out)
}
package main
import (
	"bytes"
	"fmt"
	"github.com/g-dx/clarac/console"
	"github.com/g-dx/clarac/lex"
	"io"
	"strings"
)

// AST

type Node struct {
	attrs  attributes
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

func (n *Node) Add(stmt *Node) *Node {
	n.stmts = append(n.stmts, stmt)
	return n
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
		t := n.left.typ
		return (t.Is(Array) || t.Is(String)) && n.right.sym.Name == "length"
	default:
		return false
	}
}

func (n *Node) isFuncDcl() bool {
	switch n.op {
	case opBlockFnDcl, opExprFnDcl, opExternFnDcl, opConsFnDcl:
		return true
	default:
		return false
	}
}

func (n *Node) isTerminating() bool {

	switch n.op {
	case opReturn:
		return true

	case opCase:
		if len(n.stmts) == 0 {
			return false
		}
		if !n.stmts[len(n.stmts)-1].isTerminating() {
			return false
		}
		return true

	case opMatch:
		if len(n.stmts) == 0 {
			return false
		}
		for _, n := range n.stmts {
			if !n.isTerminating() {
				return false
			}
		}
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
	if len(n.stmts) == 0 {
		return false
	}
	if n.stmts[len(n.stmts)-1].op != opReturn {
		return false
	}
	return true
}

func (n *Node) isLocalFn() bool {
	return (n.op == opBlockFnDcl || n.op == opExprFnDcl) && n.token.Val == "fn"
}

func (n *Node) Is(ops ...int) bool {
	for _, op := range ops {
		if n.op == op {
			return true
		}
	}
	return false
}

func (n *Node) isNonGlobalFnCall() bool {
	return n.op == opFuncCall && (n.left.sym == nil || !n.left.sym.IsGlobal)
}

func (n *Node) isGenericFnCall() bool {
	if !n.Is(opFuncCall) {
		return false
	}
	if n.left.sym != nil {
		return len(n.left.sym.Type.AsFunction().Types) > 0
	}
	if n.left.typ != nil {
		return len(n.left.typ.AsFunction().Types) > 0
	}
	panic("Function call not annotated with function type!")
}

func (n *Node) typeName() string {
	switch n.op {
	case opStructDcl, opEnumDcl:
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

func (n *Node) Describe() string {
	switch n.op {
	case opFuncCall:
		var args []string
		for _, arg := range n.stmts {
			args = append(args, arg.typ.String())
		}
		return fmt.Sprintf("%v(%v)", n.token.Val, strings.Join(args, ", "))
	default:
		panic(fmt.Sprintf("Describe() is not implemented for type: %v", nodeTypes[n.op]))
	}
}

// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------

func ident(t *lex.Token, s *Symbol) *Node {
	return &Node{op: opIdentifier, token: t, sym: s, typ: s.Type}
}

func fnCallBySym(val *lex.Token, s *Symbol, args ... *Node) *Node {
	return &Node{op: opFuncCall, token: lex.Val("()"), left: ident(val, s), typ: s.Type.AsFunction().ret, stmts: args }
}

func generateStruct(root *Node, name string, fields ... *Symbol) (*Symbol, *Symbol) {

	var nodes []*Node
	var syms []*Symbol
	for i, f := range fields {

		// Create new symbol & associated AST
		sym := &Symbol{Name: f.Name, Addr: i * ptrSize, Type: f.Type}
		syms = append(syms, sym)
		nodes = append(nodes, ident(lex.Val(sym.Name), sym))
	}

	// Create struct declaration & symbol
	n := &Node{op: opStructDcl, token: &lex.Token{Val: name}, stmts: nodes}
	sym := &Symbol{Name: name, IsGlobal: true, Type: &Type{Kind: Struct, Data: &StructType{Name: name, Fields: syms}}}
	n.sym = sym

	// Add to root node
	root.Add(n)
	root.symtab.Define(sym)

	// Generate constructor
	consSym, err := generateStructConstructor(root, n)
	if err != nil {
		panic(fmt.Sprintf("error, failed to generate struct constructor: %v\n", err))
	}
	return sym, consSym
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
	opConsFnDcl
	opFuncCall
	opTypeList
	opBlock
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
	opBNot
	opBAnd
	opBOr
	opBXor
	opBLeft
	opBRight
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
	opEnumDcl
	opMatch
	opCase
)

var nodeTypes = map[int]string{
	opBlockFnDcl:  "Block Fn Decl",
	opExternFnDcl: "Extern Fn Decl",
	opExprFnDcl:   "Expr Fn Decl",
	opConsFnDcl:   "Cons Fn Decl",
	opFuncCall:    "Func Call",
	opFuncType:    "Func Type",
	opArrayType:   "Array Type",
	opNamedType:   "Named Type",
	opLit:         "Literal",
	opAdd:         "Binary Op [Add]",
	opSub:         "Binary Op [Min]",
	opMul:         "Binary Op [Mul]",
	opDiv:         "Binary Op [Div]",
	opBNot:        "Bitwise Op [~]",
	opBAnd:        "Bitwise Op [&]",
	opBOr:         "Bitwise Op [|]",
	opBXor:        "Bitwise Op [^]",
	opBLeft:       "Bitwise Op [<<]",
	opBRight:      "Bitwise Op [>>]",
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
	opEnumDcl:    "Enum",
	opMatch:      "Match",
	opCase:       "Case",
	opWhile:      "While",
	opBlock:      "Block", // Only generated by AST rewrites
	opTypeList:   "«TypeList»",
}

func printTree(n *Node, f func(*Node) bool, out io.Writer) {
	fmt.Fprintln(out, "\nAbstract Syntax Tree:")
	printTreeImpl(n, f, "    ", true, out)
	fmt.Fprintln(out)
}

func printTreeImpl(n *Node, f func(*Node) bool, prefix string, isTail bool, out io.Writer) {
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
	printNodeListImpl(n.params, f, prefix+row, out)

	// Print statements & left/right
	printTreeImpl(n.left, f, prefix + row, false, out)
	printTreeImpl(n.right, f, prefix + row, true, out)
	printNodeListImpl(n.stmts, f, prefix+row, out)
}

func printNodeListImpl(nodes []*Node, f func(*Node) bool, prefix string, out io.Writer) {

	if len(nodes) == 0 {
		return
	}

	alwaysMatch := func(n *Node) bool { return true }
	for i := 0; i < len(nodes)-1; i++ {
		if f(nodes[i]) {
			printTreeImpl(nodes[i], alwaysMatch, prefix, false, out)
		}
	}

	// Handle n child
	if f(nodes[len(nodes)-1]) {
		printTreeImpl(nodes[len(nodes)-1], alwaysMatch, prefix, true, out)
	}
}
package main
import (
	"errors"
	"fmt"
	"github.com/g-dx/clarac/lex"
	"strings"
	"strconv"
)

//
// Functions for various semantic passes
//

const (
	errRedeclaredMsg           = "%v:%d:%d: error, '%v' redeclared"
	errUnknownTypeMsg          = "%v:%d:%d: error, unknown type '%v'"
	errUnknownVarMsg           = "%v:%d:%d: error, no declaration for identifier '%v' found"
	errAmbiguousVarMsg         = "%v:%d:%d: error, multiple identifiers for '%v' found:\n\t* %v"
	errStructNamingLowerMsg    = "%v:%d:%d: error, struct names must start with a lowercase letter, '%v'"
	errConstructorOverrideMsg  = "%v:%d:%d: error, function name '%v' is reserved for struct constructor"
	errNotStructMsg            = "%v:%d:%d: error, '%v' is not a struct"
	errStructHasNoFieldMsg     = "%v:%d:%d: error, field '%v' is not defined in struct '%v'"
	errInvalidDotSelectionMsg  = "%v:%d:%d: error '%v', expected field or function call"
	errInvalidOperatorTypeMsg  = "%v:%d:%d: type '%v' invalid for operator '%v'"
	errMismatchedTypesMsg      = "%v:%d:%d: mismatched types, got '%v', wanted '%v'"
	errInvalidNumberArgsMsg    = "%v:%d:%d: invalid number of arguments, got '%v', wanted '%v'"
	errResolveFunctionMsg      = "%v:%d:%d: Cannot resolve function '%v'"
	errNonIntegerIndexMsg      = "%v:%d:%d: error, found type '%v', array index must be integer"
	errUnexpectedAssignMsg     = "%v:%d:%d: error, left hand side of assignment must be identifier"
	errNotAddressableAssignMsg = "%v:%d:%d: error, left hand side of assignment is not addressable"
	errNotWritableAssignMsg    = "%v:%d:%d: error, cannot assign value to readonly field '%v'"
	errMissingReturnMsg 	   = "%v:%d:%d: error, missing return for function '%v'"
	errIntegerOverflowMsg 	   = "%v:%d:%d: error, constant '%v' overflow integer type"

	// Debug messages
	debugTypeInfoFormat = "⚫ %s%-60s%s %s%-30s%s ⇨ %s%s%s\n"
)

//---------------------------------------------------------------------------------------------------------------

type OperatorTypes map[int][]TypeKind

var operatorTypes = OperatorTypes {
	opAdd: { Integer, Byte },
	opSub: { Integer, Byte },
	opMul: { Integer, Byte },
	opDiv: { Integer, Byte },
	opOr:  { Boolean },
	opAnd: { Boolean },
	// TODO: What about unary operators? Operators which return a different type?
}

func (ot OperatorTypes) isValid(op int, tk TypeKind) bool {
	tks := ot[op]
	if tks == nil {
		return false
	}
	for _, t := range tks {
		if t == tk {
			return true
		}
	}
	return false
}

func processTopLevelTypes(rootNode *Node, symtab *SymTab) (errs []error) {
	// Add types and check for redeclares
	for _, n := range rootNode.stmts {
		switch n.op {
		case opStructDcl:
			sym := &Symbol{Name: n.typeName(), IsGlobal: true, Type: &Type{Kind: Struct, Data: &StructType{Name: n.token.Val}}}
			n.sym = sym
			if _, found := symtab.Define(sym); found {
				errs = append(errs, semanticError(errRedeclaredMsg, n.token))
			}
		case opBlockFnDcl, opExprFnDcl, opExternFnDcl:
			// NOTE: This is simply a "marker" symbol used to check for redeclaration
			if _, found := symtab.Define(&Symbol{Name: n.typeName(), Type: &Type{Kind: Nothing}}); found {
				errs = append(errs, semanticError(errRedeclaredMsg, n.token))
			}
		}
	}
	if len(errs) > 0 {
		return errs
	}

	// Process structs & funcs
loop:
	for _, n := range rootNode.stmts {
		switch n.op {
		case opStructDcl:
			s, _ := symtab.Resolve(n.token.Val)
			strt := s.Type.AsStruct()

			// Calculate field information
			x := 0
			child := symtab.Child()
			for _, n := range n.stmts {

				// Look up type
				fieldType, err := createType(child, n.left)
				if err != nil {
					errs = append(errs, err)
					continue loop
				}
				s := &Symbol{Name: n.token.Val, Addr: x * ptrSize, Type: fieldType}

				// Define field
				if _, found := child.Define(s); found {
					errs = append(errs, semanticError(errRedeclaredMsg, n.token))
					continue loop
				}
				strt.Fields = append(strt.Fields, s)
				n.sym = s
				x += 1
			}

		case opBlockFnDcl, opExternFnDcl, opExprFnDcl:

			// Add actual symbol and link to existing symbol if already present
			fnType := &FunctionType{IsExternal: n.op == opExternFnDcl}
			sym := &Symbol{Name: n.token.Val, IsGlobal: true, Type: &Type{Kind: Function, Data: fnType}}
			if s, found := symtab.Define(sym); found {
				for ; s.Next != nil; s = s.Next { /* ... */ }
				s.Next = sym
			}
			n.sym = sym

			// Process parameters
			child := symtab.Child()
			n.symtab = child // Required during typecheck
			for _, param := range n.params {
				paramType, err := createType(child, param.left)
				if err != nil {
					errs = append(errs, err)
					continue loop
				}
				sym, found := child.Define(&Symbol{Name: param.token.Val, Type: paramType})
				param.sym = sym
				param.typ = paramType
				if found {
					errs = append(errs, semanticError(errRedeclaredMsg, param.token))
					continue loop
				}
				fnType.Args = append(fnType.Args, paramType)
			}

			// Process return
			fnType.ret = nothingType // Default case
			if n.left != nil {
				retType, err := createType(child, n.left)
				if err != nil {
					errs = append(errs, err)
					continue loop
				}
				fnType.ret = retType
			}

			// Check for termination
			if n.op == opBlockFnDcl && !fnType.ret.Is(Nothing) && !n.isTerminating() {
				errs = append(errs, semanticError(errMissingReturnMsg, n.token))
				continue loop
			}
		}
	}
	return errs
}

func createType(symtab *SymTab, n *Node) (*Type, error) {
	switch n.op {
	case opNamedType:
		s, ok := symtab.Resolve(n.token.Val)
		if !ok {
			return nil, semanticError(errUnknownTypeMsg, n.token)
		}
		return s.Type, nil

	case opArrayType:
		s, ok := symtab.Resolve(n.left.token.Val)
		if !ok {
			return nil, semanticError(errUnknownTypeMsg, n.left.token)
		}
		return &Type{ Kind: Array, Data: &ArrayType{Elem: s.Type} }, nil

	case opFuncType:
		var argTypes []*Type
		for _, arg := range n.stmts {
			t, err := createType(symtab, arg)
			if err != nil {
				return nil, err
			}
			argTypes = append(argTypes, t)
		}
		fnType := &FunctionType{ Args: argTypes }
		fnType.ret = nothingType
		if n.left != nil {
			t, err := createType(symtab, n.left)
			if err != nil {
				return nil, err
			}
			fnType.ret = t
		}
		return &Type{ Kind: Function, Data: fnType}, nil
	default:
		panic(fmt.Sprintf("AST node [%v]does not represent a type!", nodeTypes[n.op]))
	}
}

func addRuntimeInit(root *Node, symtab *SymTab, n *Node) error {
	if n.op == opBlockFnDcl && n.token.Val == "main" {
		n.stmts = append([]*Node{ {op:opFuncCall, token: &lex.Token{Val: "init"}, symtab: n.symtab, typ: nothingType} }, n.stmts...) // Insert runtime init
	}
	return nil
}

func foldConstants(root *Node, symtab *SymTab, n *Node) error {

	// Rewrite negative literals to single AST nodes
	if n.op == opNeg && n.left.op == opLit && n.left.token.Kind == lex.Integer {
		n.op = opLit
		n.token = lex.WithVal(n.left.token, "-" + n.left.token.Val)
		n.left = nil
	}

	// Check for overflow
	if n.op == opLit && n.token.Kind == lex.Integer {
		_, err := strconv.ParseInt(n.token.Val, 10, 64)
		if err != nil {
			return semanticError(errIntegerOverflowMsg, n.token)
		}
	}
	return nil
}

func generateStructConstructors(root *Node, symtab *SymTab, n *Node) error {
	if n.op == opStructDcl {
		_, err := generateStructConstructor(root, n)
		if err != nil {
			return err
		}
	}
	return nil
}

func generateStructConstructor(root *Node, n *Node) (*Symbol, error) {

	name := n.token.Val
	firstLetter := name[:1]

	// Check struct begins with lowercase
	if strings.ToUpper(firstLetter) == firstLetter {
		return nil, semanticError(errStructNamingLowerMsg, n.token)
	}

	// Create name
	constructorName := strings.ToUpper(firstLetter) + name[1:]

	// Ensure there are no other function definitions with this name
	if _, found := root.symtab.Resolve(constructorName); found {
		var n *Node
		for _, x := range root.stmts {
			if (x.op == opBlockFnDcl || x.op == opExprFnDcl || x.op == opExternFnDcl) && x.token.Val == constructorName {
				n = x
				break
			}
		}
		return nil, semanticError(errConstructorOverrideMsg, n.token)
	}

	// Collect struct field types
	var args []*Type
	var params []*Node

	for _, field := range n.stmts {

		// Copy symbol
		s := &Symbol{Name: field.sym.Name, Type: field.sym.Type}
		args = append(args, s.Type)

		// Copy node
		params = append(params, &Node{token: field.token, op: opIdentifier, sym: s, typ: s.Type})
	}

	// Create & define symbol
	fnSym := &Symbol{ Name: constructorName, IsGlobal: true, Type: &Type{ Kind: Function, Data:
	&FunctionType{ Args: args, isConstructor: true, ret: n.sym.Type, }}}
	root.symtab.Define(fnSym)

	// Add AST node
	root.Add(&Node{token:&lex.Token{Val : constructorName}, op: opBlockFnDcl, params: params, sym: fnSym})
	return fnSym, nil
}

func semanticError(msg string, t *lex.Token, vals ...interface{}) error {
	args := append([]interface{}(nil), t.File, t.Line, t.Pos, t.Val)
	args = append(args, vals...)
	return errors.New(fmt.Sprintf(msg, args...))
}

func semanticError2(msg string, t *lex.Token, vals ...interface{}) error {
	args := append([]interface{}(nil), t.File, t.Line, t.Pos)
	args = append(args, vals...)
	return errors.New(fmt.Sprintf(msg, args...))
}

type order byte
const (
	postOrder   = iota
	preOrder
	inOrder
)

func walk(o order, root *Node, symtab *SymTab, n *Node, visit func(*Node, *SymTab, *Node) error) (errs []error) {

	// Depth First Search

	// Visit left
	left := func() {
		if n.left != nil {
			errs = append(errs, walk(o, root, symtab, n.left, visit)...)
		}
	}

	// Visit right
	right := func() {
		if n.right != nil {
			errs = append(errs, walk(o, root, symtab, n.right, visit)...)
		}
	}

	// Visit current
	cur := func() {
		if err := visit(root, symtab, n); err != nil {
			errs = append(errs, err)
		}
		for _, param := range n.params {
			if param != nil {
				errs = append(errs, walk(o, root, symtab, param, visit)...)
			}
		}
		for _, stat := range n.stmts {
			if stat != nil {
				errs = append(errs, walk(o, root, symtab, stat, visit)...)
			}
		}
	}

	switch o {
	case postOrder:
		left()
		right()
		cur()

	case preOrder:
		cur()
		left()
		right()

	case inOrder:
		left()
		cur()
		right()

	default:
		panic(fmt.Sprintf("Unknown tree traversal order: %v", o))
	}

	return
}



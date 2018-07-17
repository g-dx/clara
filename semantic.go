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

			err := processFnType(n, n.token.Val, symtab)
			if err != nil {
				errs = append(errs, err)
				continue loop
			}
		}
	}
	return errs
}

func processFnType(n *Node, symName string, symtab *SymTab) (error) {
	// Add actual symbol and link to existing symbol if already present
	fnType := &FunctionType{Kind: Normal}
	if n.op == opExternFnDcl {
		fnType.Kind = External
	}
	sym := &Symbol{Name: symName, IsGlobal: true, Type: &Type{Kind: Function, Data: fnType}}
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
			return err
		}
		sym, found := child.Define(&Symbol{Name: param.token.Val, Type: paramType})
		param.sym = sym
		param.typ = paramType
		if found {
			return semanticError(errRedeclaredMsg, param.token)
		}
		fnType.Args = append(fnType.Args, paramType)
	}

	// Process return
	fnType.ret = nothingType // Default case
	if n.left != nil {
		retType, err := createType(child, n.left)
		if err != nil {
			return err
		}
		fnType.ret = retType
	}

	// Check for termination
	if n.op == opBlockFnDcl && !fnType.ret.Is(Nothing) && !n.isTerminating() {
		return semanticError(errMissingReturnMsg, n.token)
	}
	return nil
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

var id = uint(0) // TODO: Find a better solution to this...

func rewriteAnonFnAndClosures(rootNode *Node, rootSymtab *SymTab, n *Node) error {

	if n.isLocalFn() {

		id += 1

		freeVars := clIdentifyFreeVars(n)
		if len(freeVars) > 0 {

			// ----------------------------------------------------------
			// Closure
			// ----------------------------------------------------------

			// Generate closure & environment structs
			env, envCons := generateStruct(rootNode, fmt.Sprintf("env.%X", id), freeVars...)
			cl, clCons := generateStruct(rootNode, fmt.Sprintf("cl.%X", id), n.sym, env)

			// Rewrite <freevar> -> env.<freevar>
			clRewriteFreeVars(n, env, freeVars)

			// Hoist function to root & rename
			clFn := copyNode(n)
			clFn.token = lex.WithVal(clFn.token, fmt.Sprintf("clFn.%X", id))
			clFn.sym.Name = clFn.token.Val
			rootNode.Add(clFn)

			// Update function type information to record closure info
			fnType := clFn.sym.Type.AsFunction()
			fnType.Kind = Closure
			fnType.Data = &ClosureFunc{ gcFunc: cl.Type.GcName() } // GC func used during code gen

			// Build AST to capture free variables
			var envArgs []*Node
			for _, v := range freeVars {
				envArgs = append(envArgs, &Node{op: opIdentifier, token: &lex.Token{Val: v.Name}, sym: v, typ: v.Type})
			}

			// AST: fn() { ... } -> Cl(<fn name>, ClEnv(freeVars...))
			n.op = opFuncCall
			n.token = lex.WithVal(clFn.token, clCons.Name)
			n.sym = clCons
			n.stmts = []*Node{
				{op: opIdentifier, token: clFn.token, sym: clFn.sym, typ: clFn.typ},
				{op: opFuncCall, token: &lex.Token{Val: envCons.Name}, typ: envCons.Type, sym: envCons, stmts: envArgs},
			}
			n.left = nil
			n.right = nil
			n.params = nil

		} else {

			// ----------------------------------------------------------
			// Anonymous Function
			// ----------------------------------------------------------

			// Hoist function to root & rename
			fn := copyNode(n)
			fn.token = lex.WithVal(fn.token, fmt.Sprintf("anonFn.%X", id))
			rootNode.Add(fn)

			// AST: fn() { ... } -> <fn name>
			n.op = opIdentifier
			n.token = fn.token
			n.left = nil
			n.right = nil
			n.params = nil
			n.stmts = nil
		}
	}

	if n.isNonGlobalFnCall() {

		// ----------------------------------------------------------
		// Closure/Anonymous/Fn Pointer Call Site
		// ----------------------------------------------------------

		s := rootSymtab.MustResolve("invokeDynamic")

		// AST: <name>(args...) -> invokeDynamic(<name>, args...)
		var stmts []*Node
		if n.sym != nil {
			stmts = append([]*Node{ {op: opIdentifier, token: n.token, sym: n.sym, typ: n.typ} }, n.stmts...)
		} else {
			stmts = append([]*Node{ n.left }, n.stmts...)
		}
		n.stmts = stmts
		n.token = lex.WithVal(n.token, s.Name)
		n.left = nil
		n.sym = s
		n.typ = s.Type
	}

	return nil
}

func clIdentifyFreeVars(fn *Node) (vars []*Symbol) {

	syms := make(map[*Symbol]bool) // Set of syms

	walk(postOrder, nil, nil, fn, func(root *Node, symTab *SymTab, e *Node) error {
		if (e.op == opIdentifier || e.op == opFuncCall) && !fn.symtab.OwnedBy(e.sym) && !e.sym.IsGlobal {
			syms[e.sym] = true
		}
		return nil
	})
	for s := range syms {
		vars = append(vars, s)
	}
	return vars
}

func clRewriteFreeVars(n *Node, env *Symbol, freeVars []*Symbol) {

	// Update AST & function type to pass "env" as first parameter
	envVar := fmt.Sprintf("$env$")
	n.params = append([]*Node{{op: opIdentifier, token: &lex.Token{Val: envVar}, sym: env, typ: env.Type}}, n.params...)
	fn := n.sym.Type.AsFunction()
	fn.Args = append([]*Type{env.Type}, fn.Args...)

	walk(postOrder, nil, nil, n, func(root *Node, symTab *SymTab, e *Node) error {
		if e.op == opIdentifier || e.op == opFuncCall {
			// TODO: O(n) here, we should be able to be O(1)
			for _, freeVar := range freeVars {
				if freeVar == e.sym {

					// AST: <var> -> env.<var> or <fnCall> -> env.<fnCall>
					left := &Node{op: opIdentifier, token: &lex.Token{Val: envVar}, sym: env, typ: env.Type}
					var right *Node
					sym := env.Type.AsStruct().GetField(e.sym.Name)
					if e.op == opIdentifier {
						right = &Node{op: opIdentifier, token: e.token, sym: sym, typ: sym.Type}
					} else {
						right = &Node{op: opFuncCall, token: e.token, sym: sym, typ: sym.Type, stmts: e.stmts}
					}
					e.op = opDot
					e.token = lex.WithVal(e.token, ".")
					e.left = left
					e.right = right
					e.stmts = nil
				}
			}
		}
		return nil
	})
}

func generateStruct(root *Node, name string, fields ... *Symbol) (*Symbol, *Symbol) {

	var nodes []*Node
	var syms []*Symbol
	for i, f := range fields {

		// Create new symbol & associated AST
		sym := &Symbol{Name: f.Name, Addr: i * ptrSize, Type: f.Type}
		syms = append(syms, sym)
		nodes = append(nodes, &Node{op: opIdentifier, token: &lex.Token{Val: sym.Name}, sym: sym})
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
	&FunctionType{ Args: args, ret: n.sym.Type, Kind: StructCons }}}
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
			if err := visit(root, symtab, n.left); err != nil {
				errs = append(errs, err)
			}
		}
	}

	// Visit right
	right := func() {
		if n.right != nil {
			errs = append(errs, walk(o, root, symtab, n.right, visit)...)
			if err := visit(root, symtab, n.right); err != nil {
				errs = append(errs, err)
			}
		}
	}

	// Visit current
	cur := func() {
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
		if err := visit(root, symtab, n); err != nil {
			errs = append(errs, err)
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



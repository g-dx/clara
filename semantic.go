package main

import (
	"errors"
	"fmt"
	"github.com/g-dx/clarac/lex"
	"strconv"
	"strings"
)

//
// Functions for various semantic passes
//

const (
	errRedeclaredMsg            = "%v:%d:%d: error, '%v' redeclared"
	errUnknownTypeMsg           = "%v:%d:%d: error, unknown type '%v'"
	errUnknownVarMsg            = "%v:%d:%d: error, no declaration for identifier '%v' found"
	errAmbiguousVarMsg          = "%v:%d:%d: error, multiple identifiers for '%v' found:\n\t* %v"
	errStructNamingLowerMsg     = "%v:%d:%d: error, struct names must start with a lowercase letter, '%v'"
	errConstructorOverrideMsg   = "%v:%d:%d: error, function name '%v' is reserved for struct constructor"
	errNotStructMsg             = "%v:%d:%d: error, '%v' is not a struct"
	errStructHasNoFieldMsg      = "%v:%d:%d: error, field '%v' is not defined in struct '%v'"
	errInvalidDotSelectionMsg   = "%v:%d:%d: error '%v', expected field or function call"
	errInvalidOperatorTypeMsg   = "%v:%d:%d: type '%v' invalid for operator '%v'"
	errMismatchedTypesMsg       = "%v:%d:%d: mismatched types, got '%v', wanted '%v'"
	errInvalidNumberArgsMsg     = "%v:%d:%d: invalid number of arguments, got '%v', wanted '%v'"
	errInvalidNumberTypeArgsMsg = "%v:%d:%d: invalid number of type arguments, got '%v', wanted '%v'"
	errResolveFunctionMsg       = "%v:%d:%d: Cannot resolve function '%v'"
	errOverloadResolutionMsg    = "%v:%d:%d: Cannot resolve function '%v' from possible candidates:\n%v"
	errNonIntegerIndexMsg       = "%v:%d:%d: error, found type '%v', array index must be integer"
	errUnexpectedAssignMsg      = "%v:%d:%d: error, left hand side of assignment must be identifier"
	errNotAddressableAssignMsg  = "%v:%d:%d: error, left hand side of assignment is not addressable"
	errNotWritableAssignMsg     = "%v:%d:%d: error, cannot assign value to readonly field '%v'"
	errMissingReturnMsg         = "%v:%d:%d: error, missing return for function '%v'"
	errIntegerOverflowMsg       = "%v:%d:%d: error, constant '%v' overflow integer type"
	errUnknownEnumCaseMsg       = "%v:%d:%d: error, unknown case '%v' for enum '%v'"
	errMatchNotExhaustiveMsg    = "%v:%d:%d: error, match over enum '%v' is not exhaustive"
	errNotAnEnumCaseMsg         = "%v:%d:%d: error, '%v' is not an enum case"
	errTooManyArgsMsg           = "%v:%d:%d: error, '%v' exceeds maximum argument count of '%v'"
	errTypeParameterNotKnownMsg = "%v:%d:%d: error, type parameter(s) '%v' of return type '%v' not known, explicit function call type parameters required"
	errEmptyArrayLiteralMsg     = "%v:%d:%d: error, empty array literal not allowed ... yet!"
	errNoTypeParametersMsg      = "%v:%d:%d: error, type '%v' does not declare type parameters"
	maxCaseArgCount             = 5
	maxFnArgCount               = 6

	// Debug messages
	debugTypeInfoFormat = "⚫ %s%-60s%s %s%-30s%s ⇨ %s%s%s\n"
)

//---------------------------------------------------------------------------------------------------------------

type OperatorTypes map[int][]TypeKind

var operatorTypes = OperatorTypes{
	opAdd:    {Integer},
	opSub:    {Integer},
	opMul:    {Integer},
	opDiv:    {Integer},
	opRange:  {Integer},
	opOr:     {Boolean},
	opAnd:    {Boolean},
	opBAnd:   {Integer},
	opBOr:    {Integer},
	opBXor:   {Integer},
	opBLeft:  {Integer},
	opBRight: {Integer},
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
	for _, n := range rootNode.stmts {
		var topType *Type
		switch n.op {
		case opEnumDcl:
			n.symtab = symtab.Child()
			var types []*Type
			for _, tParam := range n.params {
				sym, found := n.symtab.Define(&Symbol{Name: tParam.token.Val, IsType: true})
				if found {
					errs = append(errs, semanticError(errRedeclaredMsg, tParam.token))
					continue
				}
				sym.Type = &Type{Kind: Parameter, Data: &ParameterType{Name: tParam.token.Val}}
				tParam.sym = sym
				tParam.typ = sym.Type
				types = append(types, sym.Type)
			}
			topType = &Type{Kind: Enum, Data: &EnumType{Name: n.token.Val, Types: types}}

		case opStructDcl:
			n.symtab = symtab.Child()
			var types []*Type
			for _, tParam := range n.params {
				sym, found := n.symtab.Define(&Symbol{Name: tParam.token.Val, IsType: true})
				if found {
					errs = append(errs, semanticError(errRedeclaredMsg, tParam.token))
					continue
				}
				sym.Type = &Type{Kind: Parameter, Data: &ParameterType{Name: tParam.token.Val}}
				tParam.sym = sym
				tParam.typ = sym.Type
				types = append(types, sym.Type)
			}
			topType = &Type{Kind: Struct, Data: &StructType{Name: n.token.Val, Types: types}}

		case opBlockFnDcl, opExprFnDcl, opExternFnDcl:
			// NOTE: This type is unimportant as function symbols created here
			// are intended only to check for redeclares. The real function symbols
			// are created later.
			topType = &Type{Kind: Nothing}

		default:
			continue
		}

		// Build symbol & ensure unique
		n.sym = &Symbol{Name: n.typeName(), IsGlobal: true, IsType: true, Type: topType}
		if _, found := symtab.Define(n.sym); found {
			errs = append(errs, semanticError(errRedeclaredMsg, n.token))
		}
	}
	if len(errs) > 0 {
		return errs
	}

	// Process structs, enums & funcs
loop:
	for _, n := range rootNode.stmts {
		switch n.op {
		case opEnumDcl:

			// Process each member constructor
			enumType := n.sym.Type.AsEnum()
			for i, cons := range n.stmts {

				// Build type info
				child := n.symtab.Child()
				consType, err := processFnType(cons, cons.token.Val, symtab, child, enumType.Types, false) // Add to root symtab
				if err != nil {
					errs = append(errs, err)
					continue loop
				}
				if len(consType.Params) > maxCaseArgCount {
					errs = append(errs, semanticError2(errTooManyArgsMsg, cons.token, cons.token.Val, maxCaseArgCount))
					continue
				}

				// Update function type information to record enum info
				consType.Kind = EnumCons
				consType.Data = &EnumConsFunc{Tag: i}
				enumType.Members = append(enumType.Members, consType)

				// Move to root AST node
				rootNode.Add(cons)
			}
			n.stmts = nil // Clear constructors

		case opStructDcl:

			strt := n.sym.Type.AsStruct()

		fields:
			for x, stmt := range n.stmts {

				// Look up type
				fieldType, err := createType(n.symtab, stmt.left)
				if err != nil {
					errs = append(errs, err)
					continue fields
				}
				s := &Symbol{Name: stmt.token.Val, Addr: x * ptrSize, Type: fieldType}

				// Define field
				if _, found := n.symtab.Define(s); found {
					errs = append(errs, semanticError(errRedeclaredMsg, stmt.token))
					continue fields
				}
				strt.Fields = append(strt.Fields, s)
				stmt.sym = s
			}

		case opBlockFnDcl, opExternFnDcl, opExprFnDcl:

			_, err := processFnType(n, n.token.Val, symtab, symtab.Child(), nil, true)
			if err != nil {
				errs = append(errs, err)
				continue loop
			}
		}
	}

	if len(errs) != 0 {
		return errs
	}

	// Perform type instantiation for all generic types now that all top
	// level types have been processed

	for _, n := range rootNode.stmts {
		switch n.op {
		case opStructDcl:
			errs = append(errs, instantiateFieldTypes(n)...)
		case opBlockFnDcl, opExternFnDcl, opExprFnDcl:
			errs = append(errs, instantiateFunctionTypes(n)...)
		}
	}
	return errs
}

func instantiateFieldTypes(n *Node) (errs []error) {
	for _, field := range n.stmts {
		// TODO: Set field.typ to new type as well?
		instantiated := instantiateType(n.symtab, field.left, &errs)
		field.sym.Type = instantiated
	}
	return errs
}

func instantiateFunctionTypes(n *Node) (errs []error) {
	for i, param := range n.params {
		// TODO: Set param.typ to new type as well?
		instantiated := instantiateType(n.symtab, param.left, &errs)
		param.sym.Type = instantiated
		n.sym.Type.AsFunction().Params[i] = instantiated
	}
	if n.left != nil {
		n.sym.Type.AsFunction().ret = instantiateType(n.symtab, n.left, &errs)
	}
	return errs
}

func instantiateType(symtab *SymTab, n *Node, errs *[]error) *Type {

	switch n.op {
	case opNamedType:
		s, ok := symtab.ResolveAll(n.token.Val, func(s *Symbol) bool { return s.IsType })
		if !ok {
			*errs = append(*errs, semanticError(errUnknownTypeMsg, n.token))
			return nil
		}

		// If not parameterised type simply return as is
		if n.left == nil {
			return s.Type
		}
		// Instantiate type parameter recursively
		var types []*Type
		for _, typeParam := range n.left.params {
			types = append(types, instantiateType(symtab, typeParam, errs))
		}
		switch s.Type.Kind {
		case Struct:
			st := s.Type.AsStruct()
			if len(st.Types) == 0 {
				*errs = append(*errs, semanticError2(errNoTypeParametersMsg, n.token, st.Name))
				return nil
			}
			if len(st.Types) != len(types) {
				*errs = append(*errs, semanticError2(errInvalidNumberTypeArgsMsg, n.token, len(types), len(st.Types)))
				return nil
			}
			if len(*errs) > 0 {
				return nil
			}
			bound := make(map[*Type]*Type)
			for i, t := range types {
				bound[st.Types[i]] = t
			}
			return substituteType(s.Type, bound)
		case Enum:
			et := s.Type.AsEnum()
			if len(et.Types) == 0 {
				*errs = append(*errs, semanticError2(errNoTypeParametersMsg, n.token, et.Name))
				return nil
			}
			if len(et.Types) != len(types) {
				*errs = append(*errs, semanticError2(errInvalidNumberTypeArgsMsg, n.token, len(types), len(et.Types)))
				return nil
			}
			if len(*errs) > 0 {
				return nil
			}
			bound := make(map[*Type]*Type)
			for i, t := range types {
				bound[et.Types[i]] = t
			}
			return substituteType(s.Type, bound)
		case Integer, String, Boolean, Bytes, Pointer, Parameter, Nothing:
			*errs = append(*errs, semanticError2(errNoTypeParametersMsg, n.token, s.Name))
			return nil
		default:
			panic("unreachable")
		}
	case opFuncType:
		// TODO: If no parameters or return is generic nothing to
		var params []*Type
		for _, param := range n.stmts {
			params = append(params, instantiateType(symtab, param, errs))
		}
		fn := &FunctionType{Params: params, ret: nothingType}
		if n.left != nil {
			fn.ret = instantiateType(symtab, n.left, errs)
		}
		if len(*errs) > 0 {
			return nil
		}
		return &Type{Kind: Function, Data: fn}

	case opArrayType:
		t := instantiateType(symtab, n.left, errs)
		if len(*errs) > 0 {
			return nil
		}
		return &Type{Kind: Array, Data: &ArrayType{Elem: t}}

	default:
		panic(fmt.Sprintf("AST node [%v] does not represent a type!", nodeTypes[n.op]))
	}
}

func processFnType(n *Node, symName string, symtab *SymTab, child *SymTab, types []*Type, allowOverload bool) (*FunctionType, error) {
	// Add actual symbol and link to existing symbol if already present
	fnType := &FunctionType{Kind: Normal, Types: types}
	if n.op == opExternFnDcl {
		fnType.Kind = External
	}
	if n.attrs.requiresRawValues() {
		fnType.RawValues = true
	}
	sym := &Symbol{Name: symName, IsGlobal: true, Type: &Type{Kind: Function, Data: fnType}}
	if s, found := symtab.Define(sym); found {
		if !allowOverload {
			return nil, semanticError(errRedeclaredMsg, n.token)
		}
		for ; s.Next != nil; s = s.Next { /* ... */
		}
		s.Next = sym
	}
	n.sym = sym

	// Process parameters
	n.symtab = child
	if n.right != nil {
		for _, typeParameter := range n.right.params {
			sym, found := n.symtab.Define(&Symbol{Name: typeParameter.token.Val, IsType: true})
			if found {
				return nil, semanticError(errRedeclaredMsg, typeParameter.token)
			}
			sym.Type = &Type{Kind: Parameter, Data: &ParameterType{Name: typeParameter.token.Val}}
			typeParameter.sym = sym
			typeParameter.typ = sym.Type
			fnType.Types = append(fnType.Types, sym.Type)
		}
	}
	for _, param := range n.params {
		paramType, err := createType(n.symtab, param.left)
		if err != nil {
			return nil, err
		}
		sym, found := n.symtab.Define(&Symbol{Name: param.token.Val, Type: paramType})
		param.sym = sym
		param.typ = paramType
		if found {
			return nil, semanticError(errRedeclaredMsg, param.token)
		}
		fnType.Params = append(fnType.Params, paramType)
	}

	// Process return
	fnType.ret = nothingType // Default case
	if n.left != nil {
		retType, err := createType(n.symtab, n.left)
		if err != nil {
			return nil, err
		}
		fnType.ret = retType
	}

	// Check for termination
	if n.op == opBlockFnDcl && !fnType.ret.Is(Nothing) && !n.isTerminating() {
		return nil, semanticError(errMissingReturnMsg, n.token)
	}
	return fnType, nil
}

// TODO: Should return a list of errs
func createType(symtab *SymTab, n *Node) (*Type, error) {
	switch n.op {
	case opNamedType:
		s, ok := symtab.ResolveAll(n.token.Val, func(s *Symbol) bool { return s.IsType })
		if !ok {
			return nil, semanticError(errUnknownTypeMsg, n.token)
		}
		// Validate all parameterised types exist
		if n.left != nil {
			for _, typeParam := range n.left.params {
				_, err := createType(symtab, typeParam)
				if err != nil {
					return nil, err
				}
			}
		}
		return s.Type, nil

	case opArrayType:
		elem, err := createType(symtab, n.left)
		if err != nil {
			return nil, err
		}
		return &Type{Kind: Array, Data: &ArrayType{Elem: elem}}, nil

	case opFuncType:
		var paramTypes []*Type
		for _, arg := range n.stmts {
			t, err := createType(symtab, arg)
			if err != nil {
				return nil, err
			}
			paramTypes = append(paramTypes, t)
		}
		fnType := &FunctionType{Params: paramTypes}
		fnType.ret = nothingType
		if n.left != nil {
			t, err := createType(symtab, n.left)
			if err != nil {
				return nil, err
			}
			fnType.ret = t
		}
		return &Type{Kind: Function, Data: fnType}, nil
	default:
		panic(fmt.Sprintf("AST node [%v]does not represent a type!", nodeTypes[n.op]))
	}
}

func foldConstants(errs *[]error, n *Node) {

	// Rewrite negative literals to single AST nodes
	if n.op == opNeg && n.left.op == opLit && n.left.token.Kind == lex.Integer {
		n.op = opLit
		n.token = lex.WithVal(n.left.token, "-"+n.left.token.Val)
		n.left = nil
	}

	// Check for overflow
	if n.op == opLit && n.token.Kind == lex.Integer {
		_, err := strconv.ParseInt(n.token.Val, 0, 64)
		if err != nil {
			*errs = append(*errs, semanticError(errIntegerOverflowMsg, n.token))
		}
	}
}

func lowerForStatement(n *Node) {
	// Maybe: for x in b where x > 2 {}      // Iterator with predicate
	if n.op == opFor {
		arrayOrRange := n.right
		var val *Node

		// 2 cases - arrayOrRange or range expression
		if arrayOrRange.typ.Is(Array) {
			var stmts []*Node

			// Array literals need assigned to a slot first
			if arrayOrRange.Is(opArrayLit) {
				arrayLitSlot := newVar("$arrayLit$", intArrayType)
				stmts = append(stmts, das(arrayLitSlot, arrayOrRange.left))
				arrayOrRange = arrayLitSlot
			}
			val := newVar("$idx$", intType)
			while := while(lt(val.copy(), length(arrayOrRange)))
			while.stmts = append(while.stmts, as(n.left, access(arrayOrRange, val.copy())))
			while.stmts = append(while.stmts, n.stmts...)
			while.stmts = append(while.stmts, inc(val.copy(), 1))
			n.stmts = append(stmts, das(val, intLit(0)), while)
		} else {
			val = n.left
			initVal := arrayOrRange.left
			cond := lt(val.copy(), arrayOrRange.right)
			nextVal := inc(val.copy(), 1)
			if arrayOrRange.right.Is(opLit) && arrayOrRange.right.token.Val == "0" {
				initVal = plus(arrayOrRange.left, intLit(-1))
				cond = lte(arrayOrRange.right, val.copy())
				nextVal = inc(val.copy(), -1)
			}
			while := while(cond)
			while.stmts = append(while.stmts, n.stmts...)
			while.stmts = append(while.stmts, nextVal)
			n.stmts = []*Node{das(val, initVal), while}
		}
		n.op = opBlock
		n.token = lex.WithVal(n.token, "-")
		n.left = nil
		n.right = nil
		n.typ = nil
		n.sym = nil
	}
}

func rewriteArrayLiteralExpr(n *Node, symtab *SymTab) {
	if n.Is(opArrayLit) {
		setElement := symtab.MustResolve("setElement")
		x := fnCallBySym(lex.NoToken, symtab.MustResolve("intArray"), intLit(len(n.stmts)))
		for i, expr := range n.stmts {
			if i < len(n.stmts) {
				x = fnCallBySym(lex.NoToken, setElement, x, intLit(i), expr)
			}
		}
		n.left = x
		n.stmts = nil
	}
}

func lowerMatchStatement(symtab *SymTab, n *Node) {
	if n.op == opMatch {

		// AST:
		// match <expr> {
		//   case First(a, b):
		//
		//   case Second(c):
		//
		// }
		//
		// ->
		//
		// {
		//   $tmp1 := <expr>
		//   if asEnum($tmp1).Tag == First.Tag {
		//      a := asEnum($tmp1)._0
		//      b := asEnum($tmp1)._1
		//
		//   } else if asEnum($tmp1).Tag == Second.Tag {
		//      c := asEnum($tmp1)._0
		//   }
		// }

		// Declare var for match expression result
		matchVar := newVar("$tmp", n.left.typ)
		matchExpr := das(matchVar, n.left)

		// Convert cases to if/else if
		asEnum := symtab.MustResolve("asEnum")
		enum := symtab.MustResolve("enum_").Type.AsStruct()
		var cur *Node
		for i, cas := range n.stmts {

			// Create expr to compare tags
			tag := cas.sym.Type.AsFunction().AsEnumCons().Tag
			caseExpr := eq(
				dot(fnCallBySym(lex.NoToken, asEnum, matchVar),
					ident(lex.NoToken, enum.GetField("tag")), intType),
				intLit(tag),
			)

			// opCase -> opIf/ElseIf
			cas.left = caseExpr
			cas.typ = nil
			cas.sym = nil
			cas.token = nil
			cas.op = opElseIf
			if i == 0 {
				cas.op = opIf
				cur = cas
			} else {
				cur.right = cas
			}
			cur = cas

			// Declare case vars
			var vars []*Node
			for i, v := range cas.params {
				field := enum.GetField(fmt.Sprintf("_%v", i))
				vars = append(vars,
					das(v,
						dot(fnCallBySym(lex.NoToken, asEnum, matchVar),
							ident(lex.NoToken, field), v.typ))) // Expression yields type on left!
			}
			cas.stmts = append(vars, cas.stmts...)
			cas.params = nil
		}

		// opMatch -> opBlock
		n.op = opBlock
		if len(n.stmts) > 0 {
			n.stmts = []*Node{matchExpr, n.stmts[0]}
		} else {
			n.stmts = []*Node{matchExpr}
		}
		n.left = nil
		n.token = nil
	}
}

func generateStructConstructors(errs *[]error, root *Node, n *Node) {
	if n.op == opStructDcl {
		_, err := generateStructConstructor(root, n)
		if err != nil {
			*errs = append(*errs, err)
		}
	}
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
			if x.isFuncDcl() && x.token.Val == constructorName {
				n = x
				break
			}
		}
		return nil, semanticError(errConstructorOverrideMsg, n.token)
	}

	// Create function
	st := n.sym.Type
	ft := &FunctionType{ret: st, Types: st.AsStruct().Types, Kind: StructCons}
	fs := &Symbol{Name: constructorName, IsGlobal: true, Type: &Type{Kind: Function, Data: ft}}
	root.symtab.Define(fs)

	// Create & add fn to root
	cons := &Node{token: &lex.Token{Val: constructorName}, op: opConsFnDcl, sym: fs}
	root.Add(cons)

	for _, field := range n.stmts {

		// Copy symbol
		ft.Params = append(ft.Params, field.sym.Type)

		// Copy node
		s := &Symbol{Name: field.sym.Name, Type: field.sym.Type}
		cons.params = append(cons.params, ident(field.token, s))
	}
	return fs, nil
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

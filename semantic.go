package main

import (
	"errors"
	"fmt"
	"github.com/g-dx/clarac/lex"
	"math/rand"
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
	errTypeParameterNotBoundMsg = "%v:%d:%d: error, type parameter '%v' is not bound for this function call"
	maxCaseArgCount             = 5
	maxFnArgCount               = 6

	// Debug messages
	debugTypeInfoFormat = "⚫ %s%-60s%s %s%-30s%s ⇨ %s%s%s\n"
)

//---------------------------------------------------------------------------------------------------------------

type OperatorTypes map[int][]TypeKind

var operatorTypes = OperatorTypes{
	opAdd:    {Integer, Byte},
	opSub:    {Integer, Byte},
	opMul:    {Integer, Byte},
	opDiv:    {Integer, Byte},
	opOr:     {Boolean},
	opAnd:    {Boolean},
	opBAnd:   {Integer, Byte},
	opBOr:    {Integer, Byte},
	opBXor:   {Integer, Byte},
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
			topType = &Type{Kind: Enum, Data: &EnumType{Name: n.token.Val}}

		case opStructDcl:
			topType = &Type{Kind: Struct, Data: &StructType{Name: n.token.Val}}

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
				consType, err := processFnType(cons, cons.token.Val, symtab, false) // Add to root symtab
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
			s, _ := symtab.Resolve(n.token.Val)
			strt := s.Type.AsStruct()

			// Calculate field information
			x := 0
			n.symtab = symtab.Child()
			for _, stmt := range n.stmts {

				// Look up type
				fieldType, err := createType(n.symtab, stmt.left)
				if err != nil {
					errs = append(errs, err)
					continue loop
				}
				s := &Symbol{Name: stmt.token.Val, Addr: x * ptrSize, Type: fieldType}

				// Define field
				if _, found := n.symtab.Define(s); found {
					errs = append(errs, semanticError(errRedeclaredMsg, stmt.token))
					continue loop
				}
				strt.Fields = append(strt.Fields, s)
				stmt.sym = s
				x += 1
			}

		case opBlockFnDcl, opExternFnDcl, opExprFnDcl:

			_, err := processFnType(n, n.token.Val, symtab, true)
			if err != nil {
				errs = append(errs, err)
				continue loop
			}
		}
	}
	return errs
}

func processFnType(n *Node, symName string, symtab *SymTab, allowOverload bool) (*FunctionType, error) {
	// Add actual symbol and link to existing symbol if already present
	fnType := &FunctionType{Kind: Normal}
	if n.op == opExternFnDcl {
		fnType.Kind = External
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
	n.symtab = symtab.Child()
	if n.right != nil {
		for _, typeParameter := range n.right.params {
			sym, found := n.symtab.Define(&Symbol{Name: typeParameter.token.Val, IsType: true})
			if found {
				return nil, semanticError(errRedeclaredMsg, typeParameter.token)
			}
			sym.Type = &Type{Kind: Parameter, Data: &ParameterType{Width: 8, Name: typeParameter.token.Val}}
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

func createType(symtab *SymTab, n *Node) (*Type, error) {
	switch n.op {
	case opNamedType:
		s, ok := symtab.ResolveAll(n.token.Val, func(s *Symbol) bool { return s.IsType })
		if !ok {
			return nil, semanticError(errUnknownTypeMsg, n.token)
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

func applyBoxing(n *Node, symtab *SymTab) {
	if n.isGenericFnCall() {
		h := NewAstHelper(symtab)
		for i, arg := range n.stmts {
			if arg.typ.IsPrimitive() {
				switch arg.typ.Kind {
				case Array:
					panic(fmt.Sprintf("Invoking generic functions with arrays of primitives is not implemented!"))
				case Function:
					// Create a function to generate a boxing shim around the original function.
					fnDcl := h.fnDcl(&Type{
						Kind: Function,
						Data: &FunctionType{Kind: Normal, Params: []*Type{arg.typ}, ret: h.boxType(arg.typ)},
					})
					fnDcl.Add(h.returnExpr(h.boxExpr(fnDcl.params[0])))

					// Invoke function to generate shim by passing the original unboxed function
					n.stmts[i] = h.fnCallByType(fnDcl, arg)
				default:
					n.stmts[i] = h.boxExpr(arg)
				}
			}
		}
		if n.typ.IsPrimitive() {
			switch n.typ.Kind {
			case Integer:
				h.interceptFnCallBySym(h.iUnwrap, n)
			case Byte:
				h.interceptFnCallBySym(h.btUnwrap, n)
			case Boolean:
				h.interceptFnCallBySym(h.boUnwrap, n)
			case Function:
				// Create an anonymous function to produce a boxed -> unboxed function
				fnDcl := h.fnDcl(&Type{
					Kind: Function,
					Data: &FunctionType{Kind: Normal, Params: []*Type{h.boxType(n.typ)}, ret: n.typ},
				})

				// Build & return and unboxed function
				fnDcl.Add(h.returnExpr(h.unboxExpr(fnDcl.params[0])))

				// Immediately invoke anonymous function
				n.stmts = []*Node {h.fnCallBySym(n.sym, n.stmts...)}
				n.token = lex.NoToken
				n.left = fnDcl
				n.sym = nil
				n.params = nil
			}
		}
	}
}

type astHelper struct {
	iWrap, iUnwrap   *Symbol
	btWrap, btUnwrap *Symbol
	boWrap, boUnwrap *Symbol
}

func NewAstHelper(syms *SymTab) *astHelper {
	return &astHelper{
		iWrap:    syms.MustResolve("IntWrapper"),
		iUnwrap:  syms.MustResolve("unwrapInt"),
		btWrap:   syms.MustResolve("ByteWrapper"),
		btUnwrap: syms.MustResolve("unwrapByte"),
		boWrap:   syms.MustResolve("BoolWrapper"),
		boUnwrap: syms.MustResolve("unwrapBool"),
	}
}

func (h *astHelper) interceptFnCallBySym(s *Symbol, n * Node) {
	if !n.Is(opFuncCall) {
		panic(fmt.Sprintf("Node to intercept is not a fn call!"))
	}
	if !s.Type.Is(Function) {
		panic(fmt.Sprintf("Intercept symbol is not a fn!"))
	}
	n.stmts = []*Node {h.fnCallBySym(n.sym, n.stmts...)}
	n.token = lex.WithVal(lex.NoToken, s.Name)
	n.sym = s
	n.typ = s.Type.AsFunction().ret
	n.params = nil
}

func (h *astHelper) boxExpr(n * Node) *Node {
	switch n.typ.Kind {
	default:
		return n // No boxing required
	case Integer:
		return h.fnCallBySym(h.iWrap, n)
	case Byte:
		return h.fnCallBySym(h.btWrap, n)
	case Boolean:
		return h.fnCallBySym(h.boWrap, n)
	case Function:
		// Build new function with args & return boxed where necessary
		fnDcl := h.fnDcl(h.boxType(n.typ))

		// Unbox parameters & invoke original function
		var args []*Node
		for _, arg := range fnDcl.params {
			args = append(args, h.unboxExpr(arg))
		}
		call := h.fnCallBySym(n.sym, args...)

		// If no return value, we are done. Otherwise store in tmp, return boxed version of tmp
		if call.typ.Is(Nothing) {
			fnDcl.Add(call).Add(h.returnExpr(nil))
		} else {
			das := h.dasStmt(fnDcl.symtab, call, fmt.Sprintf("_%v", len(fnDcl.params)))
			fnDcl.Add(das).Add(h.returnExpr(h.boxExpr(das.left)))
		}
		return fnDcl
	}
}

func (h *astHelper) unboxExpr(n * Node) *Node {
	switch n.typ.Kind {
	case Struct:
		switch n.typ.AsStruct().Name {
		default:
			return n // No unboxing required
		case "intWrapper":
			return h.fnCallBySym(h.iUnwrap, n)
		case "byteWrapper":
			return h.fnCallBySym(h.btUnwrap, n)
		case "boolWrapper":
			return h.fnCallBySym(h.boUnwrap, n)
		}
	case Function:
		// Build new function with args & return unboxed where necessary
		fnDcl := h.fnDcl(h.unboxType(n.typ))

		// box parameters & invoke original function
		var args []*Node
		for _, arg := range fnDcl.params {
			args = append(args, h.boxExpr(arg))
		}
		call := h.fnCallBySym(n.sym, args...)

		// If no return value, we are done. Otherwise store in tmp, return boxed version of tmp
		if call.typ.Is(Nothing) {
			fnDcl.Add(call).Add(h.returnExpr(nil))
		} else {
			das := h.dasStmt(fnDcl.symtab, call, fmt.Sprintf("_%v", len(fnDcl.params)))
			fnDcl.Add(das).Add(h.returnExpr(h.unboxExpr(das.left)))
		}
		return fnDcl
	default:
		return n // No unboxing required
	}
}

func (h *astHelper) boxType(t *Type) *Type {
	switch t.Kind {
	default:
		return t // No boxing required
	case Byte:
		return h.btWrap.Type.AsFunction().ret
	case Integer:
		return h.iWrap.Type.AsFunction().ret
	case Boolean:
		return h.boWrap.Type.AsFunction().ret
	case Function:
		f := t.AsFunction()
		fd := &FunctionType{
			Kind:       f.Kind,
			Data:       f.Data,
			Types:      append([]*Type(nil), f.Types...),
			isVariadic: f.isVariadic,
		}
		for _, p := range f.Params {
			fd.Params = append(fd.Params, h.boxType(p))
		}
		fd.ret = h.boxType(f.ret)
		return &Type{Kind: Function, Data: fd}
	}
}

func (h *astHelper) unboxType(t *Type) *Type {
	switch t.Kind {
	case Struct:
		switch t.AsStruct().Name {
		default:
			return t // No unboxing required
		case "intWrapper":
			return h.iUnwrap.Type.AsFunction().ret
		case "byteWrapper":
			return h.btUnwrap.Type.AsFunction().ret
		case "boolWrapper":
			return h.boUnwrap.Type.AsFunction().ret
		}
	case Function:
		f := t.AsFunction()
		fd := &FunctionType{
			Kind:       f.Kind,
			Data:       f.Data,
			Types:      append([]*Type(nil), f.Types...),
			isVariadic: f.isVariadic,
		}
		for _, p := range f.Params {
			fd.Params = append(fd.Params, h.unboxType(p))
		}
		fd.ret = h.unboxType(f.ret)
		return &Type{Kind: Function, Data: fd}
	default:
		return t // No unboxing required
	}
}

func (h *astHelper) fnCallBySym(s *Symbol, args ... *Node) *Node {
	if s == nil {
		panic("nil function symbol")
	}
	return fnCallBySym(lex.WithVal(lex.NoToken, s.Name), s, args...)
}

func (h *astHelper) fnCallByType(n *Node, args ...*Node) *Node {
	return &Node{op: opFuncCall, token: lex.NoToken, typ: n.typ.AsFunction().ret, stmts: args, left: n}
}

func (h *astHelper) returnExpr(n *Node) *Node {
	x := &Node{op: opReturn, token: lex.NoToken}
	if n != nil {
		x.left =  n
		x.typ = n.typ
	}
	return x
}

func (h *astHelper) dasStmt(symtab *SymTab, n *Node, name string) *Node {
	sym := &Symbol{Name: name, Type: n.typ, IsStack: true}
	symtab.Define(sym)
	id := ident(lex.WithVal(lex.NoToken, sym.Name), sym)
	return &Node{op: opDas, token: lex.NoToken, left: id, right: n}
}

func (h *astHelper) fnDcl(t *Type) *Node {
	// Create function
	n := &Node{
		op: opBlockFnDcl,
		token:  lex.WithVal(lex.NoToken, "fn"),
		typ:    t,
		sym:    &Symbol{Name: fmt.Sprintf("%X", rand.Uint32()), Type: t},
		symtab: NewSymtab(),
	}
	// Create parameters
	for i, pType := range t.AsFunction().Params {
		s := &Symbol{Name: fmt.Sprintf("_%v", i), Type: pType, IsStack: true}
		param := &Node{
			op:    opIdentifier,
			token: lex.WithVal(lex.NoToken, s.Name),
			sym:   s,
			typ:   pType,
		}
		n.symtab.Define(param.sym)
		n.params = append(n.params, param)
	}
	return n
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

func declareCaseVars(symtab *SymTab, n *Node) {
	if n.op == opMatch {

		// AST: case <ident>(<var1>, <var2>, etc...): -> <var1> = asEnum(e)._1, <var2> = asEnum(e)._2, etc...
		asEnum := symtab.MustResolve("asEnum")
		enum := symtab.MustResolve("enum_").Type.AsStruct()
		for _, cas := range n.stmts {
			var vars []*Node
			for i, v := range cas.params {
				field := enum.GetField(fmt.Sprintf("_%v", i))
				vars = append(vars,
					&Node{op: opDas, left: v, right: &Node{op: opDot,
						left:  fnCallBySym(lex.NoToken, asEnum, n.left),
						right: ident(lex.NoToken, field),
						typ:   v.typ}, // Expression yields type on left!
					})
			}
			cas.stmts = append(vars, cas.stmts...)
			cas.params = nil
		}
	}
}

func lowerMatchStatement(symtab *SymTab, n *Node) {
	if n.op == opMatch {

		// AST:
		// match <expr> {
		//   case First(...):
		//
		//   case Second(...):
		//
		// }
		//
		// ->
		//
		// {
		//   $tmp1 := <expr>
		//   if asEnum($tmp1).Tag == First.Tag {
		//
		//   } else if asEnum($tmp1).Tag == Second.Tag {
		//
		//   }
		// }

		// Declare var for match expression result
		matchVar := ident(lex.NoToken, &Symbol{Name: "$tmp", Type: n.left.typ, IsStack: true})
		matchExpr := &Node{op: opDas, left: matchVar, right: n.left}

		// Convert cases to if/else if
		asEnum := symtab.MustResolve("asEnum")
		enum := symtab.MustResolve("enum_").Type.AsStruct()
		var cur *Node
		for i, cas := range n.stmts {

			// Create expr to compare tags
			tag := cas.sym.Type.AsFunction().AsEnumCons().Tag
			caseExpr := &Node{op: opEq,
				left: &Node{op: opDot,
					left:  fnCallBySym(lex.NoToken, asEnum, matchVar),
					right: ident(lex.NoToken, enum.GetField("tag")),
				},
				right: &Node{op: opLit, sym: &Symbol{Name: strconv.Itoa(tag), Type: intType}},
				typ:   boolType,
			}

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
	ft := &FunctionType{ret: n.sym.Type, Kind: StructCons}
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
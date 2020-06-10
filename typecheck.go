package main

import (
	"bytes"
	"fmt"
	"github.com/g-dx/clarac/console"
	"github.com/g-dx/clarac/lex"
	"math/rand"
	"strings"
)

//---------------------------------------------------------------------------------------------------------------

func typeCheck(n *Node, symtab *SymTab, fn *FunctionType, debug bool) (errs []error) {

	left := n.left
	right := n.right

	switch n.op {
	case opWhile:
		errs = append(errs, typeCheck(left, symtab, fn, debug)...)

		if !left.hasType() {
			goto end
		}

		if !left.typ.Is(Boolean) {
			errs = append(errs, semanticError2(errMismatchedTypesMsg, left.token, left.typ, boolType))
			goto end
		}

		// Type check body
		n.symtab = symtab.Child()
		for _, stmt := range n.stmts {
			errs = append(errs, typeCheck(stmt, n.symtab, fn, debug)...)
		}

	case opFor:
		errs = append(errs, typeCheckFor(n, symtab, fn, debug)...)

	case opTernary:
		errs = append(errs, typeCheckTernary(n, symtab, fn, debug)...)

	case opIf, opElseIf:
		errs = append(errs, typeCheck(left, symtab, fn, debug)...)

		if !left.hasType() {
			goto end
		}

		if !left.typ.Is(Boolean) {
			errs = append(errs, semanticError2(errMismatchedTypesMsg, left.token, left.typ, boolType))
			goto end
		}

		// Type check body
		n.symtab = symtab.Child()
		for _, stmt := range n.stmts {
			errs = append(errs, typeCheck(stmt, n.symtab, fn, debug)...)
		}

		// Type check next elseif case (if any)
		if right != nil {
			errs = append(errs, typeCheck(right, symtab, fn, debug)...)
		}

		// Does not promote type...

	case opElse, opBlock:
		// Type check body
		n.symtab = symtab.Child()
		for _, stmt := range n.stmts {
			errs = append(errs, typeCheck(stmt, n.symtab, fn, debug)...)
		}

		// Does not promote type...

	case opReturn:

		// Default to empty return
		rType := nothingType
		rToken := n.token

		// Check expression if any
		if left != nil {
			errs = append(errs, typeCheck(left, symtab, fn, debug)...)
			if !left.hasType() {
				goto end
			}
			rType = left.typ
			rToken = left.token
		}

		if !fn.ret.Matches(rType) {
			errs = append(errs, semanticError2(errMismatchedTypesMsg, rToken, rType, fn.ret))
			goto end
		}
		n.typ = rType

	case opAnd, opOr, opAdd, opMul, opSub, opDiv, opBAnd, opBOr, opBXor, opBLeft, opBRight, opRange:
		errs = append(errs, typeCheck(left, symtab, fn, debug)...)
		errs = append(errs, typeCheck(right, symtab, fn, debug)...)

		if !left.hasType() || !right.hasType() {
			goto end
		}

		if !operatorTypes.isValid(n.op, left.typ.Kind) {
			// Not valid for op
			errs = append(errs, semanticError2(errInvalidOperatorTypeMsg, left.token, left.typ, n.token.Val))
			goto end
		}
		if !operatorTypes.isValid(n.op, right.typ.Kind) {
			// Not valid for op
			errs = append(errs, semanticError2(errInvalidOperatorTypeMsg, right.token, right.typ, n.token.Val))
			goto end
		}
		if !left.typ.Matches(right.typ) {
			// Mismatched types
			errs = append(errs, semanticError2(errMismatchedTypesMsg, left.token, left.typ, right.typ))
		}

		// Promote appropriate type
		switch n.op {
		case opAnd, opOr:
			n.typ = boolType
		default:
			n.typ = intType // All arithmetic operations produces int
		}

	case opNot:
		errs = append(errs, typeCheck(left, symtab, fn, debug)...)

		if !left.hasType() {
			goto end
		}

		if !left.typ.Is(Boolean) {
			errs = append(errs, semanticError2(errMismatchedTypesMsg, left.token, left.typ, boolType))
			goto end
		}
		n.typ = boolType

	case opBNot:
		errs = append(errs, typeCheck(left, symtab, fn, debug)...)

		if !left.hasType() {
			goto end
		}

		if !left.typ.Is(Integer) {
			errs = append(errs, semanticError2(errMismatchedTypesMsg, left.token, left.typ, intType))
			goto end
		}
		n.typ = intType

	case opNeg:
		errs = append(errs, typeCheck(left, symtab, fn, debug)...)

		if !left.hasType() {
			goto end
		}

		if !(left.typ.Is(Integer) || left.typ.Is(Byte)) {
			errs = append(errs, semanticError2(errMismatchedTypesMsg, left.token, left.typ, fmt.Sprintf("%v or %v", Integer, Byte)))
			goto end
		}
		n.typ = left.typ

	case opLit:
		s, found := symtab.Resolve(n.token.Val)
		if !found {
			s, _ = symtab.Define(&Symbol{Name: n.token.Val, IsLiteral: true})
			switch n.token.Kind {
			case lex.Integer:
				s.Type = intType
			case lex.String:
				s.Type = stringType
			case lex.True, lex.False:
				s.Type = boolType
			default:
				panic(fmt.Sprintf("Unknown literal! %v", lex.KindValues[n.token.Kind]))
			}
		}
		n.sym = s
		n.typ = n.sym.Type

	case opIdentifier:
		err := typeCheckIdentifier(n, symtab, false)
		if err != nil {
			errs = append(errs, err)
		}

	case opFuncCall:
		errs = append(errs, typeCheckFuncCall(n, symtab, symtab, fn, debug)...)

	case opGt, opGte, opLt, opLte, opEq:
		errs = append(errs, typeCheck(left, symtab, fn, debug)...)
		errs = append(errs, typeCheck(right, symtab, fn, debug)...)

		if !left.hasType() || !right.hasType() {
			goto end
		}
		if !left.typ.Matches(right.typ) {
			errs = append(errs, semanticError2(errMismatchedTypesMsg, left.token, left.typ, right.typ))
			goto end
		}
		n.typ = boolType

	case opStructDcl, opEnumDcl:
		// Nothing to do...

	case opBlockFnDcl, opExternFnDcl, opExprFnDcl, opConsFnDcl:

		// Closures will not have been annotated yet. Do it now.
		if n.sym == nil {
			_, err := processFnType(n, fmt.Sprintf("%X", rand.Uint32()), symtab, false)
			if err != nil {
				errs = append(errs, err)
				goto end
			}
			n.typ = n.sym.Type
		}

		// Type check stmts
		fn := n.sym.Type.AsFunction()
		for _, stmt := range n.stmts {
			errs = append(errs, typeCheck(stmt, n.symtab, fn, debug)...)
		}

		// Check expression function return type
		if n.op == opExprFnDcl {
			expr := n.stmts[0]
			if expr.typ != nil && !fn.ret.Matches(expr.typ) {
				errs = append(errs, semanticError2(errMismatchedTypesMsg, n.stmts[0].token, n.stmts[0].typ, fn.ret))
				goto end
			}
		}

	case opDot:
		errs = append(errs, typeCheck(left, symtab, fn, debug)...)

		if !left.hasType() {
			goto end
		}

		// Handle func call on right
		if right.op == opFuncCall {

			// Rewrite to func call
			n.op = opFuncCall
			n.token = right.token

			// Check if call is field _of_ struct or normal dot selection rules apply
			if left.typ.Is(Struct) && left.typ.AsStruct().HasField(right.left.token.Val) {
				n.stmts = right.stmts
				n.left = &Node{op: opDot, token: lex.WithVal(n.token, "."), left: left, right: right.left}
			} else {
				n.stmts = append([]*Node{n.left}, right.stmts...)
				n.left = right.left
			}
			n.right = nil

			// Type check func call
			errs = append(errs, typeCheck(n, symtab, fn, debug)...)

			// Handle array access on right
		} else if right.op == opArray {

			// Rewrite to array access
			n.op = opArray
			n.token = right.token
			n.left = &Node{op: opDot, token: lex.WithVal(n.token, "."), left: left, right: right.left}
			n.right = right.right
			errs = append(errs, typeCheck(n, symtab, fn, debug)...)

			// Handle field access on right
		} else if right.op == opIdentifier {

			// SPECIAL CASE: Fudge strings to give them a special int field "length" at offset 0
			if (left.typ.Is(Array) || left.typ.Is(String)) && right.token.Val == "length" {
				right.sym = &Symbol{Name: "length", Addr: 0, Type: intType}
				right.typ = right.sym.Type
				n.typ = right.typ
				return errs
			}

			// SPECIAL CASE: Pointer types can be dereferenced by
			if left.typ.Is(Pointer) && right.token.Val == "deref" {
				right.sym = &Symbol{Name: "deref", Addr: 0, Type: pointerType}
				right.typ = right.sym.Type
				n.typ = right.typ
				return errs
			}

			// Check we have a struct
			var strct *StructType
			if left.typ.Is(Struct) {
				strct = left.typ.AsStruct()
			} else if left.typ.IsFunction(Struct) && left.op == opFuncCall {
				strct = left.typ.AsFunction().ret.AsStruct()
			} else {
				errs = append(errs, semanticError(errNotStructMsg, left.token))
				goto end
			}

			// Check field exists in struct
			sym := strct.GetField(right.token.Val)
			if sym == nil {
				errs = append(errs, semanticError(errStructHasNoFieldMsg, right.token, strct.Name))
				goto end
			}

			// Set right symbol and set parent as right
			right.sym = sym
			right.typ = sym.Type
			n.typ = right.typ

		} else {
			// Unexpected type on right
			errs = append(errs, semanticError(errInvalidDotSelectionMsg, right.token))
			goto end
		}

	case opArrayLit:
		errs = append(errs, typeCheckArrayLit(n, symtab, fn, debug)...)

	case opArray:
		errs = append(errs, typeCheck(left, symtab, fn, debug)...)
		errs = append(errs, typeCheck(right, symtab, fn, debug)...)

		if !left.hasType() || !right.hasType() {
			goto end
		}

		if !right.typ.Is(Integer) {
			errs = append(errs, semanticError2(errNonIntegerIndexMsg, right.token, right.typ))
			goto end
		}

		// SPECIAL CASE: If the left type is a string, array access yields a byte
		if left.typ.Is(String) {
			n.typ = byteType
		} else if left.typ.Is(Array) {
			n.typ = left.typ.AsArray().Elem
		} else {
			errs = append(errs, semanticError2(errMismatchedTypesMsg, n.token, left.typ, "string or array type"))
			goto end
		}

	case opDas:
		errs = append(errs, typeCheck(right, symtab, fn, debug)...)

		if !right.hasType() {
			goto end
		}

		// Check we have identifier on left
		// TODO: Should we attempt to type check left to get more information?
		if left.op != opIdentifier {
			errs = append(errs, semanticError2(errUnexpectedAssignMsg, left.token))
		}

		// Now right is resolved, define symbol for left
		sym, ok := symtab.Define(&Symbol{Name: left.token.Val, IsStack: true})
		if ok {
			errs = append(errs, semanticError(errRedeclaredMsg, left.token))
			goto end
		}

		// Left gets type of right
		left.sym = sym
		left.sym.Type = right.typ
		left.typ = right.typ

		// Does not promote type...

	case opAs:
		errs = append(errs, typeCheck(right, symtab, fn, debug)...)
		errs = append(errs, typeCheck(left, symtab, fn, debug)...)

		if !right.hasType() || !left.hasType() {
			goto end
		}

		// Check left is addressable
		if !left.isAddressable() {
			errs = append(errs, semanticError2(errNotAddressableAssignMsg, left.token))
			goto end
		}

		// Check left is writable
		if left.isReadOnly() {
			errs = append(errs, semanticError2(errNotWritableAssignMsg, left.right.token, left.right.token.Val))
			goto end
		}

		// Check types in assignment
		if !left.typ.Matches(right.typ) {
			errs = append(errs, semanticError2(errMismatchedTypesMsg, right.token, right.typ, left.typ))
			goto end
		}

		// Does not promote type...

	case opRoot:
		for _, n := range n.stmts {
			errs = append(errs, typeCheck(n, symtab, nil, debug)...)
		}

	case opError:
		// TODO: Decide what to do here...
		goto end

	case opMatch:
		errs = append(errs, typeCheck(left, symtab, fn, debug)...)

		if !left.hasType() {
			goto end
		}

		// Handle cases
		for _, caseBlock := range n.stmts {
			errs = append(errs, typeCheck(caseBlock, symtab, fn, debug)...)
		}

		// Ensure enum type
		if !left.typ.Is(Enum) {
			errs = append(errs, semanticError2(errMismatchedTypesMsg, left.token, left.typ, "<enum>"))
			goto end
		}

		enum := left.typ.AsEnum()
		cases := make(map[*FunctionType]bool)
		for _, cons := range n.stmts {
			if cons.sym == nil {
				continue
			}

			// Ensure cons function belong to this enum
			if !enum.HasMember(cons.sym.Type.AsFunction()) {
				errs = append(errs, semanticError2(errUnknownEnumCaseMsg, cons.token, cons.token.Val, left.typ))
				continue
			}

			// Check no repeated cases
			fn := cons.sym.Type.AsFunction()
			if _, ok := cases[fn]; ok {
				errs = append(errs, semanticError2(errRedeclaredMsg, cons.token, cons.token.Val))
				continue
			}
			cases[fn] = true
		}

		if len(errs) > 0 {
			goto end
		}

		// TODO: Allow "remaining" keyword to be used
		if len(n.stmts) != len(enum.Members) {
			errs = append(errs, semanticError2(errMatchNotExhaustiveMsg, left.token, left.typ))
			goto end
		}

	case opCase:

		// Attempt to find constructor
		sym, ok := symtab.Resolve(n.token.Val)
		if !ok || !sym.Type.Is(Function) || !sym.Type.AsFunction().IsEnumCons() {
			errs = append(errs, semanticError(errNotAnEnumCaseMsg, n.token))
			goto end
		}
		n.sym = sym

		// Ensure correct number of args
		cons := sym.Type.AsFunction()
		if len(cons.Params) != len(n.params) {
			errs = append(errs, semanticError2(errInvalidNumberArgsMsg, n.token, len(n.params), len(cons.Params)))
			goto end
		}

		// Assign types and check for redeclares
		n.symtab = symtab.Child()
		for i, arg := range n.params {
			sym := &Symbol{Name: arg.token.Val, Type: cons.Params[i], IsStack: true}
			if _, ok := n.symtab.Define(sym); ok {
				errs = append(errs, semanticError(errRedeclaredMsg, arg.token))
				continue
			}
			arg.sym = sym
			arg.typ = sym.Type
		}

		// Type check statements
		for _, stmt := range n.stmts {
			errs = append(errs, typeCheck(stmt, n.symtab, fn, debug)...)
		}
	case opNamedType, opFuncType, opArrayType:
		t, err := createType(symtab, n)
		if err != nil {
			errs = append(errs, err)
			goto end
		}
		// TODO: Is a 'dummy' symbol required?
		n.typ = t
	default:
		panic(fmt.Sprintf("Node type [%v] not processed during type check!", nodeTypes[n.op]))
	}

	// DEBUG
	if debug {
		printTypeInfo(n)
	}

end:
	return errs
}

func typeCheckTernary(n *Node, symtab *SymTab, fn *FunctionType, debug bool) []error {
	cond := n.left
	if errs := typeCheck(cond, symtab, fn, debug); !cond.hasType() {
		return errs
	}
	if !cond.typ.Is(Boolean) {
		return []error{ semanticError2(errMismatchedTypesMsg, cond.token, cond.typ, boolType) }
	}
	ifExpr := n.stmts[0]
	if errs := typeCheck(ifExpr, symtab, fn, debug); !ifExpr.hasType() {
		return errs
	}
	elseExpr := n.stmts[1]
	if errs := typeCheck(elseExpr, symtab, fn, debug); !elseExpr.hasType() {
		return errs
	}
	if !ifExpr.typ.Matches(elseExpr.typ) {
		return []error{ semanticError2(errMismatchedTypesMsg, elseExpr.token, elseExpr.typ, ifExpr.typ) }
	}
	n.typ = ifExpr.typ
	return nil
}

func typeCheckArrayLit(n *Node, symtab *SymTab, fn *FunctionType, debug bool) []error {
	if len(n.stmts) == 0 {
		return []error{ semanticError(errEmptyArrayLiteralMsg, n.token) }
	}
	for _, expr := range n.stmts {
		if errs := typeCheck(expr, symtab, fn, debug); !expr.hasType() {
			return errs
		}
		// Type of first element defines type for rest of elements
		if !expr.typ.Matches(n.stmts[0].typ) {
			return []error{ semanticError2(errMismatchedTypesMsg, expr.token, expr.typ, intType) }
		}
	}
	n.typ = &Type{Kind: Array, Data: &ArrayType{Elem: n.stmts[0].typ}}
	return nil
}

func typeCheckFor(n *Node, symtab *SymTab, fn *FunctionType, debug bool) (errs []error) {
	n.symtab = symtab.Child()
	errs = append(errs, typeCheck(n.right, symtab, fn, debug)...)
	if !n.right.hasType() {
		return errs
	}

	// 2 case - either an range expression or an array
	var varType *Type
	switch {
	case n.right.typ.Kind == Array:
		varType = n.right.typ.AsArray().Elem

	case n.right.Is(opRange):
		varType = n.right.typ

	default:
		errs = append(errs, semanticError2(errMismatchedTypesMsg, n.right.token, n.right.typ, "<array> or <range expression>"))
	}

	// Create & assign new symbol
	sym, ok := n.symtab.Define(NewStackSym(n.left.token.Val, varType))
	if ok {
		panic("Symbol already defined in empty scope?")
	}
	n.left.sym = sym
	n.left.typ = sym.Type

	// Typecheck body
	for _, stmt := range n.stmts {
		errs = append(errs, typeCheck(stmt, n.symtab, fn, debug)...)
	}
	return errs
}

func typeCheckFuncCall(n *Node, fnSymtab *SymTab, symtab *SymTab, fn *FunctionType, debug bool) (errs []error) {

	if len(n.stmts) > maxFnArgCount {
		errs = append(errs, semanticError2(errTooManyArgsMsg, n.token, n.token.Val, maxFnArgCount))
		return errs
	}

	// Typecheck function call source
	switch n.left.op {
	case opBlockFnDcl, opDot, opFuncCall, opArray:
		errs = append(errs, typeCheck(n.left, symtab, fn, debug)...)
	case opIdentifier:
		err := typeCheckIdentifier(n.left, symtab, true)
		if err != nil {
			errs = append(errs, err)
		}
	default:
		errs = append(errs, semanticError(errResolveFunctionMsg, n.left.token))
	}
	if !n.left.hasType() {
		return errs
	}

	// Type check type parameters
	for _, param := range n.params {
		errs = append(errs, typeCheck(param, symtab, fn, debug)...)
		if !param.hasType() {
			return errs
		}
	}

	// Type check args
	for _, arg := range n.stmts {
		if arg.hasType() {
			continue
		}
		switch arg.op {
		case opIdentifier:
			err := typeCheckIdentifier(arg, symtab, true)
			if err != nil {
				return append(errs, err)
			}
		default:
			errs = append(errs, typeCheck(arg, symtab, fn, debug)...)
		}
		if !arg.hasType() {
			return errs
		}
	}

	// SPECIAL CASE: Skip dealing with variadic functions as printf & debug are the only ones
	if n.left.token.Val == "printf" || n.left.token.Val == "debug" {
		s, _ := fnSymtab.Resolve(n.left.token.Val)
		n.left.sym = s
		n.typ = nothingType
		return errs
	}

	// SPECIAL CASE: Allow anything into the unsafe function
	if n.left.token.Val == "unsafe" {
		s, _ := fnSymtab.Resolve(n.left.token.Val)
		unsafe := s.Type.AsFunction()
		if len(n.stmts) != 3 {
			return append(errs, semanticError2(errInvalidNumberArgsMsg, n.left.token, len(n.stmts), len(unsafe.Params)))
		}
		n.left.sym = s
		n.typ = n.stmts[len(n.stmts)-1].typ
		return errs
	}

	// 2 cases, either the function call is a named call (global, parameter, etc) or is
	// an "anonymous" call from some expression evaluation (f(x)(y), etc)

	// Match results
	nf := n
	var returnType *Type
	var boundTypes map[*Type]*Type

	// TODO: Split into two functions. TypeCheckFnCallByType, TypeCheckFnCallBySymbol (*Node, *Type, map[*Type]*Type)
	if n.left.sym == nil {
		err, bound := matchFuncCallByType(n.left.typ, n)
		if err != nil {
			return append(errs, err)
		}
		nf = n.left
		returnType = n.left.typ.AsFunction().ret
		boundTypes = bound
	} else {
		s := n.left.sym
		match, bound, serrs := matchFuncCallBySymbol(s, n)
		if match == nil {
			if len(serrs) == 1 {
				return append(errs, serrs[0])
			}
			candidates := bytes.NewBufferString("")
			for x := s; x != nil; x = x.Next {
				if x.Type.Is(Function) {
					candidates.WriteString("	" + x.Describe() + "\n")
				}
			}
			return append(errs, semanticError2(errOverloadResolutionMsg, n.token, n.Describe(),
				candidates.String()))
		}
		n.left.sym = match
		returnType = match.Type.AsFunction().ret
		boundTypes = bound
	}

	// Attempt to convert from generic -> concrete type. If still generic, check that's allowed in this context
	returnType = reifyType(returnType, boundTypes)
	if returnType.Is(Parameter) && !fn.HasTypeParameter(returnType) {
		return append(errs, semanticError2(errTypeParameterNotBoundMsg, nf.token, returnType.AsParameter().Name))
	}
	n.typ = returnType
	return errs
}

func matchFuncCallBySymbol(f *Symbol, n *Node) (s *Symbol, types map[*Type]*Type, errs []error) {
	for s = f; s != nil; s = s.Next {
		err, bound := matchFuncCallByType(s.Type, n)
		if err == nil {
			return s, bound, nil
		}
		errs = append(errs, err)
	}
	return nil, nil, errs
}

func matchFuncCallByType(t *Type, n *Node) (error, map[*Type]*Type) {
	args := n.stmts
	if !t.Is(Function) {
		var argTypes []string
		for _, arg := range args {
			argTypes = append(argTypes, arg.typ.String())
		}
		return semanticError2(errMismatchedTypesMsg, n.token, t, fmt.Sprintf("fn(%v)", strings.Join(argTypes, ","))), nil
	}
	f := t.AsFunction()
	if len(args) != len(f.Params) {
		return semanticError2(errInvalidNumberArgsMsg, n.token, len(args), len(f.Params)), nil
	}
	types := n.params
	if len(types) != 0 && len(types) != len(f.Types) {
		return semanticError2(errInvalidNumberTypeArgsMsg, n.token, len(types), len(f.Types)), nil
	}
	bound := make(map[*Type]*Type)
	for i, t := range types {
		bound[f.Types[i]] = t.typ
	}
	argCheck:
	for i, arg := range args {
		param := f.Params[i]
		// Check each overloaded identifier for a match
		if arg.op == opIdentifier && arg.sym.Next != nil {
			for s := arg.sym; s != nil; s = s.Next {
				if s.Type.PolyMatch(param, bound) {
					arg.sym = s
					arg.typ = s.Type
					continue argCheck
				}
			}
			// Failed to find a match
			candidates := bytes.NewBufferString("")
			for s := arg.sym; s != nil; s = s.Next {
				candidates.WriteString(fmt.Sprintf("	%v\n", s.Describe()))
			}
			return semanticError2(errOverloadResolutionMsg, arg.token, reifyType(param, bound),
				candidates.String()), nil
		}

		// Match on declared type
		if !arg.typ.PolyMatch(param, bound) {
			return semanticError2(errMismatchedTypesMsg, arg.token, arg.typ, reifyType(param, bound)), nil
		}
	}
	return nil, bound
}

func reifyType(t *Type, bound map[*Type]*Type) *Type {
	if len(bound) == 0 {
		return t
	}
	switch {
	case t.Is(Array):
		return &Type{Kind: Array, Data: &ArrayType{Elem: reifyType(t.AsArray().Elem, bound)}}

	case t.Is(Function):
		f := t.AsFunction()
		var params []*Type
		for _, p := range f.Params {
			params = append(params, reifyType(p, bound))
		}
		returnType := reifyType(f.ret, bound)
		// TODO: Should Data be copied too?
		return &Type{Kind: Function, Data: &FunctionType{Kind: f.Kind, isVariadic: f.isVariadic, ret: returnType, Params: params, Data: f.Data}}

	case t.Is(Struct) || t.Is(Enum):
		panic("reifying structs & enums is not yet supported!")

	default:
		for k, tt := range bound {
			if k.Matches(t) {
				return tt
			}
		}
		return t
	}
}

func typeCheckIdentifier(n *Node, symtab *SymTab, allowAmbiguous bool) error {

	// If no symbol - try to find identifier declaration
	if n.sym == nil {
		sym, found := symtab.Resolve(n.token.Val)
		if !found {
			return semanticError(errUnknownVarMsg, n.token)
		}
		if sym.Next != nil && !allowAmbiguous {
			var types []string
			for s := sym; s != nil; s = s.Next {
				types = append(types, s.Type.String())
			}
			return semanticError2(errAmbiguousVarMsg, n.token, n.token.Val, strings.Join(types, "\n\t* "))
		}
		n.sym = sym
	}
	n.typ = n.sym.Type
	return nil
}

//---------------------------------------------------------------------------------------------------------------

func printTypeInfo(n *Node) {
	// TODO: Fix the type name printing!
	calculatedType := "<EMPTY>"
	if n.typ != nil {
		calculatedType = n.typ.String()
	}

	location := fmt.Sprintf("%v:%d:%d", n.token.File, n.token.Line, n.token.Pos)
	if n.token.File == "" {
		location = "<AST defined>"
	}
	symbolName := strings.Replace(n.token.Val, "%", "%%", -1) // Escape Go format strings
	if n.op != opLit {
		symbolName = "\"" + symbolName + "\""
	}

	// Dump type info
	fmt.Printf(debugTypeInfoFormat,
		console.Yellow, location, console.Disable,
		console.Red, fmt.Sprintf("%s(%s)", nodeTypes[n.op], symbolName), console.Disable,
		console.Green, calculatedType, console.Disable)
}

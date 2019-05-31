package main

import (
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
		child := symtab.Child()
		for _, stmt := range n.stmts {
			errs = append(errs, typeCheck(stmt, child, fn, debug)...)
		}

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
		child := symtab.Child()
		for _, stmt := range n.stmts {
			errs = append(errs, typeCheck(stmt, child, fn, debug)...)
		}

		// Type check next elseif case (if any)
		if right != nil {
			errs = append(errs, typeCheck(right, symtab, fn, debug)...)
		}

		// Does not promote type...

	case opElse, opBlock:
		// Type check body
		child := symtab.Child()
		for _, stmt := range n.stmts {
			errs = append(errs, typeCheck(stmt, child, fn, debug)...)
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


	case opAnd, opOr, opAdd, opMul, opSub, opDiv:
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
			s, _ = symtab.Define(&Symbol{ Name: n.token.Val, IsLiteral: true })
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
		errs = append(errs, typeCheckIdentifier(n, symtab, fn, debug)...)

	case opFuncCall:
		errs = append(errs, typeCheckFuncCall(n, symtab, symtab, fn, debug)...)

	case opGt, opLt, opEq:
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
			n.right = nil

			// Check if call is field _of_ struct or normal dot selection rules apply
			if left.typ.Is(Struct) && left.typ.AsStruct().HasField(right.token.Val) {
				n.stmts = right.stmts
				n.left = &Node{ op: opDot, token: lex.WithVal(n.token, "."), left: left, right:
					&Node{ op: opIdentifier, token: right.token }}
			} else {
				n.stmts = append([]*Node{n.left}, right.stmts...)
				n.left = nil
			}

			// Type check func call
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

			// Check we have a struct
			var strct *StructType
			if left.typ.Is(Struct) {
				strct = left.typ.AsStruct()
			} else if left.typ.IsFunction(Struct) {
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

			n.sym = right.sym
			n.typ = right.typ

		} else {
			// Unexpected type on right
			errs = append(errs, semanticError(errInvalidDotSelectionMsg, right.token))
			goto end
		}


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
		n.sym = left.sym
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
		sym, ok := symtab.Define(&Symbol{ Name: left.token.Val, IsStack: true })
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
			if !enum.HasMember(cons.sym.Type.AsFunction()){
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

		// TODO: Check all cases are handled or "remaining" keyword has been used

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
		if len(cons.Args) != len(n.params) {
			errs = append(errs, semanticError2(errInvalidNumberArgsMsg, n.token, len(n.params), len(cons.Args)))
			goto end
		}

		// Assign types and check for redeclares
		child := symtab.Child()
		n.symtab = child
		for i, arg := range n.params {
			sym := &Symbol{Name: arg.token.Val, Type: cons.Args[i], IsStack: true}
			if _, ok := child.Define(sym); ok {
				errs = append(errs, semanticError(errRedeclaredMsg, arg.token))
				continue
			}
			arg.sym = sym
			arg.typ = sym.Type
		}

		// Type check statements
		for _, stmt := range n.stmts {
			errs = append(errs, typeCheck(stmt, child, fn, debug)...)
		}

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

func typeCheckFuncCall(n *Node, fnSymtab *SymTab, symtab *SymTab, fn *FunctionType, debug bool) (errs []error) {

	left := n.left

	// Type check args
	for _, arg := range n.stmts {
		errs = append(errs, typeCheck(arg, symtab, fn, debug)...)
		if !arg.hasType() {
			return errs
		}
	}

	// SPECIAL CASE: Skip dealing with variadic functions as printf & debug are the only ones
	if n.token.Val == "printf" || n.token.Val == "debug" {
		s, _ := fnSymtab.Resolve(n.token.Val)
		n.sym = s
		n.typ = nothingType
		return errs
	}

	// 2 cases, either the function call is a named call (global, parameter, etc) or is
	// an "anonymous" call from some expression evaluation (f(x)(y), etc)

	if left != nil {

		// Subexpression yields function
		errs = append(errs, typeCheck(left, symtab, fn, debug)...)
		if !left.hasType() {
			return errs
		}

		// Check we have a function
		if !left.typ.Is(Function) {
			var types []string
			for _, arg := range n.stmts {
				types = append(types, arg.typ.String())
			}
			return append(errs, semanticError2(errMismatchedTypesMsg, n.token, left.typ, fmt.Sprintf("fn(%v)", strings.Join(types, ","))))
		}

		// Check correct number of args
		fn := left.typ.AsFunction()
		if len(n.stmts) != len(fn.Args) {
			return append(errs, semanticError2(errInvalidNumberArgsMsg, n.token, len(n.stmts), len(fn.Args)))
		}

		// Check all types match
		for i, arg := range fn.Args {
			if !n.stmts[i].typ.Matches(arg) {
				return append(errs, semanticError2(errMismatchedTypesMsg, n.stmts[i].token, n.stmts[i].typ, arg))
			}
		}
		n.typ = fn.ret

	} else {

		// Attempt to resolve symbol
		var match *Symbol
	loop:
		for s, _ := fnSymtab.Resolve(n.token.Val); s != nil; s = s.Next {

			// SPECIAL CASE: If symbol is ambiguous an error has already been reported and no type added so skip it
			if s.Type == nil {
				return errs
			}

			// Check is a function
			if !s.Type.Is(Function) {
				continue
			}

			// Check correct number of args
			fn := s.Type.AsFunction()
			if len(n.stmts) != len(fn.Args) {
				continue
			}

			// Check all types match
			for i, arg := range fn.Args {
				if !n.stmts[i].typ.Matches(arg) {
					continue loop
				}
			}

			// Match found
			match = s
			break
		}

		// Couldn't resolve function
		if match == nil {
			var types []string = nil
			for _, p := range n.stmts {
				types = append(types, p.typ.String())
			}
			return append(errs, semanticError2(errResolveFunctionMsg, n.token, fmt.Sprintf("%v(%v)", n.token.Val, strings.Join(types, ", "))))
		}

		// Finally set symbol on node
		n.sym = match
		n.typ = match.Type.AsFunction().ret
	}
	return errs
}

func typeCheckIdentifier(n *Node, symtab *SymTab, fn *FunctionType, debug bool) (errs []error) {

	// If no symbol - try to find identifier declaration
	if n.sym == nil {
		sym, found := symtab.Resolve(n.token.Val)
		if !found {
			return append(errs, semanticError(errUnknownVarMsg, n.token))
		}
		if sym.Next != nil {
			var types []string
			for s := sym; s != nil; s = s.Next {
				types = append(types, s.Type.String())
			}
			return append(errs, semanticError2(errAmbiguousVarMsg, n.token, n.token.Val, strings.Join(types, "\n\t* ")))
		}
		n.sym = sym
	}
	n.typ = n.sym.Type
	return errs
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
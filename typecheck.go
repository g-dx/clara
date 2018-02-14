package main

import (
	"fmt"
	"strings"
	"github.com/g-dx/clarac/console"
)

//---------------------------------------------------------------------------------------------------------------

func typeCheck(n *Node, body bool, fn *FunctionType, debug bool) (errs []error) {

	left := n.left
	right := n.right

	switch n.op {
	case opWhile:
		errs = append(errs, typeCheck(left, body, fn, debug)...)

		if !left.hasType() {
			goto end
		}

		if !left.typ.Is(Boolean) {
			errs = append(errs, semanticError2(errMismatchedTypesMsg, left.token, left.typ, boolType))
			goto end
		}

		// Type check body
		for _, stmt := range n.stmts {
			errs = append(errs, typeCheck(stmt, body, fn, debug)...)
		}

	case opIf, opElseIf:
		errs = append(errs, typeCheck(left, body, fn, debug)...)

		if !left.hasType() {
			goto end
		}

		if !left.typ.Is(Boolean) {
			errs = append(errs, semanticError2(errMismatchedTypesMsg, left.token, left.typ, boolType))
			goto end
		}

		// Type check body
		for _, stmt := range n.stmts {
			errs = append(errs, typeCheck(stmt, body, fn, debug)...)
		}

		// Type check next elseif case (if any)
		if right != nil {
			errs = append(errs, typeCheck(right, body, fn, debug)...)
		}

		// Does not promote type...

	case opElse:
		// Type check body
		for _, stmt := range n.stmts {
			errs = append(errs, typeCheck(stmt, body, fn, debug)...)
		}

		// Does not promote type...

	case opReturn:

		// Default to empty return
		rType := nothingType
		rToken := n.token

		// Check expression if any
		if left != nil {
			errs = append(errs, typeCheck(left, body, fn, debug)...)
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


	case opAnd, opOr, opAdd, opMul, opMin, opDiv:
		errs = append(errs, typeCheck(left, body, fn, debug)...)
		errs = append(errs, typeCheck(right, body, fn, debug)...)

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
		errs = append(errs, typeCheck(left, body, fn, debug)...)

		if !left.hasType() {
			goto end
		}

		if !left.typ.Is(Boolean) {
			errs = append(errs, semanticError2(errMismatchedTypesMsg, left.token, left.typ, boolType))
			goto end
		}
		n.typ = boolType


	case opNeg:
		errs = append(errs, typeCheck(left, body, fn, debug)...)

		if !left.hasType() {
			goto end
		}

		if !(left.typ.Is(Integer) || left.typ.Is(Byte)) {
			errs = append(errs, semanticError2(errMismatchedTypesMsg, left.token, left.typ, fmt.Sprintf("%v or %v", Integer, Byte)))
			goto end
		}
		n.typ = left.typ

	case opLit:
		n.typ = n.sym.Type

	case opIdentifier:
		// If no symbol - try to find identifier declaration
		if n.sym == nil {
			sym, found := n.symtab.Resolve(n.token.Val)
			if !found {
				errs = append(errs, semanticError(errUnknownVarMsg, n.token))
				goto end
			}
			if sym.Next != nil {
				var types []string
				for s := sym; s != nil; s = s.Next {
					types = append(types, s.Type.String())
				}
				errs = append(errs, semanticError2(errAmbiguousVarMsg, n.token, n.token.Val, strings.Join(types, "\n\t* ")))
				goto end
			}
			n.sym = sym
		}
		n.typ = n.sym.Type

	case opFuncCall:
		// Type check args
		for _, arg := range n.stmts {
			errs = append(errs, typeCheck(arg, body, fn, debug)...)
			if !arg.hasType() {
				goto end
			}
		}

		// SPECIAL CASE: Skip dealing with variadic functions as printf is the only one
		if n.token.Val == "printf" {
			s, _ := n.symtab.Resolve(n.token.Val)
			n.sym = s
			n.typ = nothingType
			return
		}

		// =============================================================================================================
		// Handle "anonymous" func call f(1, true)("<string>")() ... etc ...
		// =============================================================================================================

		// TODO: Unify the logic for anonymous and named functions
		if left != nil {

			// Subexpression will yield a function
			errs = append(errs, typeCheck(left, body, fn, debug)...)
			if !left.hasType() {
				goto end
			}

			// Check we have a function
			if !left.typ.Is(Function) {
				var types []string
				for _, arg := range n.stmts {
					types = append(types, arg.typ.String())
				}
				errs = append(errs, semanticError2(errMismatchedTypesMsg, n.token, left.typ, fmt.Sprintf("fn(%v)", strings.Join(types, ","))))
				goto end
			}

			// Check correct number of args
			fn := left.typ.AsFunction()
			if len(n.stmts) != len(fn.Args) {
				errs = append(errs, semanticError2(errInvalidNumberArgsMsg, n.token, len(n.stmts), len(fn.Args)))
				goto end
			}

			// Check all types match
			for i, arg := range fn.Args {
				if !n.stmts[i].typ.Matches(arg) {
					errs = append(errs, semanticError2(errMismatchedTypesMsg, n.stmts[i].token, n.stmts[i].typ, arg))
					goto end
				}
			}
			n.typ = fn.ret
			return
		}

		// =============================================================================================================
		// Handle "named" function call x().y().z() ... etc ...
		// =============================================================================================================

		// Attempt to resolve symbol
		var match *Symbol
	loop:
		for s, _ := n.symtab.Resolve(n.token.Val); s != nil; s = s.Next {

			// SPECIAL CASE: If symbol is ambiguous an error has already been reported and no type added so skip it
			if s.Type == nil {
				goto end
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
			errs = append(errs, semanticError2(errResolveFunctionMsg, n.token, fmt.Sprintf("%v(%v)", n.token.Val, strings.Join(types, ", "))))
			goto end
		}

		// Finally set symbol on node
		n.sym = match
		n.typ = match.Type.AsFunction().ret

	case opGt, opLt, opEq:
		errs = append(errs, typeCheck(left, body, fn, debug)...)
		errs = append(errs, typeCheck(right, body, fn, debug)...)

		if !left.hasType() || !right.hasType() {
			goto end
		}
		if !left.typ.Matches(right.typ) {
			errs = append(errs, semanticError2(errMismatchedTypesMsg, left.token, left.typ, right.typ))
			goto end
		}
		n.typ = boolType

	case opFuncDcl:

		fn := n.sym.Type.AsFunction()
		if !body {

			// Check if we need to configure function types
			// TODO: Seriously consider disallowing use before declaration of types. It would simplify things a lot...
			addArgs := len(fn.Args) == 0

			// Type check params
			for _, param := range n.params {
				errs = append(errs, typeCheck(param, body, fn, debug)...)
				if !param.hasType() {
					goto end
				}
				if addArgs {
					fn.Args = append(fn.Args, param.typ)
				}
			}
		}

		if body {

			// Type check stmts
			for _, stmt := range n.stmts {
				errs = append(errs, typeCheck(stmt, body, fn, debug)...)
			}

			n.typ = n.sym.Type

			// Check for termination
			if !fn.ret.Is(Nothing) && !fn.IsExternal && !fn.isConstructor && !n.isTerminating() {
				errs = append(errs, semanticError(errMissingReturnMsg, n.token))
			}
		}

	case opDot:
		errs = append(errs, typeCheck(left, body, fn, debug)...)

		if !left.hasType() {
			goto end
		}

		// Handle func call on right
		if right.op == opFuncCall {

			// Rewrite to func call
			n.op = opFuncCall
			n.token = right.token
			n.symtab = right.symtab
			n.stmts = append([]*Node{n.left}, right.stmts...)
			n.left = nil
			n.right = nil

			// Type check func call
			errs = append(errs, typeCheck(n, body, fn, debug)...)

			// Handle field access on right
		} else if right.op == opIdentifier {

			// SPECIAL CASE: Fudge strings to give them a special int field "length" at offset 0
			if (left.sym.Type.Is(Array) || left.sym.Type.Is(String)) && right.token.Val == "length" {
				right.sym = &Symbol{Name: "length", Addr: 0, Type: intType}
				right.typ = right.sym.Type
				n.typ = right.typ
				return errs
			}

			// Check we have a struct
			var strct *StructType
			if left.sym.Type.Is(Struct) {
				strct = left.sym.Type.AsStruct()
			} else if left.sym.Type.IsFunction(Struct) {
				strct = left.sym.Type.AsFunction().ret.AsStruct()
			} else {
				errs = append(errs, semanticError(errNotStructMsg, left.token))
				goto end
			}

			// Check field exists in struct
			sym, offset := strct.Offset(right.token.Val)
			if sym == nil {
				errs = append(errs, semanticError(errStructHasNoFieldMsg, right.token, strct.Name))
				goto end
			}

			// Set field offset
			// TODO: This whole process process isn't necessary because when we build a StructType we can set the offsets
			// for each symbol
			sym.Addr = offset

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
		errs = append(errs, typeCheck(left, body, fn, debug)...)
		errs = append(errs, typeCheck(right, body, fn, debug)...)

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
		errs = append(errs, typeCheck(right, body, fn, debug)...)

		if !right.hasType() {
			goto end
		}

		// Check we have identifier on left
		// TODO: Should we attempt to type check left to get more information?
		if left.op != opIdentifier {
			errs = append(errs, semanticError2(errUnexpectedAssignMsg, left.token))
		}

		// Left gets type of right
		left.sym.Type = right.typ
		left.typ = right.typ

		// Does not promote type...

	case opAs:
		errs = append(errs, typeCheck(right, body, fn, debug)...)
		errs = append(errs, typeCheck(left, body, fn, debug)...)

		if !right.hasType() || !left.hasType() {
			goto end
		}

		// Check left is addressable
		if !left.isAddressable() {
			errs = append(errs, semanticError2(errNotAddressableAssignMsg, left.token))
			goto end
		}

		// Check types in assignment
		if !left.typ.Matches(right.typ) {
			errs = append(errs, semanticError2(errMismatchedTypesMsg, right.token, right.typ, left.typ))
			goto end
		}

		// Does not promote type...

	case opRoot:
		panic("Type check called on root node of AST")

	case opError:
		// TODO: Decide what to do here...
		goto end

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
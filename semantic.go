package main
import (
	"errors"
	"fmt"
	"github.com/g-dx/clarac/lex"
	"strings"
)

//
// Functions for various semantic passes
//

const (
	errUndefinedMsg = "%v:%d:%d: error, '%v' undefined"
	errNotFuncMsg = "%v:%d:%d: error, '%v' is not a function"
	errTooManyArgsMsg = "%v:%d:%d: error, too many arguments to call '%v'"
	errTooFewArgsMsg = "%v:%d:%d: error, not enough arguments to call '%v'"
	errUnknownTypeMsg = "%v:%d:%d: error, unknown type '%v'"
	errUnknownVarMsg = "%v:%d:%d: error, no declaration for identifier '%v' found"
	errStructNamingLowerMsg = "%v:%d:%d: error, struct names must start with a lowercase letter, '%v'"
	errNotStructMsg = "%v:%d:%d: error, '%v' is not a struct"
	errStructHasNoFieldMsg = "%v:%d:%d: error, field '%v' is not defined in struct '%v'"
	errInvalidDotSelectionMsg = "%v:%d:%d: error '%v', expected field or function call"
	errMismatchedTypesMsg = "%v:%d:%d: mismatched types '%v' and '%v'"
	errNonIntegerIndexMsg = "%v:%d:%d: error, found type '%v', array index must be integer"

	// Debug messages
	debugVarTypeMsg = "%v:%d:%d: debug, %v: identifier '%v' assigned type '%v'\n"
)

var fn *FunctionType // Function which is currently being type checked

func typeCheck(root *Node, symtab *SymTab, n *Node) error {

	left := n.left
	right := n.right

	switch n.op {
	case opLit:
		n.typ = n.sym.Type

	case opIdentifier:
		// If no symbol - try to find identifier declaration
		if n.sym == nil {
			def, ok := n.symtab.Resolve(n.token.Val)
			if !ok {
				return semanticError(errUnknownVarMsg, n.token)
			}
			n.sym = def
		}
		n.typ = n.sym.Type

	case opFuncCall:
		// Check exists
		s, found := symtab.Resolve(n.token.Val) // TODO: Should this be p.symtab?
		if !found {
			// Undefined
			return semanticError(errUndefinedMsg, n.token)
		}

		// Check is a function
		if !s.Type.Is(Function) {
			return semanticError(errNotFuncMsg, n.token)
		}

		// Check for too few args
		fn := s.Type.AsFunction()
		if len(n.stmts) < fn.ArgCount {
			return semanticError(errTooFewArgsMsg, n.token)
		}

		// Check for too many
		if len(n.stmts) > fn.ArgCount && !fn.isVariadic {
			return semanticError(errTooManyArgsMsg, n.token)
		}

		// TODO: type check args to function signature!

		// Finally set symbol on node
		n.sym = s
		n.typ = fn.ret

	case opAdd, opMin, opMul, opDiv :
		if left.typ == nil || right.typ == nil {
			return nil
		}

		// TODO: Not checking string & int distinction here...
		if left.typ.Kind == right.typ.Kind {
			n.typ = left.typ
		}

	case opOr, opAnd:
		if left.typ == nil || right.typ == nil {
			return nil
		}
		if !left.typ.Is(Boolean) || !right.typ.Is(Boolean) {
			// TODO: Need to know which side is wrong so we can output a better position
			return semanticError2(errMismatchedTypesMsg, left.token, left.typ.Kind, right.typ.Kind)
		}
		n.typ = boolType

	case opGt, opLt, opEq:
		if left.typ == nil || right.typ == nil {
			return nil
		}
		if left.typ.Kind != right.typ.Kind {
			return semanticError2(errMismatchedTypesMsg, left.token, left.typ.Kind, right.typ.Kind)
		}
		n.typ = boolType

	case opIf, opElseIf, opNot:
		if left.typ == nil {
			return nil
		}
		if !left.typ.Is(Boolean) {
			// TODO: More specific message for if statement?
			return semanticError2(errMismatchedTypesMsg, left.token, left.typ.Kind, boolType.Kind)
		}

	case opReturn:
		// "Empty" return
		if left == nil {
			n.typ = nothingType
			return nil
		}
		if left.typ == nil {
			return nil
		}
		if !fn.ret.Is(left.typ.Kind) {
			return semanticError2(errMismatchedTypesMsg, left.token, left.typ.Kind, fn.ret.Kind)
		}
		n.typ = n.left.typ

	case opArray:
		if left.typ == nil {
			return nil
		}
		if !right.typ.Is(Integer) {
			return semanticError2(errNonIntegerIndexMsg, right.token, right.typ.Kind)
		}
		n.typ = left.typ

		// SPECIAL CASE: If the left type is a string, array access yields a byte
		if left.typ.Is(String) {
			n.typ = byteType
		}

	case opDot:
		if right.typ == nil {
			return nil
		}
		n.typ = right.typ

	case opRoot, opError, opElse:

	case opFuncDcl:
		n.typ = n.sym.Type

		// TODO:
		// - Last statement should be a return expression if this function returns something
		// - All return expression should return the correct type

	default:
		panic(fmt.Sprintf("Node type [%v] not processed during type check!", nodeTypes[n.op]))
	}

	// DEBUG
	printTypeInfo(n)
	return nil
}

func printTypeInfo(n *Node) {
	// TODO: Fix the type name printing!
	calculatedType := "<EMPTY>"
	if n.typ != nil {
		calculatedType = n.typ.String()
	}

	// Dump type info
	fmt.Printf(debugVarTypeMsg,
		n.token.File,
		n.token.Line,
		n.token.Pos,
		nodeTypes[n.op],
		strings.Replace(n.token.Val, "%", "%%", -1), // Escape Go format strings
		calculatedType)
}

func generateStructConstructors(root *Node, symtab *SymTab, n *Node) error {
	if n.op == opStruct {

		name := n.token.Val
		firstLetter := name[:1]

		// Check struct begins with lowercase
		if strings.ToUpper(firstLetter) == firstLetter {
			return semanticError(errStructNamingLowerMsg, n.token)
		}

		// Create name
		constructorName := strings.ToUpper(firstLetter) + name[1:]

		// Create & define symbol
		fnSym := &Symbol{ Name: constructorName, Type: &Type{ Kind: Function, Data:
			&FunctionType{ Name: constructorName, ArgCount: len(n.stmts), isConstructor: true, ret: n.sym.Type, args: n.symtab, }}}
		root.symtab.Define(fnSym)

		// Add AST node
		root.Add(&Node{token:&lex.Token{Val : constructorName}, op:opFuncDcl, params: n.stmts, symtab: n.symtab, sym: fnSym})
	}
	return nil
}

func rewriteDotSelection(root *Node, symtab *SymTab, n *Node) error {
	if n.op == opDot {

		// Process children
		left := n.left
		right := n.right

		// Resolve type on left if not set
		if left.sym == nil {
			sym, found := left.symtab.Resolve(left.token.Val)
			if !found {
				return semanticError(errUnknownVarMsg, left.token)
			}
			left.sym = sym
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

		// Handle field access on right
		} else if right.op == opIdentifier {

			// SPECIAL CASE: Fudge strings to give them a special int field "length" at offset 0
			// TODO: Add arrays here too when required
			if left.sym.Type.Is(String) && right.token.Val == "length" {
				right.sym = &Symbol{ Name: "length", Addr: 0, Type: intType }
				right.typ = right.sym.Type
				n.typ = right.typ
				return nil
			}

			// Check we have a struct
			if !left.sym.Type.Is(Struct) {
				return semanticError(errNotStructMsg, left.token)
			}

			// Check field exists in struct
			strct := left.sym.Type.AsStruct()
			sym, offset := strct.Offset(right.token.Val)
			if sym == nil {
				return semanticError(errStructHasNoFieldMsg, right.token, strct.Name)
			}

			// Set field offset
			// TODO: This whole process process isn't necessary because when we build a StructType we can set the offsets
			// for each symbol
			sym.Addr = offset

			// Set right symbol and set parent as right
			right.sym = sym
			n.sym = right.sym

		// Unexpected
		} else {
			// TODO: Ideally we do not want to process and more field access or function calls
			return semanticError(errInvalidDotSelectionMsg, right.token)
		}
	}
	return nil
}

func returnTypeCheck(root *Node, symtab *SymTab, n *Node) error {
	switch n.op {
	case opFuncDcl:
	case opReturn:
	}
	return nil
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

func walk(root *Node, symtab *SymTab, n *Node, visit func(*Node, *SymTab, *Node) error) (errs []error) {

	// Depth First Search

	// Visit left and right
	if n.left != nil {
		errs = append(errs, walk(root, symtab, n.left, visit)...)
	}
	if n.right != nil {
		errs = append(errs, walk(root, symtab, n.right, visit)...)
	}

	// Visit parameters
	for _, param := range n.params {
		if param != nil {
			errs = append(errs, walk(root, symtab, param, visit)...)
		}
	}

	// Visit statement
	for _, stat := range n.stmts {
		if stat != nil {
			errs = append(errs, walk(root, symtab, stat, visit)...)
		}
	}

	// Visit node
	if err := visit(root, symtab, n); err != nil {
		errs = append(errs, err)
	}
	return
}



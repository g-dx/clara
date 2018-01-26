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
	errUndefinedMsg            = "%v:%d:%d: error, '%v' undefined"
	errNotFuncMsg              = "%v:%d:%d: error, '%v' is not a function"
	errTooManyArgsMsg          = "%v:%d:%d: error, too many arguments to call '%v'"
	errUnknownTypeMsg          = "%v:%d:%d: error, unknown type '%v'"
	errUnknownVarMsg           = "%v:%d:%d: error, no declaration for identifier '%v' found"
	errStructNamingLowerMsg    = "%v:%d:%d: error, struct names must start with a lowercase letter, '%v'"
	errConstructorOverrideMsg  = "%v:%d:%d: error, function name '%v' is reserved for struct constructor"
	errNotStructMsg            = "%v:%d:%d: error, '%v' is not a struct"
	errStructHasNoFieldMsg     = "%v:%d:%d: error, field '%v' is not defined in struct '%v'"
	errInvalidDotSelectionMsg  = "%v:%d:%d: error '%v', expected field or function call"
	errInvalidOperatorTypeMsg  = "%v:%d:%d: type '%v' invalid for operator '%v'"
	errMismatchedTypesMsg      = "%v:%d:%d: mismatched types, got '%v', wanted '%v'"
	errCannotApplyArgsMsg      = "%v:%d:%d: function '%v' cannot be applied to '(%v)'"
	errNotEnoughArgsMsg        = "%v:%d:%d: not enough arguments to call function '%v'"
	errNonIntegerIndexMsg      = "%v:%d:%d: error, found type '%v', array index must be integer"
	errUnexpectedAssignMsg     = "%v:%d:%d: error, left hand side of assignment must be identifier"
	errNotAddressableAssignMsg = "%v:%d:%d: error, left hand side of assignment is not addressable"
	errMissingReturnMsg 	   = "%v:%d:%d: error, missing return for function '%v'"

	// Debug messages
	debugTypeInfoFormat = "⚫ %s%-60s%s %s%-30s%s ⇨ %s%s%s\n"
)

//---------------------------------------------------------------------------------------------------------------

type OperatorTypes map[int][]TypeKind

var operatorTypes = OperatorTypes {
	opAdd: { Integer, Byte },
	opMin: { Integer, Byte },
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

		// Ensure there are no other function definitions with this name
		if _, found := root.symtab.Resolve(constructorName); found {
			var n *Node
			for _, x := range root.stmts {
				if x.op == opFuncDcl && x.token.Val == constructorName {
					n = x
					break
				}
			}
			return semanticError(errConstructorOverrideMsg, n.token)
		}

		// Collect struct field symbols
		var args []*Symbol
		for _, field := range n.stmts {
			args = append(args, field.sym)
		}

		// Create & define symbol
		fnSym := &Symbol{ Name: constructorName, Type: &Type{ Kind: Function, Data:
			&FunctionType{ Name: constructorName, Args: args, isConstructor: true, ret: n.sym.Type, }}}
		root.symtab.Define(fnSym)

		// Add AST node
		root.Add(&Node{token:&lex.Token{Val : constructorName}, op:opFuncDcl, params: n.stmts, symtab: n.symtab, sym: fnSym})
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



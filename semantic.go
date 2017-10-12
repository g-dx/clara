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
	errStructNotFoundMsg = "%v:%d:%d: error, struct '%v' not found"
	errStructHasNoFieldMsg = "%v:%d:%d: error, struct '%v' has no field '%v'"

	// Debug messages
	debugVarTypeMsg = "%v:%d:%d: debug, identifier '%v' assigned type '%v'\n"
)

func resolveIdentifierTypes(root *Node, symtab *SymTab, n *Node) error {
	if n.op == opIdentifier {

		// If no symbol - try to find variable declaration
		if n.sym == nil {
			def, ok := n.symtab.Resolve(symVar, n.token.Val)
			if !ok {
				return semanticError(errUnknownVarMsg, n.token)
			}
			n.sym = def
		}

		// If symbol has no type and the parser recorded one - check symbol table
		if idSym, ok := n.sym.(*IdentSymbol); ok && idSym.typ == nil && n.typ != nil {
			typ, ok := n.symtab.Resolve(symType, n.typ.Val)
			if !ok {
				return semanticError(errUnknownTypeMsg, n.typ)
			}
			idSym.typ = typ.(*TypeSymbol)

			// DEBUG
			fmt.Printf(fmt.Sprintf(debugVarTypeMsg, n.token.File, n.token.Line, n.token.Pos, n.token.Val, idSym.typ.val))
		}
	}
	return nil
}

func generateStructConstructors(root *Node, symtab *SymTab, n *Node) error {
	if n.op == opStruct {

		name := n.token.Val
		firstLetter := name[:1]

		// Check struct begins with lowercase
		if strings.ToUpper(firstLetter) == firstLetter {
			return semanticError(errStructNamingLowerMsg, n.token)
		}

		// Get symbol
		typ, err := resolveTypeSym(n.symtab, n.token, errUnknownTypeMsg)
		if err != nil {
			panic(err) // Should not happen! Struct type symbol should have been added during parse...
		}

		// Create name
		constructorName := strings.ToUpper(firstLetter) + name[1:]

		// Create & define symbol
		fnSym := &IdentSymbol{val: constructorName, typ2: &Type{ Kind: Function, Data:
			&FunctionType{ Name: constructorName, ArgCount: len(n.stmts), isConstructor: true, ret: typ, args: n.symtab, }}}
		root.symtab.Define(fnSym)

		// Add AST node
		root.Add(&Node{token:&lex.Token{Val : constructorName}, op:opFuncDcl, params: n.stmts, symtab: n.symtab, sym: fnSym})
	}
	return nil
}

func rewriteDotFuncCalls(root *Node, symtab *SymTab, n *Node) error {
	if n.op == opDot && n.right.op == opFuncCall {
		// Configure this node to be a func call. Must be careful here to copy across all state required for a func call!
		n.op = opFuncCall
		n.token = n.right.token
		n.symtab = n.right.symtab
		n.stmts = append([]*Node{n.left}, n.right.stmts...)

		// Clear children
		n.left = nil
		n.right = nil
	}
	return nil
}

func configureFieldAccess(root *Node, symtab *SymTab, n *Node) error {
	if n.op == opDot && n.right.op == opIdentifier {

		// Determine struct type on left
		typName := n.left.sym.(*IdentSymbol).typ.name()
		strct, ok := n.symtab.Resolve(symVar, typName)
		if !ok {
			// TODO: Currently this block will never trigger because if unknown types are caught and reported earlier
			return semanticError(errStructNotFoundMsg, n.left.token)
		}

		// Find field in struct
		strctSym := strct.(*IdentSymbol).Type().AsStruct()
		offset := strctSym.Offset(n.right.sym.(*IdentSymbol))
		if offset == -1 {
			// TODO: Currently this block will never trigger because if unknown type are caught and reported earlier
			return semanticError2(errStructHasNoFieldMsg, n.right.token, strct.name(), n.right.token.Val)
		}

		// Set field offset
		// TODO: When structs can have structs inside we need to set the type symbol on opDot node correctly
		n.right.sym.(*IdentSymbol).addr = offset

		// Get type symbol for struct
		ts, found := n.symtab.Resolve(symType, typName)
		if !found {
			panic(fmt.Sprintf("Struct '%v' has no corresponding type symbol?", typName)) // This should not be possible!
		}

		// Add width information which is required during codegen
		ts.(*TypeSymbol).width = strctSym.Width
	}
	return nil
}

func resolveFnCall(root *Node, symtab *SymTab, n *Node) (error) {

	if n.op == opFuncCall {
		// Check exists
		s, found := symtab.Resolve(symVar, n.token.Val)
		if !found {
			// Undefined
			return semanticError(errUndefinedMsg, n.token)
		}

		// Check is a function
		id, ok := s.(*IdentSymbol)
		if !ok {
			return semanticError(errNotFuncMsg, n.token)
		}
		fn := id.typ2.AsFunction()

		// Check for too few args
		if len(n.stmts) < fn.ArgCount {
			return semanticError(errTooFewArgsMsg, n.token)
		}

		// Check for too many
		if len(n.stmts) > fn.ArgCount && !fn.isVariadic {
			return semanticError(errTooManyArgsMsg, n.token)
		}

		// Finally set symbol on node
		n.sym = s
	}

	// Check args
	return nil
}

func resolveTypeSym(tab *SymTab, t *lex.Token, errMsg string) (*TypeSymbol, error) {
	s, found := tab.Resolve(symType, t.Val)
	if !found {
		return nil, semanticError(errMsg, t)
	}
	ts, ok := s.(*TypeSymbol)
	if !ok {
		panic(fmt.Sprintf("Expected *TypeSymbol: %v", symTypes[s.kind()]))
	}
	return ts, nil
}

func semanticError(msg string, t *lex.Token) error {
	return errors.New(fmt.Sprintf(msg,
		t.File,
		t.Line,
		t.Pos,
		t.Val))
}

func semanticError2(msg string, t *lex.Token, vals ...string) error {
	return errors.New(fmt.Sprintf(msg,
		t.File,
		t.Line,
		t.Pos,
		vals))
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



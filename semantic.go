package main
import (
	"errors"
	"fmt"
)

//
// Functions for various semantic passes
//

const (
	errUndefinedMsg = "%v:%d:%d: error, '%v' undefined"
	errNotFuncMsg = "%v:%d:%d: error, '%v' is not a function"
	errTooManyArgsMsg = "%v:%d:%d: error, too many arguments to call '%v'"
	errTooFewArgsMsg = "%v:%d:%d: error, not enough arguments to call '%v'"
)

func resolveFnCall(symtab SymTab, n *Node) (error) {

	if n.op == opFuncCall {
		// Check exists
		s, found := symtab.Resolve(symFnDecl, n.token.Val)
		if !found {
			// Undefined
			return semanticError(errUndefinedMsg, n)
		}

		// Check is a function
		fn, ok := s.(*Function)
		if !ok {
			return semanticError(errNotFuncMsg, n)
		}

		// Check for too few args
		if len(n.stats) < fn.argCount() {
			return semanticError(errTooFewArgsMsg, n)
		}

		// Check for too many
		if len(n.stats) > fn.argCount() && !fn.isVariadic {
			return semanticError(errTooManyArgsMsg, n)
		}

		// Finally set symbol on node
		n.sym = s
	}

	// Check args
	return nil
}
func semanticError(msg string, n *Node) error {
	return errors.New(fmt.Sprintf(msg,
		n.token.File,
		n.token.Line,
		n.token.Pos,
		n.token.Val))
}

func walk(symtab SymTab, n *Node, visit func(SymTab, *Node) error) (errs []error) {

	// Visit node
	if err := visit(symtab, n); err != nil {
		errs = append(errs, err)
	}
	// Visit children
	for _, stat := range n.stats {
		if stat != nil {
			errs = append(errs, walk(symtab, stat, visit)...)
		}
	}
	return
}



package main
import (
	"errors"
	"fmt"
)

//
// Functions for various semantic passes
//

func resolveFnCall(symtab SymTab, n *Node) (err error) {

	if n.op == opFuncCall {
		// Check exists
		s, found := symtab.Resolve(symFnDecl, n.token.val)
		if !found {
			// Undefined
			err = errors.New(fmt.Sprintf(errUndefinedMsg,
				n.token.file,
				n.token.line,
				n.token.pos,
				n.token.val))
			return err
		}

		// Check is a function
		fn, ok := s.(*Function)
		if !ok {
			err = errors.New(fmt.Sprintf(errNotFuncMsg,
				n.token.file,
				n.token.line,
				n.token.pos,
				n.token.val))
			return err
		}

		// Check arg count
		// TODO: Return the position of the wrong argument - not the function
		if fn.argCount() != len(n.stats) {
			err = errors.New(fmt.Sprintf(errArgCountMsg,
				n.token.file,
				n.token.line,
				n.token.pos,
				n.token.val))
			return err
		}

		// Finally set symbol on node
		n.sym = s
	}

	// Check args
	return err
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



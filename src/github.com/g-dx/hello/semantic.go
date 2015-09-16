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
		s := symtab.Resolve(n.token.val)
		if s == nil {
			// Undefined
			err = errors.New(fmt.Sprintf(errUndefinedMsg,
				n.token.line,
				n.token.pos,
				n.token.val))

		} else if fn, ok := s.(*BuiltInFunction); ok { // TODO: We only have functions!
			// Wrong arg count
			// TODO: Return the position of the wrong argument - not the function
			if fn.argCount() != len(n.stats) {
				err = errors.New(fmt.Sprintf(errArgCountMsg,
					n.token.line,
					n.token.pos,
					n.token.val))
			}
		}
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
		errs = append(errs, walk(symtab, stat, visit)...)
	}
	return
}



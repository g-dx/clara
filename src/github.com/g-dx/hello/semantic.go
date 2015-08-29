package main
import (
	"errors"
	"fmt"
)

//
// Functions for various semantic passes
//

func resolveFnCall(symtab SymTab, n *Node) (err error) {
	if n.op == opFuncCall && symtab.Resolve(n.token.val) == nil{
		// Undefined
		err = errors.New(fmt.Sprintf(errUndefinedMsg,
			n.token.line,
			n.token.pos,
			n.token.val))
	}
	return err
}

func walk(symtab SymTab, n *Node, visit func(SymTab, *Node) error) (errs []error) {

	// Visit node
	err := visit(symtab, n)
	if err != nil {
		errs = append(errs, err)
	}
	// Visit children
	for _, stat := range n.stats {
		errs = append(errs, walk(symtab, stat, visit)...)
	}
	return
}



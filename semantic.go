package main
import (
	"errors"
	"fmt"
	"github.com/g-dx/clarac/lex"
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
	errUnknownVarMsg = "%v:%d:%d: error, unknown var '%v'"
)

// TODO: When code like `type Person record [firstName: string, lastName: string]` is added symbol resolution _could_ be done during parse
func resolveVariables(symtab *SymTab, n *Node) error {
	var stab *SymTab
	if n.op == opFuncDcl || n.op == opFuncCall {

		// Record current symtab
		var nodes []*Node
		// TODO: Double check that this will ensure the current symtab is available when resolve fn calls!
		if n.op == opFuncDcl {
			stab = n.sym.(*Function).args
			nodes = n.args
			fmt.Printf("Resolving function Declaration: %v\n", n.token.Val)
		} else {
			nodes = n.stats
			stab = n.symtab
			fmt.Printf("Resolving function Call: %v\n", n.token.Val)
		}

		if stab != nil {
			fmt.Printf("Symbols:\n")
			for _, v := range stab.symbols {
				fmt.Printf(" @ %v : %v\n", symTypes[v.kind()], v)
			}
		}

		for _, arg := range nodes {

			// Only interested in resolving types for identifiers
			if arg.op == opIdentifier {

				fmt.Printf(" -- var: %v\n", arg.token.Val)
				// 1. Lookup var if none set
				if arg.sym == nil {
					v, found := stab.Resolve(symVar, arg.token.Val)
					if !found {
						return semanticError(errUnknownVarMsg, arg.token)
					}
					arg.sym = v
				}

				// 2. Lookup type if none set
				if arg.typ != nil {
					fmt.Printf(" -- type: %v\n", arg.typ.Val)
				}

				if vr, ok := arg.sym.(*VarSymbol); ok {
					if vr.typ == nil {
						t, found := stab.Resolve(symType, arg.typ.Val)
						if !found {
							return semanticError(errUnknownTypeMsg, arg.typ)
						}

						// Finally set symbol on node
						//if v, ok := arg.sym.(*VarSymbol); ok {
						if t, ok := t.(*TypeSymbol); ok {
							vr.typ = t
							fmt.Printf(" --- Set type: %v\n", t.val)
						} else {
							panic(fmt.Sprintf("Symtab should have returned type: %v", symTypes[t.kind()]))
						}
						//} else {
						//	panic(fmt.Sprintf("Argument without var symbol: %v, %v", n.token.Val, symTypes[v.kind()]))
						//}
					}
				}
			}
		}

		// TODO: Check for identifier clashes...
	}
	return nil
}

func resolveFnCall(symtab *SymTab, n *Node) (error) {

	if n.op == opFuncCall {
		// Check exists
		s, found := symtab.Resolve(symFnDecl, n.token.Val)
		if !found {
			// Undefined
			return semanticError(errUndefinedMsg, n.token)
		}

		// Check is a function
		fn, ok := s.(*Function)
		if !ok {
			return semanticError(errNotFuncMsg, n.token)
		}

		// Check for too few args
		if len(n.stats) < fn.argCount() {
			return semanticError(errTooFewArgsMsg, n.token)
		}

		// Check for too many
		if len(n.stats) > fn.argCount() && !fn.isVariadic {
			return semanticError(errTooManyArgsMsg, n.token)
		}

		// Finally set symbol on node
		n.sym = s
	}

	// Check args
	return nil
}
func semanticError(msg string, t *lex.Token) error {
	return errors.New(fmt.Sprintf(msg,
		t.File,
		t.Line,
		t.Pos,
		t.Val))
}

func walk(symtab *SymTab, n *Node, visit func(*SymTab, *Node) error) (errs []error) {

	// Visit node
	if err := visit(symtab, n); err != nil {
		errs = append(errs, err)
	}
	// TODO: What about expressions? Arg lists?
	// Visit children
	for _, stat := range n.stats {
		if stat != nil {
			errs = append(errs, walk(symtab, stat, visit)...)
		}
	}
	return
}



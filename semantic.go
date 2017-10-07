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
	if n.op == opFuncDcl || n.op == opFuncCall || n.op == opIdentifier {

		// Record current symtab
		var nodes []*Node
		// TODO: Double check that this will ensure the current symtab is available when resolve fn calls!
		if n.op == opFuncDcl {
			stab = n.sym.(*Function).args
			nodes = n.params
			fmt.Printf("Resolving function Declaration: %v\n", n.token.Val)
		} else {
			nodes = n.stmts
			stab = n.symtab
			fmt.Printf("Resolving function Call: %v\n", n.token.Val)
		}

		if stab != nil {
			fmt.Printf("Symbols:\n")
			for _, v := range stab.symbols {
				fmt.Printf(" @ %v : %v\n", symTypes[v.kind()], v)
			}
		}

		// Resolve function return type (if it declares one)
		if n.op == opFuncDcl && n.typ != nil {

			// Lookup
			ts, err := resolveTypeSym(stab, n.typ, errUnknownTypeMsg)
			if err != nil {
				return err
			}

			// Set symbol
			n.sym.(*Function).ret = ts
			fmt.Printf(" -- type: %v\n", n.typ.Val)
		}

		// Resolve variables for identifiers
		if n.op == opIdentifier {
			v, found := stab.Resolve(symVar, n.token.Val)
			if !found {
				return semanticError(errUnknownVarMsg, n.token)
			}
			n.sym = v
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

						// Lookup
						ts, err := resolveTypeSym(stab, arg.typ, errUnknownTypeMsg)
						if err != nil {
							return err
						}

						// Finally set symbol on node
						vr.typ = ts
						fmt.Printf(" --- Set type: %v\n", vr.typ.val)
					}
				}
			}
		}

		// TODO: Check for identifier clashes...
	}
	return nil
}
func rewriteDotSelections(symtab *SymTab, n *Node) error {
	if n.op == opDot {

		// We only support function calls at present. TODO: When structs and fields get added this will need to change!
		if n.right.op != opFuncCall {
				return nil // This will (currently) be reported as unknown identifier
		}

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
		if len(n.stmts) < fn.argCount() {
			return semanticError(errTooFewArgsMsg, n.token)
		}

		// Check for too many
		if len(n.stmts) > fn.argCount() && !fn.isVariadic {
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

func walk(symtab *SymTab, n *Node, visit func(*SymTab, *Node) error) (errs []error) {

	// Visit node
	if err := visit(symtab, n); err != nil {
		errs = append(errs, err)
	}
	// Visit left and right
	if n.left != nil {
		errs = append(errs, walk(symtab, n.left, visit)...)
	}
	if n.right != nil {
		errs = append(errs, walk(symtab, n.right, visit)...)
	}

	// TODO: What about function args?

	// Visit children
	for _, stat := range n.stmts {
		if stat != nil {
			errs = append(errs, walk(symtab, stat, visit)...)
		}
	}
	return
}



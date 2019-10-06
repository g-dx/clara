package main

import (
	"fmt"
	"github.com/g-dx/clarac/lex"
)

var id = uint(0) // TODO: Find a better solution to this...

func rewriteAnonFnAndClosures(rootNode *Node, rootSymtab *SymTab, n *Node) error {

	if n.isLocalFn() {

		id += 1

		freeVars := clIdentifyFreeVars(n)
		if len(freeVars) > 0 {

			// ----------------------------------------------------------
			// Closure
			// ----------------------------------------------------------

			// Generate closure & environment structs
			env, envCons := generateStruct(rootNode, fmt.Sprintf("env.%X", id), freeVars...)
			_, clCons := generateStruct(rootNode, fmt.Sprintf("cl.%X", id), n.sym, env)

			// Rewrite <freevar> -> env.<freevar>
			clRewriteFreeVars(n, env, freeVars)

			// Hoist function to root & rename
			clFn := copyNode(n)
			clFn.token = lex.WithVal(clFn.token, fmt.Sprintf("clFn.%X", id))
			clFn.sym.Name = clFn.token.Val
			rootNode.Add(clFn)

			// Update function type information to record closure info
			fnType := clFn.sym.Type.AsFunction()
			fnType.Kind = Closure
			fnType.Data = &ClosureFunc{ /* TODO: Check if this is still required */ }

			// Build AST to capture free variables
			var envArgs []*Node
			for _, v := range freeVars {
				envArgs = append(envArgs, &Node{op: opIdentifier, token: &lex.Token{Val: v.Name}, sym: v, typ: v.Type})
			}

			// AST: fn() { ... } -> Cl(<fn name>, ClEnv(freeVars...))
			n.op = opFuncCall
			n.token = lex.WithVal(clFn.token, clCons.Name)
			n.sym = clCons
			n.stmts = []*Node{
				{op: opIdentifier, token: clFn.token, sym: clFn.sym, typ: clFn.typ},
				{op: opFuncCall, token: &lex.Token{Val: envCons.Name}, typ: envCons.Type, sym: envCons, stmts: envArgs},
			}
			n.left = nil
			n.right = nil
			n.params = nil

		} else {

			// ----------------------------------------------------------
			// Anonymous Function
			// ----------------------------------------------------------

			// Hoist function to root & rename
			fn := copyNode(n)
			fn.token = lex.WithVal(fn.token, fmt.Sprintf("anonFn.%X", id))
			rootNode.Add(fn)

			// AST: fn() { ... } -> <fn name>
			n.op = opIdentifier
			n.token = fn.token
			n.left = nil
			n.right = nil
			n.params = nil
			n.stmts = nil
		}
	}

	if n.isNonGlobalFnCall() {

		// ----------------------------------------------------------
		// Closure/Anonymous/Fn Pointer Call Site
		// ----------------------------------------------------------

		s := rootSymtab.MustResolve("invokeDynamic")

		// AST: <name>(args...) -> invokeDynamic(<name>, args...)
		var stmts []*Node
		if n.sym != nil {
			stmts = append([]*Node{ {op: opIdentifier, token: n.token, sym: n.sym, typ: n.typ} }, n.stmts...)
		} else {
			stmts = append([]*Node{ n.left }, n.stmts...)
		}
		n.stmts = stmts
		n.token = lex.WithVal(n.token, s.Name)
		n.left = nil
		n.sym = s
		n.typ = s.Type
	}

	return nil
}

func clIdentifyFreeVars(fn *Node) (vars []*Symbol) {

	syms := make(map[*Symbol]bool) // Set of syms

	walk(postOrder, nil, nil, fn, func(root *Node, symTab *SymTab, e *Node) error {
		if (e.op == opIdentifier || e.op == opFuncCall) && !fn.symtab.OwnedBy(e.sym) && !e.sym.IsGlobal {
			syms[e.sym] = true
		}
		return nil
	})
	for s := range syms {
		vars = append(vars, s)
	}
	return vars
}

func clRewriteFreeVars(n *Node, env *Symbol, freeVars []*Symbol) {

	// Update AST & function type to pass "env" as first parameter
	envVar := fmt.Sprintf("$env$")
	n.params = append([]*Node{{op: opIdentifier, token: &lex.Token{Val: envVar}, sym: env, typ: env.Type}}, n.params...)
	fn := n.sym.Type.AsFunction()
	fn.Params = append([]*Type{env.Type}, fn.Params...)

	walk(postOrder, nil, nil, n, func(root *Node, symTab *SymTab, e *Node) error {
		if e.op == opIdentifier || e.op == opFuncCall {
			// TODO: O(n) here, we should be able to be O(1)
			for _, freeVar := range freeVars {
				if freeVar == e.sym {

					// AST: <var> -> env.<var> or <fnCall> -> env.<fnCall>
					left := &Node{op: opIdentifier, token: &lex.Token{Val: envVar}, sym: env, typ: env.Type}
					var right *Node
					sym := env.Type.AsStruct().GetField(e.sym.Name)
					if e.op == opIdentifier {
						right = &Node{op: opIdentifier, token: e.token, sym: sym, typ: sym.Type}
					} else {
						right = &Node{op: opFuncCall, token: e.token, sym: sym, typ: sym.Type, stmts: e.stmts}
					}
					e.op = opDot
					e.token = lex.WithVal(e.token, ".")
					e.left = left
					e.right = right
					e.stmts = nil
				}
			}
		}
		return nil
	})
}

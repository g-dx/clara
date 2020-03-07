package main

import (
	"fmt"
	"github.com/g-dx/clarac/lex"
)

var id = uint(0) // TODO: Find a better solution to this...

func rewriteAnonFnAndClosures(rootNode *Node, n *Node) {

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
			clFn.sym.IsGlobal = true
			rootNode.Add(clFn)

			// Update function type information to record closure info
			fnType := clFn.sym.Type.AsFunction()
			fnType.Kind = Closure
			fnType.Data = &ClosureFunc{ /* TODO: Check if this is still required */ }

			// Build AST to capture free variables
			var envArgs []*Node
			for _, v := range freeVars {
				envArgs = append(envArgs, ident(lex.Val(v.Name), v,))
			}

			// AST: fn() { ... } -> Cl(<fn name>, ClEnv(freeVars...))
			n.op = opFuncCall
			n.token = lex.NoToken
			n.stmts = []*Node{
				ident(clFn.token, clFn.sym),
				fnCallBySym(lex.Val(envCons.Name), envCons, envArgs...),
			}
			n.left = ident(lex.WithVal(clFn.token, clCons.Name), clCons)
			n.right = nil
			n.params = nil
			n.sym = nil
			n.typ = clCons.Type.AsFunction().ret

		} else {

			// ----------------------------------------------------------
			// Anonymous Function
			// ----------------------------------------------------------

			// Hoist function to root & rename
			fn := copyNode(n)
			fn.token = lex.WithVal(fn.token, fmt.Sprintf("anonFn.%X", id))
			fn.sym.IsGlobal = true
			rootNode.Add(fn)

			// AST: fn() { ... } -> <fn name>
			n.op = opIdentifier
			n.token = fn.token
			n.sym = fn.sym
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

		s := rootNode.symtab.MustResolve("invokeDynamic")

		// AST: <name>(args...) -> invokeDynamic(<name>, args...)
		n.stmts = append([]*Node{n.left}, n.stmts...)
		n.left = ident(lex.WithVal(n.token, s.Name), s)
		n.token = lex.NoToken
	}
}

func clIdentifyFreeVars(fn *Node) (vars []*Symbol) {

	checker := NewFreeVarChecker(fn)
	WalkPreOrder(fn, checker.IdentityFreeVars)
	for s := range checker.free {
		vars = append(vars, s)
	}
	return vars
}

func clRewriteFreeVars(n *Node, env *Symbol, freeVars []*Symbol) {

	// Update AST & function type to pass "env" as first parameter
	envToken := &lex.Token{Val: "$env$"}
	n.params = append([]*Node{ident(envToken, env)}, n.params...)
	fn := n.sym.Type.AsFunction()
	fn.Params = append([]*Type{env.Type}, fn.Params...)

	WalkPostOrder(n, func(e *Node) {
		if e.Is(opIdentifier) {
			// TODO: O(n) here, we should be able to be O(1)
			for _, freeVar := range freeVars {
				if freeVar == e.sym {

					// AST: <var> -> env.<var> or <fnCall> -> env.<fnCall>
					sym := env.Type.AsStruct().GetField(e.sym.Name)
					e.left = ident(envToken, env)
					e.right = ident(e.token, sym)
					e.op = opDot
					e.token = lex.WithVal(e.token, ".")
					e.stmts = nil
					e.sym = sym
				}
			}
		}
	})
}

type freeVarChecker struct {
	n      *Node
	stack  []*Node
	scopes []*SymTab
	free   map[*Symbol]bool
}

func NewFreeVarChecker(n *Node) *freeVarChecker {
	if !n.Is(opBlockFnDcl, opExprFnDcl) {
		panic(fmt.Sprintf("Unexpected node type for free variable checker: %v", nodeTypes[n.op]))
	}
	return &freeVarChecker{n: n, free: make(map[*Symbol]bool), scopes: append([]*SymTab(nil), n.symtab)}
}

func (fc *freeVarChecker) IdentityFreeVars(n *Node) bool {
	if n == nil {
		return fc.exitNode()
	}
	fc.stack = append(fc.stack, n)

	switch {
	case opensScope(n):
		fc.scopes = append(fc.scopes, n.symtab)

	case n.Is(opIdentifier) && fc.isFree(n):
		fc.free[n.sym] = true

	case n.Is(opDot, opArray) && n.left.Is(opIdentifier):
		if fc.isFree(n.left) {
			fc.free[n.left.sym] = true
		}
		return false // No need to walk right!

	case n.Is(opBlockFnDcl, opExprFnDcl) && n != fc.n:
		return false // Don't walk into other closures!
	}
	return true
}

func (fc *freeVarChecker) exitNode() bool {
	top, ns := fc.stack[len(fc.stack)-1], fc.stack[:len(fc.stack)-1] // Pop node
	fc.stack = ns
	if opensScope(top) {
		fc.scopes = fc.scopes[:len(fc.scopes)-1] // Pop symtab
	}
	return true
}

func (fc *freeVarChecker) isFree(n *Node) bool {
	if n.sym == nil || n.sym.IsGlobal || n.sym.IsLiteral {
		return false
	}
	for _, scope := range fc.scopes {
		if scope.Owns(n.sym) {
			return false
		}
	}
	return true
}

func opensScope(n *Node) bool {
	switch n.op {
	case opIf, opElseIf, opElse, opCase, opWhile:
		return true
	default:
		return false
	}
}

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

		s := rootNode.symtab.MustResolve("invokeDynamic")

		// AST: <name>(args...) -> invokeDynamic(<name>, args...)
		var stmts []*Node
		if n.sym != nil {
			stmts = append([]*Node{{op: opIdentifier, token: n.token, sym: n.sym, typ: n.typ}}, n.stmts...)
		} else {
			stmts = append([]*Node{n.left}, n.stmts...)
		}
		n.stmts = stmts
		n.token = lex.WithVal(n.token, s.Name)
		n.left = nil
		n.sym = s
		n.typ = s.Type
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
	envVar := fmt.Sprintf("$env$")
	n.params = append([]*Node{{op: opIdentifier, token: &lex.Token{Val: envVar}, sym: env, typ: env.Type}}, n.params...)
	fn := n.sym.Type.AsFunction()
	fn.Params = append([]*Type{env.Type}, fn.Params...)

	WalkPostOrder(n, func(e *Node) {
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

	case n.Is(opIdentifier, opFuncCall) && fc.isFree(n):
		fc.free[n.sym] = true

	case n.Is(opDot, opArray) && n.left.Is(opIdentifier, opFuncCall):
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
	if n.sym == nil {
		return false // TODO: This is a result of the opIdentifier/opFuncCall two ways of identifying a function!
	}
	if n.sym.IsGlobal || n.sym.IsLiteral {
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

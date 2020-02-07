package main

import "fmt"

func WalkPreOrder(n *Node, f func(*Node) bool) {
	Walk(true, n, f)
}

func WalkPostOrder(n *Node, f func(*Node)) {
	Walk(false, n, func(n *Node) bool {
		f(n)
		return true
	})
}

func Walk(isPreOrder bool, n *Node, f func(*Node) bool) {
	if isPreOrder && !f(n) {
		return
	}

	switch n.op {
	case opBlockFnDcl, opExprFnDcl, opExternFnDcl, opConsFnDcl, opFuncType:
		if n.right != nil {
			Walk(isPreOrder, n.right, f)
		}
		for _, param := range n.params {
			Walk(isPreOrder, param, f)
		}
		for _, stmt := range n.stmts {
			Walk(isPreOrder, stmt, f)
		}
		if n.left != nil {
			Walk(isPreOrder, n.left, f)
		}

	case opRoot, opStructDcl, opEnumDcl, opBlock, opElse:
		for _, stmt := range n.stmts {
			Walk(isPreOrder, stmt, f)
		}

	case opTypeList:
		for _, param := range n.params {
			Walk(isPreOrder, param, f)
		}

	case opFuncCall:
		if n.left != nil {
			Walk(isPreOrder, n.left, f)
		}
		for _, param := range n.params {
			Walk(isPreOrder, param, f)
		}
		for _, stmt := range n.stmts {
			Walk(isPreOrder, stmt, f)
		}

	case opWhile, opMatch:
		Walk(isPreOrder, n.left, f)
		for _, stmt := range n.stmts {
			Walk(isPreOrder, stmt, f)
		}

	case opCase:
		for _, p := range n.params {
			Walk(isPreOrder, p, f)
		}
		for _, stmt := range n.stmts {
			Walk(isPreOrder, stmt, f)
		}

	case opIf, opElseIf:
		Walk(isPreOrder, n.left, f)
		for _, stmt := range n.stmts {
			Walk(isPreOrder, stmt, f)
		}
		if n.right != nil {
			Walk(isPreOrder, n.right, f)
		}

	case opLit, opError, opNamedType:
		// ...

	case opIdentifier, opReturn:
		if n.left != nil {
			Walk(isPreOrder, n.left, f)
		}

	case opNot, opNeg, opBNot, opArrayType:
		Walk(isPreOrder, n.left, f)

	case opAs, opDas, opAdd, opSub, opMul, opDiv, opAnd, opOr, opBAnd,
		opBOr, opBXor, opEq, opGt, opLt, opBLeft, opBRight, opDot, opArray:
		Walk(isPreOrder, n.left, f)
		Walk(isPreOrder, n.right, f)

	default:
		panic(fmt.Sprintf("Unexpected node type: %v", nodeTypes[n.op]))
	}

	if isPreOrder {
		f(nil) // Signal node exit
	} else {
		f(n)
	}
}

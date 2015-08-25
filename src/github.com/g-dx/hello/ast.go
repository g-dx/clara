package main

// AST

type Node struct {
	token *Token
	left  *Node
	right *Node
	stats []*Node
	op    int
}

func (n *Node) Add(stat *Node) {
	n.stats = append(n.stats, stat)
}

const (
	opFuncDcl = iota
	opFuncCall
	opStrLit
	opRoot
)

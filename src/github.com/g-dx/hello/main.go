package main

import (
	"fmt"
	"os"
)

const prog = `fn main() {
   print("Hello")
   print("World", "!")
   print("!")
   helper()
 }

 fn helper() {
   print("Helper function")
 }`

func main() {
	// Print program
	fmt.Println(prog)

	// Lex
	tokens, err := lex(prog)
	if err != nil {
		fmt.Printf("Lexing error: %s\n", err)
		os.Exit(1)
	}
//		for _, t := range tokens {
//			fmt.Println(t)
//		}

	// Parse
	parser := NewParser(tokens)
	errs, tree := parser.program()
	if len(errs) > 0 {
		for _, err := range errs {
			fmt.Printf("%v\n", err)
		}
	}
	printAST(tree)

	// Semantic

	// Code-gen

	// Link
}

func printAST(n *Node) {
	fmt.Println("\nParse Tree\n")
	printASTImpl(n, "    ", true)
	fmt.Println()
}

func printASTImpl(n *Node, prefix string, isTail bool) {
	// Handle current node
	row := "├── "
	if isTail {
		row = "└── "
	}
	fmt.Printf("%v%v%v (\u001B[95m%v\u001B[0m)\n", prefix, row, n.token.val, nodeTypes[n.op])

	// Handle n-1 children
	row = "|    "
	if isTail {
		row = "   "
	}
	for i := 0; i < len(n.stats)-1; i++ {
		printASTImpl(n.stats[i], prefix + row, false)
	}

	// Handle last child
	if len(n.stats) > 0 {
		printASTImpl(n.stats[len(n.stats)-1], prefix + row, true)
	}
}
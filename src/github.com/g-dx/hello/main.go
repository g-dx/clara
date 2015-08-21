package main

import (
	"fmt"
	"os"
)

const prog = `fn main() {
   print("Hello")
   print("World")
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
	//	for _, t := range tokens {
	//		fmt.Println(t)
	//	}

	// Parse
	parser := NewParser(tokens)
	parser.Parse()
	if parser.hasErrors() {
		for _, msg := range parser.errors {
			fmt.Printf("Parsing Error: %s\n", msg)
		}
		os.Exit(1)
	}

	// Semantic

	// Code-gen

	// Link
}

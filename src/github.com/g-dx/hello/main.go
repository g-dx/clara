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
	errs, _ := parser.program()
	if errs != nil {
		for _, err := range errs {
			fmt.Printf("%v\n", err)
		}
	}
	os.Exit(0)

	// Semantic

	// Code-gen

	// Link
}

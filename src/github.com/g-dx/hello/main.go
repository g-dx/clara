package main

import (
	"fmt"
	"os"
    "strings"
    "io/ioutil"
    "flag"
//	"bytes"
)

func main() {

    // Load program path. Default to "examples"
    path := flag.String("-prog", "../examples/hello.clara", "File with Clara program to compile.")
    flag.Parse()

    // Read program file
    progBytes, err := ioutil.ReadFile(*path)
    if err != nil {
        fmt.Println(err)
        os.Exit(1)
    }

    // Print
    fmt.Println("\nInput Program\n")
    prog := string(progBytes)
    for i, line := range strings.Split(prog, "\n") {
        fmt.Printf("%2d. %v\n", i+1, line)
    }

	// Lex
	tokens, err := lex(prog)
	if err != nil {
		fmt.Printf("\nLexing errors:\n\n %s\n", err)
		os.Exit(1)
	}

	// Parse
	parser := NewParser(tokens, stdlib())
	errs, tree := parser.Parse()

    // Resolve function calls
    errs = append(errs, walk(parser.symtab, tree, resolveFnCall)...)

	if len(errs) > 0 {
        fmt.Println("\nParse Errors\n")
		for _, err := range errs {
			fmt.Printf(" - %v\n", err)
		}
	}
	printTree(tree)

	os.Remove("F:\\hello.exe")
	f, err := os.Create("F:\\hello.exe")
	if err != nil {
		fmt.Printf(" - %v\n", err)
	}

	err = codegen(parser.symtab, tree, f)
	if err != nil {
		fmt.Printf("I/O err: %v\n", err)
	}

	fmt.Printf("Binary Written!\n")

}

func stdlib() []*Node {
	return []*Node{
		// Built in print function
		&Node{token:&Token{val : "println"}, op:opFuncDcl, sym:&Function{"println", 1, 0 }},
	}
}
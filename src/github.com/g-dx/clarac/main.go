package main

import (
	"fmt"
	"os"
    "strings"
    "io/ioutil"
    "flag"
	"path/filepath"
)

func main() {

    // Load program path. Default to "examples"
    path := flag.String("prog", "../examples/hello.clara", "File with Clara program to compile.")
	showProg := flag.Bool("in", false, "Print the input program.")
	showLex := flag.Bool("lex", false, "Print the lexical output.")
	showAst := flag.Bool("ast", false, "Print the generated AST.")
	showAsm := flag.Bool("asm", false, "Print the generated assembly (intel syntax).")
    flag.Parse()

    // Read program file
    progBytes, err := ioutil.ReadFile(*path)
    if err != nil {
        fmt.Println(err)
        os.Exit(1)
    }
	prog := string(progBytes)

	if *showProg {
		printProgram(prog)
	}

	// Lex
	tokens, err := lex(prog)
	if err != nil {
		printProgram(prog)
		fmt.Printf("\nLexing errors:\n\n %s\n", err)
		os.Exit(1)
	}

	if *showLex {
		printLex(tokens)
	}

	// Parse
	parser := NewParser(tokens, stdlib())
	errs, tree := parser.Parse()

    // Resolve function calls
    errs = append(errs, walk(parser.symtab, tree, resolveFnCall)...)

	if len(errs) > 0 {
		printProgram(prog)
        fmt.Println("\nParse Errors\n")
		for _, err := range errs {
			fmt.Printf(" - %v\n", err)
		}
		os.Exit(1)
	}

	// Print AST if necessary
	if *showAst {
		printTree(tree)
	}

	// Create output file
	basename := filepath.Base(*path)
	f, err := os.Create(fmt.Sprintf("%v%c%v.exe", filepath.Dir(*path), filepath.Separator, strings.TrimSuffix(basename, filepath.Ext(basename))))
	if err != nil {
		fmt.Printf(" - %v\n", err)
	}

	// Generate code
	err = codegen(parser.symtab, tree, f, *showAsm)
	if err != nil {
		fmt.Printf("\nCode Gen Errors:\n %v\n", err)
		os.Exit(1)
	}
}

func stdlib() []*Node {
	return []*Node{
		// Built in print function
		&Node{token:&Token{val : "println"}, op:opFuncDcl, sym:&Function{"println", 1, 0 }},
	}
}

func printProgram(prog string) {
	fmt.Println("\nInput Program\n")
	for i, line := range strings.Split(prog, "\n") {
		fmt.Printf("%2d. %v\n", i+1, line)
	}
}

func printLex(tokens []*Token) {
	fmt.Println("\nLexical Tokens\n")
	for _, token := range tokens {
		fmt.Println(token)
	}
}
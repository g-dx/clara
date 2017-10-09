package main

import (
	"fmt"
	"os"
    "strings"
    "io/ioutil"
    "flag"
	"path/filepath"
	"github.com/g-dx/clarac/lex"
	"github.com/g-dx/clarac/console"
	"os/exec"
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
	tokens := make([]*lex.Token, 0, 10)
	lexer := lex.Lex(prog, *path)
	// TODO: Lexing errors should really appear from parse stage
	for {
		token := lexer.NextToken()
		// TODO: Parser could filter tokens it's not interested in
		switch token.Kind {
		case lex.EOL, lex.Space, lex.Comment:
			continue
		case lex.Err:
			printProgram(prog)
			fmt.Printf("\nLexing errors:\n\n %s\n", token)
			os.Exit(1)
		default:
			tokens = append(tokens, token)
		}
		// Check for EOF
		if token.Kind == lex.EOF {
			break
		}
	}

	if *showLex {
		printLex(tokens)
	}

	// Parse
	parser := NewParser(tokens, stdlib(), stdSyms())
	errs, tree := parser.Parse()

	// Rewrite dot selections
	errs = append(errs, walk(tree, parser.symtab, tree, rewriteDotFuncCalls)...)

	// Generate constructor functions
	errs = append(errs, walk(tree, parser.symtab, tree, generateStructConstructors)...)

	// Resolve function calls
	errs = append(errs, walk(tree, parser.symtab, tree, resolveFnCall)...)

	// Resolve variables/identifiers
	errs = append(errs, walk(tree, parser.symtab, tree, resolveVariables)...)
	exitIfErrors(showAst, tree, errs, prog)

	// Configure field offsets to struct vars
	errs = append(errs, walk(tree, parser.symtab, tree, configureFieldAccess)...)
	exitIfErrors(showAst, tree, errs, prog)

	// Create assembly file
	basename := filepath.Base(*path)
	progName := strings.TrimSuffix(basename, filepath.Ext(basename))
	asmPath := fmt.Sprintf("/tmp/%v.S", progName)
	os.Remove(asmPath) // Ignore error
	f, err := os.Create(asmPath)
	if err != nil {
		fmt.Printf(" - %v\n", err)
	}

	// Generate assembly
	err = codegen(parser.symtab, tree, f, *showAsm)
	if err != nil {
		fmt.Printf("\nCode Gen Errors:\n %v\n", err)
		os.Exit(1)
	}
	f.Close()

	// Write out harness
	harnessPath := "/tmp/harness.c"
	os.Remove(harnessPath) // Ignore error
	f, err = os.Create(harnessPath)
	if err != nil {
		fmt.Printf(" - %v\n", err)
	}
	fmt.Fprintf(f, "#include <stdio.h>\n#include <stdlib.h>\nint clara_main();\nint main(int argc, char** argv) { clara_main(); return 0; }")
	f.Close()

	// Invoke gcc to link files
	cmd := exec.Command("gcc", "-o", progName, asmPath, harnessPath)
	cmd.Stderr = os.Stderr
	out, err := cmd.Output()
	if err != nil {
		fmt.Printf("Link failure: %v\n%v", err, out)
	}
}

func exitIfErrors(showAst *bool, tree *Node, errs []error, prog string) {
	// Print AST if necessary
	if *showAst {
		printTree(tree)
	}
	if len(errs) > 0 {
		printProgram(prog)
		fmt.Println("\nParse Errors\n")
		for _, err := range errs {
			fmt.Printf(" - %v\n", err)
		}
		os.Exit(1)
	}
}

func stdSyms() []Symbol {
	return []Symbol{
		// string type
		&TypeSymbol{val: "string", width: 8},
		// int type
		&TypeSymbol{val: "int", width: 8},
		// bool type
		&TypeSymbol{val: "bool", width: 8},
	}
}

func stdlib() []*Node {
	return []*Node{
		// printf (from libc)
		&Node{token:&lex.Token{Val : "printf"}, op:opFuncDcl,
		sym:&Function{fnName: "printf", fnArgCount: 1, isVariadic: true}},
		// memcpy (from libc)
		&Node{token:&lex.Token{Val : "memcpy"}, op:opFuncDcl,
			sym:&Function{fnName: "memcpy", fnArgCount: 3}},
		// malloc (from libc)
		&Node{token:&lex.Token{Val : "malloc"}, op:opFuncDcl,
			sym:&Function{fnName: "malloc", fnArgCount: 1}},
	}
}

func printProgram(prog string) {
	fmt.Println("\nInput Program\n")
	for i, line := range strings.Split(prog, "\n") {
		fmt.Printf("%v%2d%v. %v%v%v\n", console.Yellow, i+1, console.Disable, console.NodeTypeColour, line, console.Disable)
	}
}

func printLex(tokens []*lex.Token) {
	fmt.Println("\nLexical Tokens\n")
	for _, token := range tokens {
		fmt.Println(token)
	}
}
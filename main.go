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
	"os/user"
)

func main() {

	// Get user details
	usr, err := user.Current()
	if err != nil {
		panic(err)
	}

	// Default install dir
	defaultInstall := fmt.Sprintf("%v/.clara", usr.HomeDir)

	// Load program path. Default to "examples"
	installPath := flag.String("install", defaultInstall, "Path to install directory.")
	path := flag.String("prog", "/examples/hello.clara", "File with Clara program to compile.")
	showProg := flag.Bool("in", false, "Print the input program.")
	showLex := flag.Bool("lex", false, "Print the lexical output.")
	showAst := flag.Bool("ast", false, "Print the generated AST.")
	showTypes := flag.Bool("types", false, "Print type information as it assigned during semantic analysis.")
	showAsm := flag.Bool("asm", false, "Print the generated assembly (intel syntax).")
	outPath := flag.String("out", ".", "Path to write program to.")
	flag.Parse()

	// Define root AST node
	rootSymtab := NewSymtab()
	rootNode := &Node{op: opRoot, symtab: rootSymtab}

	// Add "AST defined" nodes & symbols
	for _, n := range stdlib() {
		rootSymtab.Define(n.sym)
		rootNode.Add(n)
	}

	// Add any global symbols
	for _, s := range stdSyms() {
		rootSymtab.Define(s)
	}

	// Read all lib files
	pattern := fmt.Sprintf("%v/lib/*.clara", *installPath) // NOTE: Does NOT traverse all directories!
	codePaths, err := filepath.Glob(pattern)
	if err != nil {
		fmt.Printf("Could not read standard library: %v\n", err)
		os.Exit(1)
	}

	// Parser & token storage
	tokens := make([]*lex.Token, 0, 10)
	parser := NewParser()
	var errs []error
	var prog string

	// Loop over all files & lex + parse
	codePaths = append(codePaths, *path)
	for _, codePath := range codePaths {

		// Read program file
		progBytes, err := ioutil.ReadFile(codePath)
		if err != nil {
			fmt.Println(err)
			os.Exit(1)
		}
		code := string(progBytes)
		if codePath == *path {
			prog = code
		}

		// Lex
		lexer := lex.Lex(code, codePath)
		// TODO: Lexing errors should really appear from parse stage
		for {
			token := lexer.NextToken()
			// TODO: Parser could filter tokens it's not interested in
			switch token.Kind {
			case lex.EOL, lex.Space, lex.Comment:
				continue
			case lex.Err:
				printProgram(code)
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
		errs = append(errs, parser.Parse(tokens, rootNode)...)
		tokens = tokens[:0]
	}

	if *showProg {
		printProgram(prog)
	}

	// Handle top level types first
	errs = append(errs, processTopLevelTypes(rootNode, rootSymtab)...)
	exitIfErrors(showAst, rootNode, errs, prog)

	// Generate constructor functions
	errs = append(errs, walk(rootNode, rootSymtab, rootNode, generateStructConstructors)...)
	errs = append(errs, walk(rootNode, rootSymtab, rootNode, addRuntimeInit)...)
	exitIfErrors(showAst, rootNode, errs, prog)

	// Type check
	errs = append(errs, typeCheck(rootNode, rootSymtab, nil, *showTypes)...)
	exitIfErrors(showAst, rootNode, errs, prog)

	// Show final AST if necessary
	if *showAst {
		printTree(rootNode)
	}

	// Create assembly file
	basename := filepath.Base(*path)
	progName := strings.TrimSuffix(basename, filepath.Ext(basename))
	asmPath := fmt.Sprintf("/%v/%v.S", os.TempDir(), progName)
	os.Remove(asmPath) // Ignore error
	f, err := os.Create(asmPath)
	if err != nil {
		fmt.Printf(" - %v\n", err)
	}

	// Generate assembly
	asm := NewGasWriter(f, *showAsm)
	err = codegen(rootSymtab, rootNode.stmts, asm)
	if err != nil {
		fmt.Printf("\nCode Gen Errors:\n %v\n", err)
		os.Exit(1)
	}
	f.Close()

	// Invoke gcc to link files
	bootstrapPath := filepath.Join(*installPath, "/init/bootstrap.c")
	runtimePath := filepath.Join(*installPath, "/init/runtime.c")
	outputPath := filepath.Join(*outPath, progName)
	cmd := exec.Command("gcc", "-static", "-g", "-o", outputPath, asmPath, bootstrapPath, runtimePath)
	cmd.Stderr = os.Stderr
	out, err := cmd.Output()
	if err != nil {
		fmt.Printf("Link failure: %v\n%v\n", err, string(out))
		os.Exit(1)
	}
}

func exitIfErrors(showAst *bool, tree *Node, errs []error, prog string) {
	// Print AST if necessary
	if len(errs) > 0 {

		// Show state of AST before exit
		printTree(tree)

		printProgram(prog)
		fmt.Println("\nParse Errors\n")
		for _, err := range errs {
			fmt.Printf(" - %v\n", err)
		}
		os.Exit(1)
	}
}

func stdSyms() []*Symbol {
	return []*Symbol{
		// string type
		{ Name: "string", Type: stringType },
		// int type
		{ Name: "int", Type: intType },
		// byte type
		{ Name: "byte", Type: byteType },
		// bool type
		{ Name: "bool", Type: boolType },
		// nothing type
		{ Name: "nothing", Type: nothingType },
	}
}

func stdlib() []*Node {
	return []*Node{
		// printf (from libc)
		{token:&lex.Token{Val : "printf"}, op:opFuncDcl,
		sym:&Symbol{ Name: "printf", Type: &Type{ Kind: Function, Data:
			&FunctionType{ Args: []*Type { stringType }, isVariadic: true, ret: nothingType, IsExternal: true }}}},

		// debug (from runtime.c)
		{token:&lex.Token{Val : "debug"}, op:opFuncDcl,
			sym:&Symbol{ Name: "debug", Type: &Type{ Kind: Function, Data:
			&FunctionType{ Args: []*Type { stringType, stringType }, isVariadic: true, ret: nothingType, IsExternal: true }}}},
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
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
	"io"
	"errors"
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
	progPath := flag.String("prog", "/examples/hello.clara", "File with Clara program to compile.")
	showProg := flag.Bool("in", false, "Print the input program.")
	showLex := flag.Bool("lex", false, "Print the lexical output.")
	showAst := flag.Bool("ast", false, "Print the generated AST.")
	showTypes := flag.Bool("types", false, "Print type information as it assigned during semantic analysis.")
	showAsm := flag.Bool("asm", false, "Print the generated assembly (intel syntax).")
	outPath := flag.String("out", ".", "Path to write program to.")
	flag.Parse()

	// Gather standard lib & C files
	claraLib := glob(fmt.Sprintf("%v/lib/*.clara", *installPath)) // NOTE: Does NOT traverse all directories!
	cLib := glob(fmt.Sprintf("%v/init/*.c", *installPath)) // NOTE: Does NOT traverse all directories!

	// Open program to compile
	f, err := os.Open(*progPath)
	if err != nil {
		panic(err)
	}
	defer f.Close()

	options := options{ showLex: *showLex, showAst: *showAst, showTypes: *showTypes, showAsm: *showAsm, showProg: *showProg }
	Compile(options, claraLib, f, *progPath, cLib, *outPath)
}

type options struct {
	showLex   bool
	showAst   bool
	showTypes bool
	showAsm   bool
	showProg  bool
}

func Compile(options options, claraLibPaths []string, progReader io.Reader, progPath string, cLibPaths []string, outPath string) string {

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

	// Lex + parse standard lib
	var errs []error
	for _, f := range claraLibPaths {

		// Read program file
		bytes, err := ioutil.ReadFile(f)
		if err != nil {
			fmt.Println(err)
			os.Exit(1)
		}
		errs = append(errs, lexAndParse(string(bytes), f, rootNode, options.showLex)...)
	}

	// Lex + parse program
	bytes, err := ioutil.ReadAll(progReader)
	prog := string(bytes)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	errs = append(errs, lexAndParse(prog, progPath, rootNode, options.showLex)...)

	if options.showProg {
		printProgram(prog)
	}
	exitIfErrors(options.showAst, rootNode, errs, prog)

	// Handle top level types first
	errs = append(errs, processTopLevelTypes(rootNode, rootSymtab)...)
	exitIfErrors(options.showAst, rootNode, errs, prog)

	// Generate constructor functions
	errs = append(errs, walk(rootNode, rootSymtab, rootNode, generateStructConstructors)...)
	errs = append(errs, walk(rootNode, rootSymtab, rootNode, addRuntimeInit)...)
	exitIfErrors(options.showAst, rootNode, errs, prog)

	// Type check
	errs = append(errs, typeCheck(rootNode, rootSymtab, nil, options.showTypes)...)
	exitIfErrors(options.showAst, rootNode, errs, prog)

	// Show final AST if necessary
	if options.showAst {
		printTree(rootNode)
	}

	// Create assembly file
	basename := filepath.Base(progPath)
	progName := strings.TrimSuffix(basename, filepath.Ext(basename))
	asmPath := fmt.Sprintf("%v/%v.S", os.TempDir(), progName)
	os.Remove(asmPath) // Ignore error
	f, err := os.Create(asmPath)
	if err != nil {
		fmt.Printf(" - %v\n", err)
	}

	// Generate assembly
	asm := NewGasWriter(f, options.showAsm)
	err = codegen(rootSymtab, rootNode.stmts, asm)
	if err != nil {
		fmt.Printf("\nCode Gen Errors:\n %v\n", err)
		os.Exit(1)
	}
	f.Close()

	// Invoke gcc to link files
	outputPath := filepath.Join(outPath, progName)
	args := []string { "-static" }
	args = append(args, "-g")
	args = append(args, "-o")
	args = append(args, outputPath)
	args = append(args, asmPath)
	args = append(args, cLibPaths...)
	cmd := exec.Command("gcc", args...)
	cmd.Stderr = os.Stderr
	out, err := cmd.Output()
	if err != nil {
		fmt.Printf("Link failure: %v\n%v\n", err, string(out))
		os.Exit(1)
	}
	return outputPath
}

func lexAndParse(code string, path string, root *Node, showLex bool) (errs []error) {

	// Lex
	var tokens []*lex.Token
	lexer := lex.Lex(code, path)
	// TODO: Lexing errors should really appear from parse stage
	for {
		token := lexer.NextToken()
		// TODO: Parser could filter tokens it's not interested in
		switch token.Kind {
		case lex.EOL, lex.Space, lex.Comment:
			continue
		case lex.Err:
			return []error { errors.New(token.String()) }
		default:
			tokens = append(tokens, token)
		}
		// Check for EOF
		if token.Kind == lex.EOF {
			break
		}
	}

	if showLex {
		printLex(tokens)
	}

	// Parse
	return NewParser().Parse(tokens, root)
}

func exitIfErrors(showAst bool, tree *Node, errs []error, prog string) {
	// Print AST if necessary
	if len(errs) > 0 {

		// Show state of AST before exit
		if showAst	{
			printTree(tree)
		}

		printProgram(prog)
		fmt.Println("\nErrors\n")
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
		{token:&lex.Token{Val : "printf"}, op: opBlockFnDcl,
		sym:&Symbol{ Name: "printf", Type: &Type{ Kind: Function, Data:
			&FunctionType{ Args: []*Type { stringType }, isVariadic: true, ret: nothingType, IsExternal: true }}}},

		// debug (from runtime.c)
		{token:&lex.Token{Val : "debug"}, op: opBlockFnDcl,
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

func glob(pattern string) []string {
	paths, err := filepath.Glob(pattern)
	if err != nil {
		panic(err) // Only happens with bad pattern
	}
	return paths
}
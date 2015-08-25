package main

import (
	"errors"
	"fmt"
)

/**

GRAMMAR
=======

<program> ::= { fn ident() fnBodyStart { ident( args ) } fnBodyEnd }
<ident> ::= name
<fnBodyStart> ::= {
<fnBodyEnd> ::= }

*/

/**
program :: = function_dcl *
function_dcl :: fn ident () { fn_call * }
ident :: [a-zA-Z]
fn_call : ident(args)
args : arg (, arg)*
arg : string_lit
string_lit : "[a-zA-Z0-9]"

*/


type Parser struct {
	pos    int
	tokens []*Token
	errors []string
}

var errEof = errors.New("EOF")
var errNoMatch = errors.New("No match")

func NewParser(tokens []*Token) *Parser {
	return &Parser{0, tokens, make([]string, 0, 5)}
}

func (p *Parser) hasErrors() bool {
	return len(p.errors) > 0
}

func (p *Parser) error(expected string) {
	token := p.tokens[p.pos]
	p.errors = append(p.errors,
		fmt.Sprintf("Unexpected token: '%v' @ %d:%d, Expected: %v",
			token.val, token.line, token.pos, expected))
}

func isEOF(err error) bool {
	return err == errEof
}

func noMatch(err error) bool {
	return err == errNoMatch
}

func (p *Parser) program() (errs []error, root *Node) {

	// Create root node
	root = &Node{nil, nil, nil, nil, opRoot}

	// Loop until EOF
	err, node := p.function_dcl()
	for !isEOF(err) {
		// Log & skip. Try to find next function declaration
		errs = append(errs, err)
		if noMatch(err) {
			for noMatch(err) || isEOF(err) {
				if p.pos+1 == len(p.tokens) {
					err = errEof
				} else {
					p.next()
					_, node = p.function_dcl()
				}
			}
		}
		// Add function declaration and continue matching
		root.Add(node)
		err, node = p.function_dcl()
	}
	return errs, root
}

func (p *Parser) function_dcl() (err error, fn *Node) {

	// Catch match panics and return error
	defer func() {
		if r := recover(); r != nil {
			fmt.Printf("Panic: %v\n", r)
			err = r.(error)
		}
	}()

	// Match declaration
	_ = p.matchOrPanic(keyword)
	name := p.matchOrPanic(fnName)
	_ = p.matchOrPanic(fnArgsStart)
	_ = p.matchOrPanic(fnArgsEnd)
	_ = p.matchOrPanic(fnBodyStart)

	// Use lookahead to determine
	var fnCalls []*Node
	for {
		if p.lookAhead(fnName) {
			fnCalls = append(fnCalls, p.fnCall())
		} else if p.lookAhead(fnBodyEnd) {
			p.match(fnBodyEnd)
			break
		} else {
			panic(errors.New("EXPECTED 'fn name' or '}', Found: 'xxx'"))
		}
	}
	// Create node
	fn = &Node{name, nil, nil, nil, opFuncDcl}
	for _, f := range fnCalls {
		fn.Add(f)
	}
	return err, fn
}

func (p *Parser) fnCall() *Node {
	name := p.matchOrPanic(fnName)
	_ = p.matchOrPanic(fnArgsStart)
	args := p.FnArgs()
	_ = p.matchOrPanic(fnArgsEnd)
	return &Node{name, nil, nil, args, opFuncCall}
}

func (p *Parser) FnArgs() []*Node {
	var args []*Node
	if p.lookAhead(fnArgsEnd) {
		return args
	}

	// Match first argument
	arg := p.matchOrPanic(strLit)
	args = append(args, &Node{arg, nil, nil, nil, opStrLit})

	// Match all remaining args
	for {
		if p.lookAhead(argsSeperator) {
			_ = p.matchOrPanic(argsSeperator)
			arg = p.matchOrPanic(strLit)
			args = append(args, &Node{arg, nil, nil, nil, opStrLit})
		} else if p.lookAhead(fnArgsEnd) {
			break
		} else {
			panic(errors.New("Expected: ')' or ',', Found: 'xxx'"))
		}
	}
	return args
}

func (p *Parser) match(kind string) (ok bool, t *Token) {
	ok = p.tokens[p.pos].kind == kind
	if ok {
		t = p.tokens[p.pos]
		p.next()
	}
	return ok, t
}

func (p *Parser) Parse() {

	// Setup recover on EndOfInput
	defer func() {
		if r := recover(); r != nil {
			fmt.Println("END OF INPUT")
		}
	}()

	for p.matchKindValueOrSkip(keyword, fnKeyword) { // TODO: Fix me to point to "fn'
		p.matchKindOrSkip(fnName)
		p.matchKindOrSkip(fnArgsStart)
		p.matchKindOrSkip(fnArgsEnd)
		p.matchKindOrSkip(fnBodyStart)

		// Fun declaration
		fnDcl := &Node{op: opFuncDcl}
		for p.matchKind(fnName) {

			// Fun call
			fnCall := &Node{op: opFuncCall}
			p.matchKindOrSkip(fnArgsStart)
			if p.matchKind(strLit) {
				fnCall.left = &Node{op: opStrLit} // Store arg
			}
			p.matchKindOrSkip(fnArgsEnd)

			// Add statement to list
			fnDcl.Add(fnCall)
		}
		p.matchKindOrSkip(fnBodyEnd)
	}
}

func (p *Parser) matchOrPanic(kind string) (*Token) {
	fmt.Printf("Matching %v, Expected: %v\n", p.tokens[p.pos].kind, kind)
	ok := p.tokens[p.pos].kind == kind
	if !ok {
		panic(errNoMatch)
	}
	token := p.tokens[p.pos]
	p.next()
	return token
}


func (p *Parser) matchKindOrSkip(kind string) bool {
	if !p.matchKind(kind) {
		p.error(kind)
		p.next()
		for !p.matchKind(kind) {
			p.next()
		}
	}
	return true
}

func (p *Parser) matchKindValueOrSkip(kind string, val string) bool {
	if !p.matchKindValue(kind, val) {
		p.error(kind)
		p.next()
		for !p.matchKindValue(kind, val) {
			p.next()
		}
	}
	return true
}

func (p *Parser) matchKindValue(kind string, val string) bool {
	var ok bool
	if ok = (p.tokens[p.pos].val == val && p.tokens[p.pos].kind == kind); ok {
		p.next()
	}
	return ok
}

func (p *Parser) matchKind(kind string) bool {
	ok := p.tokens[p.pos].kind == kind
	if ok {
		p.next()
	}
	return ok
}

func (p *Parser) next() {
	p.pos++
	if p.pos == len(p.tokens) {
		panic(errEof)
	}
}

func (p *Parser) lookAhead(kind string) bool {
//	if p.pos == len(p.tokens) {
//		panic(noMatch) // TODO: Shouldn't get here!
//	}
	return p.tokens[p.pos].kind == kind
}

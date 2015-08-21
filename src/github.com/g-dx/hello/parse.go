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

var eof = errors.New("EOF")

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

func (p *Parser) program() []error {

	// Create root node
	root := &Node()

	// Loop until EOF
	err, node := p.function_dcl()
	for !isEOF() {
		if noMatch(err) {
			// Log it and continue to next function
			for noMatch(err) || isEOF(err) { // WHILE NO-MATCH AND NOT EOF
				p.next()
				err, node = p.function_dcl()
			}
		}
		// Add node
		root.Add(node)

		// Attempt next match
		err, node = p.function_dcl()
	}
	return nil
}

func (p *Parser) function_dcl() (error, *Node) {

	// Catch match panics and return error

	_, _ = p.matchOrPanic(fnKeyword)
	_, name := p.matchOrPanic(fnName)
	_, _ = p.matchOrPanic(fnArgsStart)
	_, _ = p.matchOrPanic(fnArgsEnd)
	_, _ = p.matchOrPanic(fnBodyStart)

	// Use 1 token lookahead
	var fnCalls []*Node
	if p.next(fnName){ // IDENT
		fnCalls = p.fnCalls()
	} else if p.next(fnBodyEnd) { // END OF FUNCTION DECL
		p.match(fnBodyEnd)
	} else {
		panic() // EXPECTED 'fn name' or '}', Found: 'xxx'
	}
}

func (p *Parser) fnCalls() (bool, []*Node) {
	var calls []*Node
	for !p.next(fnBodyEnd)
	{
		name := p.matchOrPanic(fnName)
		_ = p.matchOrPanic(fnArgsStart)
		args := p.FnArgs()
		_ = p.matchOrPanic(fnArgsEnd)
		// build node and add to list
	}
	return calls
}

func (p *Parser) FnArgs() ([]*Node) {
	var args []*Node
	for !p.next(fnArgsEnd) {
		arg := p.matchOrPanic(strLit)
		_ = p.matchOrPanic(argsSeperator)
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
		panic(eof)
	}
}

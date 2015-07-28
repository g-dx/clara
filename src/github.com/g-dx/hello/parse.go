package main
import (
	"fmt"
)

/**

 GRAMMAR
 =======

program ::= { fn ident() fnBodyStart { ident( args ) } fnBodyEnd }

 <program> ::= { <fnDcl> }
 <fnDcl> ::= fn ident() fnBodyStart fnBody fnBodyEnd
 <fnBody> ::= block | empty
 <block> ::= { function }
 <function> ::= ident(args)
 <args> ::= string_literal | empty
 <ident> ::= name
 <fnBodyStart> ::= {
 <fnBodyEnd> ::= }

 */

type Parser struct {
	pos int
	tokens []*Token
	errors []string
}

func NewParser(tokens []*Token) *Parser {
	return &Parser{ 0, tokens, make([]string, 0, 5) }
}

func (p *Parser) hasErrors() bool {
	return len(p.errors) > 0
}

func (p *Parser) error(expected string) {
	p.errors = append(p.errors,
		fmt.Sprintf("Unexpected token: %v, Expected: %v", p.tokens[p.pos], expected))
}

func (p *Parser) Parse2() {

	// Setup recover on EndOfInput
	defer func() {
		if r := recover(); r != nil {
			// TODO:
			fmt.Println("END OF INPUT")
		}
	}()

	for p.expectOrSkip("KEYWORD") { // TODO: Fix me to point to "fn'
		p.expectOrSkip(fnName)
		p.expectOrSkip(fnArgsStart)
		p.expectOrSkip(fnArgsEnd)
		p.expectOrSkip(fnBodyStart)
		for p.accept("KEYWORD") || p.accept(fnName) {
			p.expectOrSkip(fnArgsStart)
			if p.accept(strLit) {
				// Store literal for node creation
			}
			p.expectOrSkip(fnArgsEnd)
			// Build node for function call
		}
		p.expectOrSkip(fnBodyEnd)
	}
}

func (p *Parser) Parse() {

	// Recover no input
	defer func() {
		if r := recover(); r != nil {
			// TODO:
			fmt.Println("END OF INPUT")
		}
	}()

	for p.pos != len(p.tokens) {
		// skip tokens until we find function declaration
		if !p.accept("KEYWORD") {
//			p.error(nil)
		}
		p.FN()
	}
}

func (p *Parser) FN() {
	p.expectOrSkip("KEYWORD")
	p.expectOrSkip(fnName)
	p.expectOrSkip(fnArgsStart)
	p.expectOrSkip(fnArgsEnd)
	p.expectOrSkip(fnBodyStart)
	p.FN_BODY()
	p.expectOrSkip(fnBodyEnd)
}

func (p *Parser) FN_BODY() {
	for !p.current(fnBodyEnd) {
		p.expectOrSkip("KEYWORD")
		p.expectOrSkip(fnArgsStart)
		if p.current(strLit) {
			p.next()
		}
		p.expectOrSkip(fnArgsEnd)
	}
}

func (p *Parser) expectOrSkip(kind string) bool {
	if !p.accept(kind) {
		p.error(kind)
		for !p.accept(kind) {
			// Skip until found...
			p.next()
		}
	}
	return true
}

func (p *Parser) current(kind string) bool {
	return p.tokens[p.pos].kind == kind
}

func (p *Parser) accept(kind string) bool {
	var ok bool
	if ok = p.current(kind); ok {
		p.next()
	}
	return ok
}

func (p *Parser) next() {
	p.pos++
	if p.pos == len(p.tokens) {
		panic("eof")
	}
}
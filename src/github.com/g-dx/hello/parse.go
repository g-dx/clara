package main
import (
	"fmt"
	"errors"
)

/**

 GRAMMAR
 =======

 <program> ::= { fn ident() fnBodyStart { ident( args ) } fnBodyEnd }
 <ident> ::= name
 <fnBodyStart> ::= {
 <fnBodyEnd> ::= }

 */

type Parser struct {
	pos int
	tokens []*Token
	errors []string
}

var eof = errors.New("EOF")

func NewParser(tokens []*Token) *Parser {
	return &Parser{ 0, tokens, make([]string, 0, 5) }
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
		for p.matchKind(fnName) {
			p.matchKindOrSkip(fnArgsStart)
			if p.matchKind(strLit) {
				// Store literal for node creation
			}
			p.matchKindOrSkip(fnArgsEnd)
			// Build node for function call
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
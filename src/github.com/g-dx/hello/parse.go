package main

import (
	"errors"
	"fmt"
	"strings"
)

/**

GRAMMAR
=======

<program>       ::= fnDeclaration*
<fnDeclaration> ::= fn ident () { fnCall* }
<fnCall>        ::= ident(fnArgs)
<fnArgs>        ::= string_lit (, string_lit)*
<string_lit>    ::= "[a-zA-Z0-9]"
<ident>         ::= [a-zA-Z]

*/

type Parser struct {
	pos    int
	tokens []*Token
	errs   []error
	discard bool
}

var errUnexpectedEof = errors.New("Unexpected EOF")

func NewParser(tokens []*Token) *Parser {
	return &Parser{tokens : tokens}
}

func (p *Parser) Parse() (root *Node) {

    // We only recover from unexpected EOF
	defer func() {
		if r := recover(); r != nil {
			if r != errUnexpectedEof {
                panic(r)
            }
		}
	}()

    // Create root & loop
    root = &Node{&Token{"", "ROOT", 0, 0}, nil, nil, nil, opRoot}
    for
	{
		if p.match(tokenEOF) {
			break;
		} else if p.match(keyword) {
			root.Add(p.fnDeclaration())
		} else {
			// If we aren't discarding, record error & continue
			if !p.discarding() {
				p.syntaxError([]string{kindValues[keyword], kindValues[tokenEOF]})
			}
			p.consume()
		}
	}
	return root
}

func (p *Parser) fnDeclaration() *Node {

	// Consume tokens
	_ = p.consumeOrSkip(keyword)
	name := p.consumeOrSkip(fnName)
	_ = p.consumeOrSkip(fnArgsStart)
	_ = p.consumeOrSkip(fnArgsEnd)
	_ = p.consumeOrSkip(fnBodyStart)

	var fnCalls []*Node
	for {
		if p.match(fnName) {
			fnCalls = append(fnCalls, p.fnCall())
		} else if p.match(fnBodyEnd) {
			p.consume()
			break
		} else {
			// If we aren't discarding, record error & continue
			if !p.discarding() {
				p.syntaxError([]string{kindValues[fnName], kindValues[fnBodyEnd]})
			}
			p.consume()
		}
	}
	// Create node
	fn := &Node{name, nil, nil, nil, opFuncDcl}
	for _, f := range fnCalls {
		fn.Add(f)
	}
	return fn
}

func (p *Parser) fnCall() *Node {
	name := p.consumeOrSkip(fnName)
	_ = p.consumeOrSkip(fnArgsStart)
	args := p.fnArgs()
	_ = p.consumeOrSkip(fnArgsEnd)
	return &Node{name, nil, nil, args, opFuncCall}
}

func (p *Parser) fnArgs() []*Node {
	var args []*Node
	if p.match(fnArgsEnd) {
		return args
	}

	// Match first argument
	arg := p.consumeOrSkip(strLit)
	args = append(args, &Node{arg, nil, nil, nil, opStrLit})

	// Match all remaining args
	for {
		if p.match(argsSeperator) {
			_ = p.consumeOrSkip(argsSeperator)
			arg = p.consumeOrSkip(strLit)
			args = append(args, &Node{arg, nil, nil, nil, opStrLit})
		} else if p.match(fnArgsEnd) {
			break
		} else {
			// If we aren't discarding, record error & continue
			if !p.discarding() {
				p.syntaxError([]string{kindValues[fnArgsEnd], kindValues[argsSeperator]})
			}
			p.consume()
		}
	}
	return args
}

func (p *Parser) consumeOrSkip(kind string) (*Token) {
	// Attempt to match until we find
	ok := p.match(kind)
	for !ok {
		if !p.discarding() {
			p.syntaxError([]string{kindValues[kind]})
		}
		p.consume()
		ok = p.match(kind)
	}

	// Store token and advance
	token := p.tokens[p.pos]
	p.consume()
	return token
}

func (p *Parser) consume() {
	// Panic if unexpectedly no more input
	if (p.pos+1 >= len(p.tokens)) {
		panic(errUnexpectedEof)
	}
	p.pos++
}

func (p *Parser) match(kind string) bool {
	ok := p.tokens[p.pos].kind == kind
	if ok {
		p.restore()
	}
	return ok
}

func (p *Parser) discarding() bool {
	wasOk := p.discard
	p.discard = true
	return wasOk
}

func (p *Parser) restore() {
	p.discard = false
}

func (p *Parser) syntaxError(expected []string) {
	token := p.tokens[p.pos]
	p.errs = append(p.errs, errors.New(fmt.Sprintf("%d:%d: syntax error, Unexpected '%v', Expecting: '%v'",
	token.line,
	token.pos,
	p.tokens[p.pos].val,
	strings.Join(expected, "' or '"))))
}
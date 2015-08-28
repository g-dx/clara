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

const (
	syntaxErrMsg = "%d:%d: syntax error, Unexpected '%v', Expecting: '%v'"
)

type Parser struct {
	pos    int
	tokens []*Token
	errs   []error
	discard bool // Are we in "discard" mode?
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
			p.syntaxError(kindValues[keyword], kindValues[tokenEOF])
		}
	}
	return root
}

func (p *Parser) fnDeclaration() *Node {

	// Match declaration
	_ = p.consumeOrSkip(keyword)
	name := p.consumeOrSkip(fnName)
	_ = p.consumeOrSkip(fnArgsStart)
	_ = p.consumeOrSkip(fnArgsEnd)
	_ = p.consumeOrSkip(fnBodyStart)

	// Match calls
	var fnCalls []*Node
	for {
		if p.match(fnName) {
			fnCalls = append(fnCalls, p.fnCall())
		} else if p.match(fnBodyEnd) {
			p.consume()
			break
		} else {
			p.syntaxError(kindValues[fnName], kindValues[fnBodyEnd])
		}
	}
	return &Node{name, nil, nil, fnCalls, opFuncDcl}
}

func (p *Parser) fnCall() *Node {
	name := p.consumeOrSkip(fnName)
	_ = p.consumeOrSkip(fnArgsStart)
	args := p.fnArgs()
	_ = p.consumeOrSkip(fnArgsEnd)
	return &Node{name, nil, nil, args, opFuncCall}
}

func (p *Parser) fnRestArgs() (args []*Node) {
	// Match rest args or end of args
	for {
		if p.match(argsSeperator) {
			p.consume()

			// Must be an argument next
			arg := p.consumeOrSkip(strLit)
			args = append(args, &Node{arg, nil, nil, nil, opStrLit})
		} else if p.match(fnArgsEnd) {
			break
		} else {
			p.syntaxError(kindValues[fnArgsEnd], kindValues[argsSeperator])
		}
	}
	return args
}

func (p *Parser) fnArgs() (args []*Node) {
	// Match none or some args
	for {
		if p.match(fnArgsEnd) {
			break
		} else if p.match(strLit) {

			// Match first arg
			arg := p.consume()
			args = append(args, &Node{arg, nil, nil, nil, opStrLit})

			// Match rest of args
			args = append(args, p.fnRestArgs()...)
			break
		} else {
			p.syntaxError(kindValues[fnArgsEnd], kindValues[fnArgsEnd])
		}
	}
	return args
}

func (p *Parser) consumeOrSkip(kind string) *Token {
	// Attempt to match until we find
	ok := p.match(kind)
	for !ok {
		p.syntaxError(kindValues[kind])
		ok = p.match(kind)
	}
	return p.consume()
}

func (p *Parser) consume() *Token {
	// Panic if unexpectedly no more input
	if (p.pos+1 >= len(p.tokens)) {
		panic(errUnexpectedEof)
	}
	token := p.tokens[p.pos]
	p.pos++
	return token
}

func (p *Parser) match(kind string) bool {
	ok := p.tokens[p.pos].kind == kind
	if ok {
		p.discard = false // Clear discard mode
	}
	return ok
}

func (p *Parser) syntaxError(expected...string) {
	if !p.discard {
		// Enable discard mode
		p.discard = true

		// Store error
		token := p.tokens[p.pos]
		p.errs = append(p.errs,
			errors.New(fmt.Sprintf(syntaxErrMsg,
				token.line,
				token.pos,
				p.tokens[p.pos].val,
				strings.Join(expected, "' or '"))))
	}
	p.consume()
}
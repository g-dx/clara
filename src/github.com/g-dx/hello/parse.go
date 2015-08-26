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
}

var errNoMatch = errors.New("No match")

func NewParser(tokens []*Token) *Parser {
	return &Parser{tokens : tokens}
}

func noMatch(err error) bool {
	return err == errNoMatch
}

func (p *Parser) program() (errs []error, root *Node) {

	// Create root node
	root = &Node{nil, nil, nil, nil, opRoot}

	// Loop until EOF
	var err error
	var node *Node
	for
	{
		if p.match(tokenEOF) {
			break;
		} else if p.match(keyword) {
			err, node = p.fnDeclaration()
			if err == nil {
				root.Add(node)
			} else {
				errs = append(errs, err)
				for !noMatch(err) && !p.match(tokenEOF) {
					// Move to next token & try again
					p.consume()
					err, node = p.fnDeclaration()
				}
			}
		} else {
			p.syntaxError([]string{"'fn'", "<EOF>"})
		}
	}
	return errs, root
}

func (p *Parser) syntaxError(expected []string) {
	token := p.tokens[p.pos]
	panic(errors.New(fmt.Sprintf("%d:%d: syntax error, Unexpected '%v', Expecting: %v",
		token.line,
		token.pos,
		p.tokens[p.pos].val,
		strings.Join(expected, " or "))))
}

func (p *Parser) fnDeclaration() (err error, fn *Node) {

	// Catch match panics and return error
	defer func() {
		if r := recover(); r != nil {
			err = r.(error)
		}
	}()

	// Consume tokens
	_ = p.consumeOrPanic(keyword)
	name := p.consumeOrPanic(fnName)
	_ = p.consumeOrPanic(fnArgsStart)
	_ = p.consumeOrPanic(fnArgsEnd)
	_ = p.consumeOrPanic(fnBodyStart)

	var fnCalls []*Node
	for {
		if p.match(fnName) {
			fnCalls = append(fnCalls, p.fnCall())
		} else if p.match(fnBodyEnd) {
			p.consume()
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
	name := p.consumeOrPanic(fnName)
	_ = p.consumeOrPanic(fnArgsStart)
	args := p.fnArgs()
	_ = p.consumeOrPanic(fnArgsEnd)
	return &Node{name, nil, nil, args, opFuncCall}
}

func (p *Parser) fnArgs() []*Node {
	var args []*Node
	if p.match(fnArgsEnd) {
		return args
	}

	// Match first argument
	arg := p.consumeOrPanic(strLit)
	args = append(args, &Node{arg, nil, nil, nil, opStrLit})

	// Match all remaining args
	for {
		if p.match(argsSeperator) {
			_ = p.consumeOrPanic(argsSeperator)
			arg = p.consumeOrPanic(strLit)
			args = append(args, &Node{arg, nil, nil, nil, opStrLit})
		} else if p.match(fnArgsEnd) {
			break
		} else {
			p.syntaxError([]string{"')'", "','"})
		}
	}
	return args
}

func (p *Parser) consumeOrPanic(kind string) (*Token) {
	ok := p.match(kind)
	if !ok {
		panic(errNoMatch)
	}
	token := p.tokens[p.pos]
	p.consume()
	return token
}

func (p *Parser) consume() {
	// Move forward if possible
	if (p.pos+1 < len(p.tokens)) {
		p.pos++
	}
}

func (p *Parser) match(kind string) bool {
	return p.tokens[p.pos].kind == kind
}

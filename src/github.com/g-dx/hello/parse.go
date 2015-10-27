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
	errRedeclaredMsg = "%d:%d: error, '%v' redeclared"
	errUndefinedMsg = "%d:%d: error, '%v' undefined"
	errArgCountMsg = "%d:%d: error, wrong argument count to call '%v'"
)

type Parser struct {
	pos    int
	tokens []*Token
	errs   []error
	discard bool // Are we in "discard" mode?
	symtab SymTab
}

var errUnexpectedEof = errors.New("Unexpected EOF")

func NewParser(tokens []*Token) *Parser {
	return &Parser{tokens : tokens, symtab: NewSymtab()}
}

func (p *Parser) Parse() (errs []error, root *Node) {

    // We only recover from unexpected EOF
	defer func() {
		if r := recover(); r != nil {
			errs = p.errs
			if r != errUnexpectedEof {
                panic(r)
            }
		}
	}()

    // Create root & loop
    root = &Node{token : &Token{"", "ROOT", 0, 0}, op : opRoot}
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
	return p.errs, root
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
	return p.fnDclNode(name, fnCalls)
}

func (p *Parser) fnDclNode(token *Token, fnCalls []*Node) *Node {

	// Check symtab for redeclare
	sym, found := p.symtab.Resolve(symFnDecl, token.val)
	if found {
		p.symbolError(errRedeclaredMsg, token)
	} else {
		sym = &Function{token.val, 0, 0}
		p.symtab.Define(sym) // Functions don't take params yet
	}
	return &Node{token : token, stats : fnCalls, op : opFuncDcl, sym : sym}
}

func (p *Parser) fnCall() *Node {
	name := p.consumeOrSkip(fnName)
	_ = p.consumeOrSkip(fnArgsStart)
	args := p.fnArgs()
	_ = p.consumeOrSkip(fnArgsEnd)
	return p.fnCallNode(name, args)
}

func (p *Parser) fnCallNode(token *Token, args []*Node) *Node {
	return &Node{token : token, stats : args, op : opFuncCall}
}

func (p *Parser) fnRestArgs() (args []*Node) {
	// Match rest args or end of args
	for {
		if p.match(argsSeparator) {
			p.consume()

			// Must be an argument next
			arg := p.consumeOrSkip(strLit)

			// Define symbol
			if _, found := p.symtab.Resolve(symStrLit, arg.val); !found {
				p.symtab.Define(&StringLiteralSymbol{ val : arg.val })
			}
			args = append(args, &Node{token : arg, op : opStrLit})
		} else if p.match(fnArgsEnd) {
			break
		} else {
			p.syntaxError(kindValues[fnArgsEnd], kindValues[argsSeparator])
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
			// Define symbol
			if _, found := p.symtab.Resolve(symStrLit, arg.val); !found {
				p.symtab.Define(&StringLiteralSymbol{ val : arg.val })
			}
			args = append(args, &Node{token : arg, op : opStrLit})

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
		p.consume()
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

func (p *Parser) symbolError(err string, token *Token) {
	p.errs = append(p.errs,
		errors.New(fmt.Sprintf(err,
			token.line,
			token.pos,
			token.val)))
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
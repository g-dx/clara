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
	errNotFuncMsg = "%d:%d: error, '%v' is not a function"
	errArgCountMsg = "%d:%d: error, wrong argument count to call '%v'"
)

type Parser struct {
	pos    int
	tokens []*Token
	errs   []error
	discard bool // Are we in "discard" mode?
	symtab SymTab
	extra []*Node
}

var errUnexpectedEof = errors.New("Unexpected EOF")

func NewParser(tokens []*Token, extra []*Node) *Parser {
	// Add any symbols from predefined nodes
	symtab := NewSymtab()
	for _, n := range extra {
		symtab.Define(n.sym)
	}
	return &Parser{tokens : tokens, symtab: symtab, extra : extra}
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
	root = &Node{op : opRoot}
	for
	{
		if p.match(kindEOF) {
			break;
		} else if p.match(kindFn) {
			root.Add(p.fnDeclaration())
		} else {
			p.syntaxError(kindValues[kindFn], kindValues[kindEOF])
		}
	}

	// Add extra nodes
	for _, n := range p.extra {
		root.Add(n)
	}
	return p.errs, root
}

func (p *Parser) fnDeclaration() *Node {

	// Match declaration
	_ = p.consumeOrSkip(kindFn)
	name := p.consumeOrSkip(kindIdentifier)
	_ = p.consumeOrSkip(kindLeftParen)
	_ = p.consumeOrSkip(kindRightParen)
	_ = p.consumeOrSkip(kindLeftBrace)

	// Match calls
	var fnCalls []*Node
	for {
		if p.match(kindIdentifier) {
			fnCalls = append(fnCalls, p.fnCall())
		} else if p.match(kindRightBrace) {
			p.consume()
			break
		} else {
			p.syntaxError(kindValues[kindIdentifier], kindValues[kindRightBrace])
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
	name := p.consumeOrSkip(kindIdentifier)
	_ = p.consumeOrSkip(kindLeftParen)
	args := p.fnArgs()
	_ = p.consumeOrSkip(kindRightParen)
	return p.fnCallNode(name, args)
}

func (p *Parser) fnCallNode(token *Token, args []*Node) *Node {
	// TODO: TEMPORARY WORKAROUND!
	sym, _ := p.symtab.Resolve(symFnDecl, token.val)
	return &Node{token : token, stats : args, op : opFuncCall, sym : sym}
}

func (p *Parser) fnRestArgs() (args []*Node) {
	// Match rest args or end of args
	for {
		if p.match(kindComma) {
			p.consume()

			// Must be an argument next
			arg := p.consumeOrSkip(kindString)

			// Define symbol
			sym, found := p.symtab.Resolve(symStrLit, arg.val)
			if !found {
				sym = &StringLiteralSymbol{ val : arg.val }
				p.symtab.Define(sym)
			}
			args = append(args, &Node{token : arg, op : opStrLit, sym : sym})
		} else if p.match(kindRightParen) {
			break
		} else {
			p.syntaxError(kindValues[kindRightParen], kindValues[kindComma])
		}
	}
	return args
}

func (p *Parser) fnArgs() (args []*Node) {
	// Match none or some args
	for {
		if p.match(kindRightParen) {
			break
		} else if p.match(kindString) {

			// Match first arg
			arg := p.consume()
			// Define symbol
			sym, found := p.symtab.Resolve(symStrLit, arg.val)
			if !found {
				sym = &StringLiteralSymbol{ val : arg.val }
				p.symtab.Define(sym)
			}
			args = append(args, &Node{token : arg, op : opStrLit, sym : sym})

			// Match rest of args
			args = append(args, p.fnRestArgs()...)
			break
		} else {
			p.syntaxError(kindValues[kindRightParen], kindValues[kindString])
		}
	}
	return args
}

func (p *Parser) consumeOrSkip(kind lexKind) *Token {
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

func (p *Parser) match(kind lexKind) bool {
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
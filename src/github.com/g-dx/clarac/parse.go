package main

import (
	"errors"
	"fmt"
	"strings"
	"strconv"
)

const (
	syntaxErrMsg = "%v:%d:%d: syntax error, Unexpected '%v', Expecting: '%v'"
	errRedeclaredMsg = "%v:%d:%d: error, '%v' redeclared"
	errUndefinedMsg = "%v:%d:%d: error, '%v' undefined"
	errNotFuncMsg = "%v:%d:%d: error, '%v' is not a function"
	errArgCountMsg = "%v:%d:%d: error, wrong argument count to call '%v'"
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

	// Setup handler to recover from unexpected EOF
	defer p.onUnexpectedEof(&errs)

	// Create root & add extra nodes
	root = &Node{op : opRoot}
	for _, n := range p.extra {
		root.Add(n)
	}

	// Loop over stream
	for p.isNot(kindEOF) {
		if p.is(kindFn) {
			root.Add(p.fnDeclaration())
		} else {
			p.syntaxError(kindFn, kindEOF)
			p.next()
		}
	}
	p.need(kindEOF)
	return p.errs, root
}

func (p *Parser) fnDeclaration() *Node {

	// Match declaration
	p.need(kindFn)
	name := p.need(kindIdentifier)
	p.need(kindLeftParen)
	p.need(kindRightParen)
	p.need(kindLeftBrace)

	// Match calls
	var fnCalls []*Node
	for p.isNot(kindRightBrace) {
		if p.is(kindIdentifier) {
			fnCalls = append(fnCalls, p.fnCall())
		} else {
			p.syntaxError(kindIdentifier, kindRightBrace)
			p.next()
		}
	}
	p.need(kindRightBrace)
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
	return p.fnCallNode(p.need(kindIdentifier), p.parseArgs())
}

func (p *Parser) fnCallNode(token *Token, args []*Node) *Node {
	// TODO: TEMPORARY WORKAROUND!
	sym, _ := p.symtab.Resolve(symFnDecl, token.val)
	return &Node{token : token, stats : args, op : opFuncCall, sym : sym}
}

// next()            - pull in next token
// is(t) bool        - t == current
// match(t) bool     - t == current, next() if true, false otherwise
// matches(...t) t   - t == current, next() if true, false otherwise
// need(t) void      - !match() scan to ??

func (p *Parser) need(k lexKind) *Token {
	for !p.is(k) {
		fmt.Printf(kindValues[p.tokens[p.pos].kind])
		p.syntaxError(k)
		p.next()
	}
	p.discard = false
	return p.next()
}

func (p *Parser) isNot(kinds...lexKind) bool {
	for _, k := range kinds {
		if p.is(k) {
			return false
		}
	}
	return true
}

func (p *Parser) is(kinds...lexKind) bool {
	for _, kind := range kinds {
		if p.tokens[p.pos].kind == kind {
			return true
		}
	}
	return false
}

func (p *Parser) match(k lexKind) bool {
	if p.is(k) {
		p.next()
		p.discard = false
		return true
	}
	return false
}

func (p *Parser) parseArgs() (n []*Node) {
	p.need(kindLeftParen)
	if p.isNot(kindEOF, kindRightParen) {
		n = append(n, p.parseArg())
		for p.match(kindComma) {
			n = append(n, p.parseArg())
		}
	}
	p.need(kindRightParen)
	return n
}

func (p *Parser) parseArg() (*Node) {
	switch p.tokens[p.pos].kind {
	case kindInteger:
		// Match first arg
		arg := p.next()
		// Define symbol
		sym, found := p.symtab.Resolve(symIntegerLit, arg.val)
		if !found {
			i, err := strconv.ParseInt(arg.val, 10, 64)
			if err != nil {
				panic(err) // Should never happen
			}
			sym = &IntegerLiteralSymbol{val : i }
			p.symtab.Define(sym)
		}
		return &Node{token : arg, op : opIntegerLit, sym : sym}
	case kindString:

		// Match first arg
		arg := p.next()

		// Define symbol
		sym, found := p.symtab.Resolve(symStrLit, arg.val)
		if !found {
			sym = &StringLiteralSymbol{val : arg.val }
			p.symtab.Define(sym)
		}
		return &Node{token : arg, op : opStrLit, sym : sym}
	default:
		p.syntaxError(kindInteger, kindString)
		p.next()
		return nil
	}
}

func (p *Parser) next() *Token {
	// Panic if unexpectedly no more input
	if (p.pos+1 >= len(p.tokens)) {
		fmt.Println("EoF!")
		panic(errUnexpectedEof)
	}
	token := p.tokens[p.pos]
	p.pos++
	return token
}

func (p *Parser) symbolError(err string, token *Token) {
	p.errs = append(p.errs,
		errors.New(fmt.Sprintf(err,
			token.file,
			token.line,
			token.pos,
			token.val)))
}

func (p *Parser) syntaxError(expected...lexKind) {
	if !p.discard {
		// Enable discard mode
		p.discard = true

		// Gather values
		expectedValues := make([]string, 0, 2)
		for _, v := range expected {
			expectedValues = append(expectedValues, kindValues[v])
		}

		// Store error
		token := p.tokens[p.pos]
		p.errs = append(p.errs,
			errors.New(fmt.Sprintf(syntaxErrMsg,
				token.file,
				token.line,
				token.pos,
				p.tokens[p.pos].val,
				strings.Join(expectedValues, "' or '"))))
	}
}

func (p *Parser) onUnexpectedEof(errs *[]error) {
	if r := recover(); r != nil {
		*errs = p.errs
		if r != errUnexpectedEof {
			panic(r)
		}
	}
}

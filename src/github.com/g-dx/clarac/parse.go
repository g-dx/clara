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
	defer p.recoverUnexpectedEof(&errs)

	// Create root & loop
	root = &Node{op : opRoot}
	for
	{
		if p.match(kindEOF) {
			break;
		} else if p.match(kindFn) {
			root.Add(p.fnDeclaration())
		} else {
			p.syntaxError(kindFn, kindEOF)
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
	p.consumeOrSkip(kindFn)
	name := p.consumeOrSkip(kindIdentifier)
	p.consumeOrSkip(kindLeftParen)
	p.consumeOrSkip(kindRightParen)
	p.consumeOrSkip(kindLeftBrace)

	// Match calls
	var fnCalls []*Node
	for {
		if p.match(kindIdentifier) {
			fnCalls = append(fnCalls, p.fnCall())
		} else if p.match(kindRightBrace) {
			p.consume()
			break
		} else {
			p.syntaxError(kindIdentifier, kindRightBrace)
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
	args := p.parseArgs()
	return p.fnCallNode(name, args)
}

func (p *Parser) fnCallNode(token *Token, args []*Node) *Node {
	// TODO: TEMPORARY WORKAROUND!
	sym, _ := p.symtab.Resolve(symFnDecl, token.val)
	return &Node{token : token, stats : args, op : opFuncCall, sym : sym}
}

//func (p *Parser) fnArgs() (args []*Node) {
//
//	parsedArg := false
//	for {
//		if p.match(kindComma) {
//			if !parsedArg {
//				p.syntaxError(kindInteger, kindString)
//				p.consume()
//				fmt.Printf("Unexpected comma\n")
//				continue
//			}
//			fmt.Printf("Parsed comma\n")
//			p.consume()
//			parsedArg = false
//
//		} else if p.match(kindRightParen) {
//			return args
//		} else {
//			// Parse argument
//			arg := p.fnArg()
//			if arg != nil {
//				args = append(args, arg)
//				parsedArg = true
//			}
//		}
//	}
//}

// next()            - pull in next token
// is(t) bool        - t == current
// match(t) bool     - t == current, next() if true, false otherwise
// matches(...t) t   - t == current, next() if true, false otherwise
// matchOrIs(m, ...s) - match(m) || is(s) return true, else false (syntax error)
// need(t) void      - !match() scan to ??

func (p *Parser) need(k lexKind) {
	p.consumeOrSkip(k)
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

func (p *Parser) match2(k lexKind) bool {
	if p.is(k) {
		p.consume()
		return true
	}
	return false
}

func (p *Parser) parseArgs() (n []*Node) {
	p.need(kindLeftParen)
	for p.isNot(kindEOF, kindRightParen) {
		n = append(n, p.parseArg())
		for p.match2(kindComma) {
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
		arg := p.consume()
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
		arg := p.consume()

		// Define symbol
		sym, found := p.symtab.Resolve(symStrLit, arg.val)
		if !found {
			sym = &StringLiteralSymbol{val : arg.val }
			p.symtab.Define(sym)
		}
		return &Node{token : arg, op : opStrLit, sym : sym}
	default:
		p.syntaxError2(kindInteger, kindString)
		return nil
	}
}

func (p *Parser) consumeOrSkip(kind lexKind) *Token {
	// Attempt to match until we find
	ok := p.match(kind)
	for !ok {
		p.syntaxError(kind)
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
			token.file,
			token.line,
			token.pos,
			token.val)))
}

func (p *Parser) syntaxError2(expected...lexKind) {

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
	p.consume()
}

func (p *Parser) recoverUnexpectedEof(errs *[]error) {
	if r := recover(); r != nil {
		errs = &p.errs
		if r != errUnexpectedEof {
			panic(r)
		}
	}
}

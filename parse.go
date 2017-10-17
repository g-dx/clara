package main

import (
	"errors"
	"fmt"
	"strings"
	"github.com/g-dx/clarac/lex"
)

const (
	syntaxErrMsg = "%v:%d:%d: syntax error, Unexpected '%v', Expecting: '%v'"
	errRedeclaredMsg = "%v:%d:%d: error, '%v' redeclared"
)

type Parser struct {
	pos    int
	tokens []*lex.Token
	errs   []error
	discard bool // Are we in "discard" mode?
	symtab *SymTab
	extra []*Node
	linker *TypeLinker
}

var errUnexpectedEof = errors.New("Unexpected EOF")

func NewParser(tokens []*lex.Token, nodes []*Node, syms []*Symbol) *Parser {
	// Add any symbols from predefined nodes
	symtab := NewSymtab()
	for _, n := range nodes {
		symtab.Define(n.sym)
	}
	// Add any global symbols
	for _, s := range syms {
		symtab.Define(s)
	}
	return &Parser{tokens : tokens, symtab: symtab, extra : nodes, linker: NewTypeLinker() }
}

func (p *Parser) Parse() (errs []error, root *Node) {

	// Setup handler to recover from unexpected EOF
	defer p.onUnexpectedEof(&errs)

	// Create root & loop over stream
	root = &Node{op : opRoot, symtab: p.symtab}
	for p.isNot(lex.EOF) {
		if p.is(lex.Fn) {
			root.Add(p.parseFnDecl())
		} else if p.is(lex.Struct) {
			root.Add(p.parseStructDecl())
		} else {
			p.syntaxError(lex.Fn, lex.EOF)
			p.next()
		}
	}

	// Add extra nodes
	// TODO: This really should come before we parse but the code generator requires them to be last
	for _, n := range p.extra {
		root.Add(n)
	}

	// Any unlinked types are undefined
	for name, _ := range p.linker.types {
		for _, token := range p.linker.tokens[name] {
			p.symbolError(errUnknownTypeMsg, token)
		}
	}
	return p.errs, root
}

// ==========================================================================================================
// Grammar-implementing functions

func (p *Parser) parseStructDecl() *Node {

	// Open new symtab
	p.symtab = p.symtab.Child()

	p.need(lex.Struct)
	id := p.need(lex.Identifier)
	p.need(lex.LBrace)

	// Parse fields
	var fields []*Node
	for p.isNot(lex.RBrace) {
		fields = append(fields, p.parseParameter())
	}
	p.need(lex.RBrace)

	// Get all symbols
	var vars []*Symbol
	for _, f := range fields {
		vars = append(vars, f.sym)
	}

	// Close symtab
	syms := p.symtab
	p.symtab = p.symtab.Parent()
	return p.structDclNode(id, syms, fields, vars)
}

func (p *Parser) parseFnDecl() *Node {

	// Open new symtab
	p.symtab = p.symtab.Child()

	// Match declaration
	p.need(lex.Fn)
	name := p.need(lex.Identifier)
	p.need('(')
	params := p.parseParameters()
	p.need(')')
	// TODO: Decide if return types are optional
	var retType *lex.Token
	if p.isNot(lex.LBrace) {
		retType = p.need(lex.Identifier)
	}
	stmts := p.parseStatements()

	// Close symtab
	syms := p.symtab
	p.symtab = p.symtab.Parent()
	return p.fnDclNode(name, params, stmts, syms, retType)
}

func (p *Parser) parseStatements() []*Node {
	p.need('{')
	var stmts []*Node
	for p.isNot('}') {
		stmts = append(stmts, p.parseStatement())
	}
	p.need('}')
	return stmts
}

func (p *Parser) parseStatement() *Node {

	switch p.Kind() {
	case lex.Return:
		return p.parseReturnExpr()
	case lex.If:
		return p.parseIfStmt()
	case lex.Identifier:
		// TODO: Assumes all identifiers beginning statements are function calls!
		return p.parseFnCall()
	default:
		p.syntaxError(lex.Identifier, lex.Return)
		p.next()
		return &Node{op: opError} // TODO: Bad statement node?
	}
}
func (p *Parser) parseIfStmt() *Node {
	// TODO: Need to handle elseif, else statements here too. When added they can be right node
	return &Node { op: opIf, token: p.need(lex.If), left: p.parseExpr(0), stmts: p.parseStatements() }
}

func (p *Parser) parseReturnExpr() *Node {
	return &Node{op:opReturn, token: p.need(lex.Return), left: p.parseExpr(0), symtab: p.symtab}
}

func (p *Parser) parseParameters() []*Node {
	var params []*Node = nil
	for p.isNot(lex.EOF, ')') {
		params = append(params, p.parseParameter())
		for p.match(',') {
			params = append(params, p.parseParameter())
		}
	}
	return params
}

func (p *Parser) parseParameter() *Node {
	idTok := p.need(lex.Identifier)
	p.need(lex.Colon)
	typeTok := p.need(lex.Identifier)

	// Attempt to resolve type
	sym, _ := p.symtab.Resolve(typeTok.Val)
	idSym := &Symbol{ Name: idTok.Val }
	if sym != nil {
		idSym.Type = sym.Type
	} else {
		// Register to be updated when/if type becomes available
		p.linker.Add(typeTok, &idSym.Type)
	}
	p.symtab.Define(idSym)
	return &Node{token: idTok, op: opIdentifier, sym: idSym, symtab: p.symtab }
}

func (p *Parser) parseArgs() (n []*Node) {
	p.need('(')
	if p.isNot(lex.EOF, ')') {
		n = append(n, p.parseArg())
		for p.match(',') {
			n = append(n, p.parseArg())
		}
	}
	p.need(')')
	return n
}

func (p *Parser) parseArg() (*Node) {
	return p.parseExpr(0)
}

//
// NOTE: The following two functions implement the "Precedence Climbing" algorithm as described here:
// https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm#climbing
//
func (p *Parser) parseExpr(prec int) (*Node) {

	t := p.parseOperand()
	for next := p.Kind(); next.IsBinaryOperator() && next.Precedence() >= prec; next = p.Kind() {
		op, tok := p.parseOperator()
		q := next.Precedence()
		if next.Associativity() == lex.Left {
			q += 1
		}
		t1 := p.parseExpr(q)
		t = &Node{ op: op, token: tok, left: t, right: t1, symtab: p.symtab }
	}
	return t
}

func (p *Parser) parseOperand() *Node {

	next := p.Kind()
	switch {
	case next.IsUnaryOperator():
		op, tok := p.parseOperator()
		t := p.parseExpr(next.Precedence())
		return &Node{ op: op, token: tok, left: t, symtab: p.symtab} // Unary operators store expr in left

	case next == lex.LParen:
		p.next()
		t := p.parseExpr(0)
		p.need(lex.RParen)
		// TODO: Do we want to record parenthesis information? If so we can do it here here with a new type of node
		return t

	case next == lex.Integer:
		return p.parseLit(intType)

	case next == lex.String:
		return p.parseLit(stringType)

	case next == lex.True || next == lex.False:
		return p.parseLit(boolType)

	case next == lex.Identifier:
		return p.parseIdentifierOrFnCall()

	default:
		// Error
		p.syntaxError(lex.LParen, lex.Integer, lex.String, lex.Identifier) // TODO: All unary operators should get add
		// TODO: What should we synchronise on?
		// Next statement?
		return &Node {op: opError, token: p.next()}
	}
}

func (p *Parser) parseIdentifierOrFnCall() *Node {
	// TODO: All logic from parseIdentifier() & parseFnCall() has been duplicated here!
	tok := p.need(lex.Identifier)
	switch p.Kind() {
	case lex.LParen:
		return p.fnCallNode(tok, p.parseArgs())
	default:
		return &Node { op: opIdentifier, token: tok, symtab: p.symtab }
	}
}

func (p *Parser) parseOperator() (int, *lex.Token) {
	switch p.Kind() {
	case lex.Not:
		return opNot, p.next()
	case lex.Dot:
		return opDot, p.next()
	case lex.Plus:
		return opAdd, p.next()
	case lex.Mul:
		return opMul, p.next()
	case lex.Div:
		return opDiv, p.next()
	case lex.Eq:
		return opEq, p.next()
	case lex.Min:
		return opMin, p.next()
	case lex.And:
		return opAnd, p.next()
	case lex.Or:
		return opOr, p.next()
	case lex.Gt:
		return opGt, p.next()
	default:
		p.syntaxError(lex.Plus)
		p.next()
		return opError, nil
	}
}

func (p *Parser) parseLit(t *Type) (*Node) {
	token := p.next()
	sym, found := p.symtab.Resolve(token.Val)
	if !found {
		sym = &Symbol{ Name: token.Val, Type: t }
		p.symtab.Define(sym)
	}
	return &Node{token : token, op : opLit, symtab: p.symtab, sym: sym }
}


// ==========================================================================================================
// AST Node functions

func (p *Parser) fnDclNode(token *lex.Token, params []*Node, stmts []*Node, syms *SymTab, returnTyp *lex.Token) *Node {

	// Check symtab for redeclare
	sym, found := p.symtab.Resolve(token.Val)
	if found {
		p.symbolError(errRedeclaredMsg, token)
	} else {
		// Define function type
		functionType := &FunctionType{Name: token.Val, ArgCount: len(params), args: syms,}
		sym = &Symbol{ Name: token.Val, Type: &Type{ Kind: Function, Data: functionType}}
		p.symtab.Define(sym) // Functions don't take params yet

		// Resolve return type
		if returnTyp == nil {
			functionType.ret = nothingType
		} else {
			retSym, found := p.symtab.Resolve(returnTyp.Val)
			if !found {
				// Register to be updated when/if type becomes available
				p.linker.Add(returnTyp, &functionType.ret)
			} else {
				functionType.ret = retSym.Type
			}
		}
	}
	return &Node{ token : token, params: params, stmts: stmts, op : opFuncDcl, sym : sym, symtab: p.symtab }
}

func (p *Parser) parseFnCall() *Node {
	return p.fnCallNode(p.need(lex.Identifier), p.parseArgs())
}

func (p *Parser) fnCallNode(token *lex.Token, args []*Node) *Node {
	// TODO: TEMPORARY WORKAROUND!
	sym, _ := p.symtab.Resolve(token.Val)
	return &Node{token : token, stmts: args, op : opFuncCall, sym : sym, symtab: p.symtab}
}

func (p *Parser) structDclNode(id *lex.Token, syms *SymTab, fields []*Node, vars []*Symbol) *Node {

	// Declare new type symbol
	sym, found := p.symtab.Resolve(id.Val)
	if found {
		p.symbolError(errRedeclaredMsg, id)
	} else {
		// Define struct symbol
		// TODO: This width calc shouldn't happen here
		sym = &Symbol{Name: id.Val, Type: &Type{ Kind: Struct, Data: &StructType{ Name: id.Val, Width: len(fields) * 8, Fields: vars }}}
		p.symtab.Define(sym)

		// Update any other nodes waiting on this type
		p.linker.Link(id, sym.Type)
	}
	return &Node{op: opStruct, token: id, symtab: syms, sym: sym, stmts: fields}
}


// ==========================================================================================================
// Matching & movement functions

func (p *Parser) need(k lex.Kind) *lex.Token {
	for !p.is(k) {
		fmt.Printf(lex.KindValues[p.tokens[p.pos].Kind])
		p.syntaxError(k)
		p.next()
	}
	p.discard = false
	return p.next()
}

func (p *Parser) isNot(kinds...lex.Kind) bool {
	for _, k := range kinds {
		if p.is(k) {
			return false
		}
	}
	return true
}

func (p *Parser) is(kinds...lex.Kind) bool {
	for _, kind := range kinds {
		if p.Kind() == kind {
			return true
		}
	}
	return false
}

func (p *Parser) Kind() lex.Kind {
	return p.tokens[p.pos].Kind
}

func (p *Parser) match(k lex.Kind) bool {
	if p.is(k) {
		p.next()
		p.discard = false
		return true
	}
	return false
}

func (p *Parser) next() *lex.Token {
	// Panic if unexpectedly no more input
	if (p.pos+1 >= len(p.tokens)) {
		panic(errUnexpectedEof)
	}
	token := p.tokens[p.pos]
	p.pos++
	return token
}

func (p *Parser) symbolError(err string, token *lex.Token) {
	p.errs = append(p.errs,
		errors.New(fmt.Sprintf(err,
			token.File,
			token.Line,
			token.Pos,
			token.Val)))
}

func (p *Parser) syntaxError(expected...lex.Kind) {
	if !p.discard {
		// Enable discard mode
		p.discard = true

		// Gather values
		expectedValues := make([]string, 0, 2)
		for _, v := range expected {
			expectedValues = append(expectedValues, lex.KindValues[v])
		}

		// Store error
		token := p.tokens[p.pos]
		p.errs = append(p.errs,
			errors.New(fmt.Sprintf(syntaxErrMsg,
				token.File,
				token.Line,
				token.Pos,
				p.tokens[p.pos].Val,
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

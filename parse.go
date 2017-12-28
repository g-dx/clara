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
	linker *TypeLinker
}

var errUnexpectedEof = errors.New("Unexpected EOF")

func NewParser() *Parser {
	// Add any symbols from predefined nodes
	return &Parser{ linker: NewTypeLinker() }
}

func (p *Parser) Parse(tokens []*lex.Token, root *Node) (errs []error) {

	// Setup handler to recover from unexpected EOF
	defer p.onUnexpectedEof(&errs)

	// Reset state
	p.symtab = root.symtab
	p.pos = 0
	p.tokens = tokens
	p.errs = p.errs[:0]
	p.discard = false

	// loop over tokens
	for p.isNot(lex.EOF) {
		if p.is(lex.Fn) {
			root.Add(p.parseFnDecl())
		} else if p.is(lex.Struct) {
			root.Add(p.parseStructDecl())
		} else {
			p.syntaxError(lex.Fn, lex.Struct, lex.EOF)
			p.next()
		}
	}
	return p.errs
}

func (p *Parser) Finish() (errs []error) {

	// Any unlinked types are undefined
	for name, _ := range p.linker.types {
		for _, token := range p.linker.tokens[name] {
			p.symbolError(errUnknownTypeMsg, token)
		}
	}
	return p.errs
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
	p.openScope()

	// Match declaration
	p.need(lex.Fn)
	name := p.need(lex.Identifier)
	p.need('(')
	params := p.parseParameters()
	p.need(')')
	// TODO: Decide if return types are optional
	var retType *lex.Token
	if p.is(lex.Identifier) {
		retType = p.next()
	}

	// Check for function body
	var stmts []*Node
	extern := false
	if p.is(lex.LBrace) {
		stmts = p.parseBlock()
	} else {
		extern = true
	}

	// Close symtab
	syms := p.closeScope()
	return p.fnDclNode(name, params, stmts, syms, retType, extern)
}

func (p *Parser) parseBlock() []*Node {
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
	case lex.Integer, lex.String, lex.Identifier, lex.True, lex.False, lex.Not, lex.LParen: // All tokens which can start an expression
		expr := p.parseExpr(0)
		if p.is(lex.Das) {
			expr = p.parseDeclAssignStmt(expr)
		} else if p.is(lex.As) {
			expr = p.parseAssignStmt(expr)
		}
		return expr
	default:
		// TODO: Maybe a better message would be "keyword or expression" expected
		p.syntaxError(lex.Identifier, lex.Return, lex.If, lex.Integer, lex.String, lex.Identifier, lex.True,
			lex.False, lex.Not, lex.LParen)
		return &Node{op: opError, token: p.next()} // TODO: Bad statement node?
	}
}

func (p *Parser) parseDeclAssignStmt(lhs *Node) *Node {

	sym, found := p.symtab.Resolve(lhs.token.Val)
	if !found {
		sym = &Symbol{ Name: lhs.token.Val, IsStack: true }
		p.symtab.Define(sym)
	} else {
		p.symbolError(errRedeclaredMsg, lhs.token)
	}
	lhs.sym = sym
	lhs.typ = sym.Type

	return &Node{ op: opDas, token: p.need(lex.Das), left: lhs, right: p.parseExpr(0), symtab: p.symtab }
}

func (p *Parser) parseAssignStmt(lhs *Node) *Node {
	return &Node{ op: opAs, token: p.need(lex.As), left: lhs, right: p.parseExpr(0), symtab: p.symtab }
}

func (p *Parser) parseIfStmt() *Node {
	tok := p.need(lex.If)
	cond := p.parseExpr(0)
	p.openScope()
	ifStmt := &Node { op: opIf, token: tok, left: cond, stmts: p.parseBlock(), symtab: p.closeScope() }
	p.parseElseStmt(p.parseElseIfStmt(ifStmt))
	return ifStmt
}

func (p *Parser) parseElseIfStmt(n *Node) *Node {
	for p.is(lex.ElseIf) {
		tok := p.need(lex.ElseIf)
		cond := p.parseExpr(0)
		p.openScope()
		n.right = &Node { op: opElseIf, token: tok, left: cond, stmts: p.parseBlock(), symtab: p.closeScope() }
		n = n.right
	}
	return n
}

func (p *Parser) parseElseStmt(n *Node) {
	if p.is(lex.Else) {
		p.openScope()
		n.right = &Node { op: opElse, token: p.need(lex.Else), stmts: p.parseBlock(), symtab: p.closeScope() }
	}
}

func (p *Parser) parseReturnExpr() *Node {
	ret := p.need(lex.Return)
	var expr *Node
	// TODO: This check for an operand may come up in other places
	if p.is(lex.LParen, lex.Integer, lex.String, lex.Identifier, lex.Not) {
		expr = p.parseExpr(0)
	}
	return &Node{op:opReturn, token: ret, left: expr, symtab: p.symtab}
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
	isArray := false
	if p.is(lex.LBrack) {
		p.next()
		p.need(lex.RBrack)
		isArray = true
	}
	typeTok := p.need(lex.Identifier)

	// Attempt to resolve type
	sym, _ := p.symtab.Resolve(typeTok.Val)
	idSym := &Symbol{ Name: idTok.Val }
	if isArray {
		arrayType := &ArrayType{Elem: sym.Type}
		if sym != nil {
			arrayType.Elem = sym.Type
		} else {
			p.linker.Add(typeTok, &arrayType.Elem)
		}
		idSym.Type = &Type{ Kind: Array, Data: arrayType }
	} else {

		if sym != nil {
			idSym.Type = sym.Type
		} else {
			p.linker.Add(typeTok, &idSym.Type)
		}
	}

	// Check for unique parameter names
	_, found := p.symtab.Resolve(idSym.Name)
	if found {
		p.symbolError(errRedeclaredMsg, idTok)
	} else {
		p.symtab.Define(idSym)
	}
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
		return p.parseIdentifierOrFnCallOrArrayAccess()

	default:
		// Error
		p.syntaxError(lex.LParen, lex.Integer, lex.String, lex.Identifier) // TODO: All unary operators should get add
		// TODO: What should we synchronise on?
		// Next statement?
		return &Node {op: opError, token: p.next()}
	}
}

func (p *Parser) parseIdentifierOrFnCallOrArrayAccess() *Node {
	// TODO: All logic from parseIdentifier() & parseFnCall() has been duplicated here!
	tok := p.need(lex.Identifier)
	switch p.Kind() {
	case lex.LParen:
		return p.fnCallNode(tok, p.parseArgs())
	case lex.LBrack:
		return p.arrayNode(tok)
	default:
		return &Node { op: opIdentifier, token: tok, symtab: p.symtab }
	}
}

func (p *Parser) arrayNode(token *lex.Token) *Node {
	p.need(lex.LBrack)
	idx := p.parseExpr(0)
	p.need(lex.RBrack)
	id := &Node { op: opIdentifier, token: token, symtab: p.symtab }
	// TODO: Should try and resolve type now?
	return &Node { op: opArray, token: token, left: id, right: idx, symtab: p.symtab }
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
	case lex.Lt:
		return opLt, p.next()
	default:
		p.syntaxError(lex.Dot, lex.Not, lex.Plus, lex.Mul, lex.Div, lex.Eq, lex.Min, lex.And, lex.Or, lex.Gt, lex.Lt)
		p.next()
		return opError, nil
	}
}

func (p *Parser) parseLit(t *Type) (*Node) {
	token := p.next()
	sym, found := p.symtab.Resolve(token.Val)
	if !found {
		sym = &Symbol{ Name: token.Val, Type: t, IsLiteral: true  }
		p.symtab.Define(sym)
	}
	return &Node{token : token, op : opLit, symtab: p.symtab, sym: sym }
}


// ==========================================================================================================
// AST Node functions

func (p *Parser) fnDclNode(token *lex.Token, params []*Node, stmts []*Node, syms *SymTab, returnTyp *lex.Token, isExternal bool) *Node {

	// Check symtab for redeclare
	sym, found := p.symtab.Resolve(token.Val)
	if found {
		p.symbolError(errRedeclaredMsg, token)
	} else {
		// Define function type
		functionType := &FunctionType{Name: token.Val, ArgCount: len(params), args: syms, IsExternal: isExternal}
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
// Scope functions

func (p *Parser) openScope() {
	p.symtab = p.symtab.Child()
}

func (p *Parser) closeScope() *SymTab {
	syms := p.symtab
	p.symtab = p.symtab.Parent()
	return syms
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

package main

import (
	"errors"
	"fmt"
	"strings"
	"github.com/g-dx/clarac/lex"
	"io"
	"io/ioutil"
	"bytes"
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
	p.openScope()

	p.need(lex.Struct)
	id := p.need(lex.Identifier)
	p.need(lex.LBrace)

	// Parse fields
	var fields []*Node
	for off := 0; p.isNot(lex.RBrace); off += 1 {
		field := p.parseParameter(ioutil.Discard)
		field.sym.Addr = off * ptrSize // Set field offset
		fields = append(fields, field)
	}
	p.need(lex.RBrace)

	// Get all symbols
	var vars []*Symbol
	for _, f := range fields {
		vars = append(vars, f.sym)
	}

	// Close symtab
	syms := p.closeScope()
	return p.structDclNode(id, syms, fields, vars)
}

func (p *Parser) parseFnDecl() *Node {

	// Open new symtab
	p.openScope()

	// Match declaration
	p.need(lex.Fn)
	name := p.need(lex.Identifier)

	// Create fn description
	w := bytes.NewBufferString(name.Val)
	w.WriteString(p.need('(').Val)
	params := p.parseParameters(w)
	w.WriteString(p.need(')').Val)

	fnType := &FunctionType{} // NOTE: Parameter types are added during type check!

	// Parse return types (if any)
	if p.is(lex.LBrack, lex.Fn, lex.Identifier) {
		w.WriteString(" ")
		p.parseType(&fnType.ret, w)
	} else {
		fnType.ret = nothingType
	}

	// Check for function body
	var stmts []*Node
	if p.is(lex.LBrace) {
		stmts = p.parseBlock()
	} else {
		fnType.IsExternal = true
	}

	// Close symtab
	syms := p.closeScope()
	return p.fnDclNode(name, params, stmts, syms, &Type{ Kind: Function, Data: fnType }, w.String())
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
	case lex.While:
		return p.parseWhileStmt()
	default:
		// TODO: Maybe a better message would be "keyword or expression" expected
		p.syntaxError(lex.Identifier, lex.Return, lex.If, lex.Integer, lex.String, lex.Identifier, lex.True,
			lex.False, lex.Not, lex.LParen)
		return &Node{op: opError, token: p.next()} // TODO: Bad statement node?
	}
}

func (p *Parser) parseWhileStmt() *Node {
	tok := p.need(lex.While)
	cond := p.parseExpr(0)
	p.openScope()
	return &Node { op: opWhile, token: tok, left: cond, stmts: p.parseBlock(), symtab: p.closeScope() }
}

func (p *Parser) parseDeclAssignStmt(lhs *Node) *Node {

	sym, ok := p.symtab.Define(&Symbol{ Name: lhs.token.Val, IsStack: true })
	if ok {
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
	if p.is(lex.LParen, lex.Integer, lex.True, lex.False, lex.String, lex.Identifier, lex.Not) {
		expr = p.parseExpr(0)
	}
	return &Node{op:opReturn, token: ret, left: expr, symtab: p.symtab}
}

func (p *Parser) parseParameters(w io.Writer) []*Node {
	var params []*Node = nil
	for p.isNot(lex.EOF, ')') {
		params = append(params, p.parseParameter(w))
		for p.match(',') {
			io.WriteString(w, ", ")
			params = append(params, p.parseParameter(w))
		}
	}
	return params
}

func (p *Parser) parseParameter(w io.Writer) *Node {

	// Handle identifier and create symbol
	name := p.need(lex.Identifier)
	s := &Symbol{ Name: name.Val }

	// Handle type
	p.need(lex.Colon)
	p.parseType(&s.Type, w)

	// Check for unique parameter names
	s, ok := p.symtab.Define(s)
	if ok {
		p.symbolError(errRedeclaredMsg, name)
	}

	return &Node{token: name, op: opIdentifier, sym: s, symtab: p.symtab }
}

func (p *Parser) parseType(t **Type, w io.Writer) {

	switch p.Kind() {
	case lex.LBrack:
		io.WriteString(w, p.next().Val)
		io.WriteString(w, p.need(lex.RBrack).Val)

		arrayType := &ArrayType{}
		p.parseType(&arrayType.Elem, w)
		*t = &Type{ Kind: Array, Data: arrayType }

	case lex.Identifier:
		token := p.next()
		io.WriteString(w, token.Val)
		if s, ok := p.symtab.Resolve(token.Val); ok {
			*t = s.Type
		} else {
			p.linker.Add(token, t)
		}

	case lex.Fn:
		io.WriteString(w, p.next().Val)
		io.WriteString(w, p.need('(').Val)

		funcType := &FunctionType{Args: make([]*Type, 20) } // TODO: Max of 20 parameters here!
		args := 0
		for p.isNot(lex.EOF, ')') {
			p.parseType(&funcType.Args[args], w)
			for args += 1; p.match(','); args += 1 {
				p.parseType(&funcType.Args[args], w)
				io.WriteString(w, ", ")
			}
		}
		funcType.Args = funcType.Args[:args] // Trim empty slots
		io.WriteString(w, p.need(')').Val)

		// Check for return type
		if p.is(lex.Fn, lex.Identifier, lex.LBrack) {
			p.parseType(&funcType.ret, w)
		} else {
			funcType.ret = nothingType
		}

		*t = &Type{ Kind: Function, Data: funcType}

	default:
		p.syntaxError(lex.Fn, lex.Identifier, lex.LBrack)
	}
}

func (p *Parser) parseArgs() (n []*Node, lparen *lex.Token) {
	lparen = p.need('(')
	if p.isNot(lex.EOF, ')') {
		n = append(n, p.parseArg())
		for p.match(',') {
			n = append(n, p.parseArg())
		}
	}
	p.need(')')
	return n, lparen
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
		op, tok := p.parseOperator(false)
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
		op, tok := p.parseOperator(true)
		t := p.parseExpr(tok.Kind.Precedence()) // NOTE: must use kind defined in token as may be different from next
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
		return p.parseIdentifier()

	default:
		// Error
		p.syntaxError(lex.LParen, lex.Integer, lex.String, lex.Identifier) // TODO: All unary operators should get add
		// TODO: What should we synchronise on?
		// Next statement?
		return &Node {op: opError, token: p.next()}
	}
}

func (p *Parser) parseIdentifier() *Node {

	id := p.need(lex.Identifier)
	var left *Node
	switch p.Kind() {
	case lex.LParen:
		args, _ := p.parseArgs()
		left = &Node { op : opFuncCall, token: id, left: left, stmts: args, symtab: p.symtab }
	case lex.LBrack:
		idx, _ := p.parseIndex()
		array := &Node { op: opIdentifier, token: id, symtab: p.symtab }
		left = &Node { op: opArray, token: id, left: array, right: idx, symtab: p.symtab }
	default:
		left = &Node { op: opIdentifier, token: id, symtab: p.symtab }
	}

	// Check for further calls or index operators
	for p.is(lex.LParen, lex.LBrack) {
		switch p.Kind() {
			case lex.LParen:
				args, lparen := p.parseArgs()
				left = &Node { op : opFuncCall, token: lparen, left: left, stmts: args, symtab: p.symtab }
			case lex.LBrack:
				index, lbrack := p.parseIndex()
				left = &Node { op: opArray, token: lbrack, left: left, right: index, symtab: p.symtab }
		}
	}
	return left
}

func (p *Parser) parseIndex() (*Node, *lex.Token) {
	lbrack := p.need(lex.LBrack)
	idx := p.parseExpr(0)
	p.need(lex.RBrack)
	return idx, lbrack
}

func (p *Parser) parseOperator(isUnary bool) (int, *lex.Token) {
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
		// Rewrite token to differentiate binary minus from unary minus
		if isUnary {
			t := p.next()
			t.Kind = lex.Neg
			return opNeg, t
		}
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
	sym, _ := p.symtab.Define(&Symbol{ Name: token.Val, Type: t, IsLiteral: true })
	return &Node{token : token, op : opLit, symtab: p.symtab, sym: sym }
}


// ==========================================================================================================
// AST Node functions

func (p *Parser) fnDclNode(token *lex.Token, params []*Node, stmts []*Node, symTab *SymTab, fnType *Type, typedFn string) *Node {

	// Check symtab for redeclare
	sym, ok := p.symtab.Resolve(typedFn)
	if ok {
		p.symbolError(errRedeclaredMsg, lex.WithVal(token, typedFn))
	} else {
		// Define "marker" symbol
		p.symtab.Define(&Symbol{Name: typedFn, Type: fnType})

		// Define function type
		sym = &Symbol{Name: token.Val, Type: fnType, IsGlobal: true}

		// Check symtab; define and link to existing symbol if already present
		if s, ok := p.symtab.Define(sym); ok {
			for ; s.Next != nil; s = s.Next { /* ... */ }
			s.Next = sym
		}
	}
	return &Node{ token : token, params: params, stmts: stmts, op : opFuncDcl, sym : sym, symtab: symTab}
}

func (p *Parser) structDclNode(id *lex.Token, syms *SymTab, fields []*Node, vars []*Symbol) *Node {

	// Declare new type symbol
	sym, ok := p.symtab.Define(&Symbol{Name: id.Val, Type: &Type{ Kind: Struct, Data: &StructType{ Name: id.Val, Fields: vars }}})
	if ok {
		p.symbolError(errRedeclaredMsg, id)
	} else {
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

package main

import (
	"errors"
	"fmt"
	"github.com/g-dx/clarac/lex"
)

const errSyntaxMsg = "%v:%d:%d: syntax error, Unexpected '%v', expected: '%v'"

type Parser struct {
	pos    int
	tokens []*lex.Token
	errs   []error
	discard bool // Are we in "discard" mode?
}

var errUnexpectedEof = errors.New("unexpected EOF")

func NewParser() *Parser {
	return &Parser{}
}

func (p *Parser) Parse(tokens []*lex.Token, root *Node) (errs []error) {

	// Setup handler to recover from unexpected EOF
	defer p.onUnexpectedEof(&errs)

	// Reset state
	p.pos = 0
	p.tokens = tokens
	p.errs = p.errs[:0]
	p.discard = false

	// loop over tokens
	for p.isNot(lex.EOF) {
		switch p.Kind() {
		case lex.Fn:
			root.Add(p.parseFn())

		case lex.Struct:
			root.Add(p.parseStruct())

		default:
			p.syntaxError(lex.KindValues[lex.Fn] + " or " + lex.KindValues[lex.Struct])
			p.next()
			// TODO: p.sync(lex.Fn, lex.Struct)
		}
	}
	p.need(lex.EOF)
	return p.errs
}

func (p *Parser) parseStruct() *Node {
	p.need(lex.Struct)
	id := p.need(lex.Identifier)
	p.need(lex.LBrace)
	var fields []*Node
	for p.isNot(lex.RBrace) {
		fields = append(fields, p.parseParameter())
	}
	p.need(lex.RBrace)
	return &Node{op: opStructDcl, token: id, stmts: fields}
}

func (p *Parser) parseFn() *Node {
	p.need(lex.Fn)
	n := &Node{op: opBlockFnDcl, token: p.need(lex.Identifier), params: p.parseParameters()}
	if p.is(lex.Fn, lex.LBrack, lex.Identifier) {
		n.left = p.parseType()
	}
	switch p.Kind() {
	case lex.LBrace:
		n.stmts = p.parseBlock()
	case lex.As:
		p.next()
		n.op = opExprFnDcl
		n.stmts = []*Node{p.parseExpr(0)}
	default:
		n.op = opExternFnDcl
	}
	return n
}

func (p *Parser) parseBlock() (block []*Node) {
	p.need(lex.LBrace)
	for p.isNot(lex.RBrace) {
		block = append(block, p.parseStatement())
	}
	p.need(lex.RBrace)
	return block
}

func (p *Parser) parseStatement() *Node {
	kind := p.Kind()
	switch {
	case kind == lex.Return:
		return p.parseReturn()

	case kind == lex.While:
		return p.parseWhile()

	case kind == lex.If:
		return p.parseIf()

	case kind.IsExprStart():
		expr := p.parseExpr(0)
		switch p.Kind() {
		case lex.As:
			return p.parseAssignment(expr)
		case lex.Das:
			return p.parseDeclarationAssignment(expr)
		default:
			return expr
		}

	default:
		p.syntaxError("<statement>")
		return &Node{op: opError, token: p.next()}
	}
}

func (p *Parser) parseReturn() *Node {
	n := &Node{op: opReturn, token: p.need(lex.Return)}
	if p.Kind().IsExprStart() {
		n.left = p.parseExpr(0)
	}
	return n
}

func (p *Parser) parseWhile() *Node {
	return &Node{op: opWhile, token: p.need(lex.While), left: p.parseExpr(0), stmts: p.parseBlock()}
}

func (p *Parser) parseIf() *Node {
	n := &Node{op: opIf, token: p.need(lex.If), left: p.parseExpr(0), stmts: p.parseBlock()}
	cur := n
	for ; p.is(lex.ElseIf); cur = cur.right {
		cur.right = &Node{op: opElseIf, token: p.need(lex.ElseIf), left: p.parseExpr(0), stmts: p.parseBlock()}
	}
	if p.is(lex.Else) {
		cur.right = &Node{op: opElse, token: p.need(lex.Else), stmts: p.parseBlock()}
	}
	return n
}

func (p *Parser) parseAssignment(n *Node) *Node {
	return &Node{op: opAs, token: p.need(lex.As), left: n, right: p.parseExpr(0)}
}

func (p *Parser) parseDeclarationAssignment(n *Node) *Node {
	return &Node{op: opDas, token: p.need(lex.Das), left: n, right: p.parseExpr(0)}
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
		t = &Node{ op: op, token: tok, left: t, right: t1 }
	}
	return t
}

func (p *Parser) parseOperand() *Node {

	kind := p.Kind()
	switch {
	case kind.IsUnaryOperator():
		op, tok := p.parseOperator(true)
		return &Node{op: op, token: tok, left: p.parseExpr(tok.Kind.Precedence())} // NOTE: must use kind defined in token as may be different from next

	case kind == lex.LParen:
		p.need(lex.LParen)
		expr := p.parseExpr(0)
		p.need(lex.RParen)
		return expr

	case kind == lex.Integer || kind == lex.String || kind == lex.True || kind == lex.False:
		return &Node{op: opLit, token: p.next()}

	case kind == lex.Identifier:
		return p.parseIdentifier()

	default:
		p.syntaxError("<expression>")
		return &Node{op: opError, token: p.next()}
	}
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
		// Rewrite token to differentiate binary subtract from unary minus
		if isUnary {
			t := p.next()
			t.Kind = lex.Neg
			return opNeg, t
		}
		return opSub, p.next()
	case lex.And:
		return opAnd, p.next()
	case lex.Or:
		return opOr, p.next()
	case lex.Gt:
		return opGt, p.next()
	case lex.Lt:
		return opLt, p.next()
	default:
		p.syntaxError("<operator>")
		return opError, p.next()
	}
}

func (p *Parser) parseIdentifier() *Node {
	val := p.need(lex.Identifier)
	var ident *Node
	switch p.Kind() {
	case lex.LParen:
		ident = &Node {op: opFuncCall, token: val, stmts: p.parseArgs()}
	case lex.LBrack:
		ident = &Node {op: opArray, token: lex.WithVal(val, "[]"), left: &Node{op: opIdentifier, token: val}, right: p.parseIndex()}
	default:
		ident = &Node {op: opIdentifier, token: val}
	}

	// Check for further calls or index operators
	for p.is(lex.LParen, lex.LBrack) {
		switch p.Kind() {
		case lex.LParen:
			ident = &Node {op: opFuncCall, token: lex.WithVal(val, "()"), left: ident, stmts: p.parseArgs()}
		case lex.LBrack:
			ident = &Node {op: opArray, token: lex.WithVal(val, "[]"), left: ident, right: p.parseIndex()}
		}
	}
	return ident
}

func (p *Parser) parseArgs() (args []*Node) {
	p.need(lex.LParen)
	if p.isNot(lex.RParen) {
		args = append(args, p.parseExpr(0))
		for p.match(lex.Comma) {
			args = append(args, p.parseExpr(0))
		}
	}
	p.need(lex.RParen)
	return args
}

func (p *Parser) parseIndex() *Node {
	p.need(lex.LBrack)
	idx := p.parseExpr(0)
	p.need(lex.RBrack)
	return idx
}

func (p *Parser) parseParameters() (x []*Node) {
	p.need(lex.LParen)
	if p.isNot(lex.RParen) {
		x = append(x, p.parseParameter())
		for p.match(lex.Comma) {
			x = append(x, p.parseParameter())
		}
	}
	p.need(lex.RParen)
	return x
}

func (p *Parser) parseParameter() *Node {
	name := p.need(lex.Identifier)
	p.need(lex.Colon)
	return &Node{op: opIdentifier, token: name, left: p.parseType()}
}

func (p *Parser) parseType() *Node {
	switch p.Kind() {
	case lex.Fn:
		t := p.next()
		var types []*Node
		p.need(lex.LParen)
		if p.isNot(lex.RParen) {
			types = append(types, p.parseType())
			for p.match(lex.Comma) {
				types = append(types, p.parseType())
			}
		}
		p.need(lex.RParen)
		n := &Node{op: opFuncType, token: t, stmts: types}
		if p.is(lex.Fn, lex.Identifier, lex.LBrack) {
			n.left = p.parseType()
		}
		return n
	case lex.LBrack:
		t := p.next()
		p.need(lex.RBrack)
		return &Node{op: opArrayType, token: t, left: p.parseType()}
	case lex.Identifier:
		return &Node{op: opNamedType, token: p.next()}
	default:
		p.syntaxError("<type>")
		return &Node{op: opError, token: p.next()}
	}
}


// ==========================================================================================================
// Matching & movement functions

func (p *Parser) need(k lex.Kind) *lex.Token {
	for !p.is(k) {
		p.syntaxError(lex.KindValues[k])
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
	if p.pos+1 >= len(p.tokens) {
		panic(errUnexpectedEof)
	}
	token := p.tokens[p.pos]
	p.pos++
	return token
}

func (p *Parser) syntaxError(expected string) {
	if !p.discard {
		// Enable discard mode
		p.discard = true

		// Store error
		token := p.tokens[p.pos]
		p.errs = append(p.errs,
			errors.New(fmt.Sprintf(errSyntaxMsg,
				token.File,
				token.Line,
				token.Pos,
				p.tokens[p.pos].Val,
				expected)))
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
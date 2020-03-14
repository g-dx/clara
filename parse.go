package main

import (
	"errors"
	"fmt"
	"github.com/g-dx/clarac/lex"
	"strings"
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

const (
	extRet = 1 << iota
)

type attributes int
func (attr attributes) isExternalReturn() bool {
	return (attr & extRet) == extRet
}

func (attr attributes) Add(name string) attributes {
	switch name {
	case "ExtRet":
		return attr | extRet
	default:
		return attr // TODO: Report unknown attributes
	}
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
		attr := p.parseAttributes()
		switch p.Kind() {
		case lex.Fn:
			root.Add(p.parseFn(attr, p.need(lex.Fn), false))

		case lex.Struct:
			root.Add(p.parseStruct(attr))

		case lex.Enum:
			root.Add(p.parseEnum(attr))

		default:
			kinds := []string{lex.KindValues[lex.Fn], lex.KindValues[lex.Struct], lex.KindValues[lex.Enum]}
			p.syntaxError(strings.Join(kinds, " or "))
			p.next()
			// TODO: p.sync(lex.Fn, lex.Struct, lex.Enum)
		}
	}
	p.need(lex.EOF)
	return p.errs
}

func (p *Parser) parseAttributes() (attr attributes) {
	if !p.is(lex.Hash) {
		return
	}
	p.need(lex.Hash)
	p.need(lex.LBrack)
	if p.isNot(lex.LBrack) {
		for ok := true; ok; ok = p.match(lex.Comma) {
			attr = attr.Add(p.need(lex.Identifier).Val)
		}
	}
	p.need(lex.RBrack)
	return
}

func (p *Parser) parseEnum(attrs attributes) *Node {
	p.need(lex.Enum)
	id := p.need(lex.Identifier)
	p.need(lex.LBrace)
	var cons []*Node
	for p.isNot(lex.RBrace) {
		cons = append(cons,
			&Node{op: opConsFnDcl, token: p.need(lex.Identifier), params: p.parseParameters(),
				left: &Node{op: opNamedType, token: id}})
	}
	p.need(lex.RBrace)
	return &Node{attrs: attrs, op: opEnumDcl, token: id, stmts: cons}
}

func (p *Parser) parseStruct(attrs attributes) *Node {
	p.need(lex.Struct)
	id := p.need(lex.Identifier)
	p.need(lex.LBrace)
	var fields []*Node
	for p.isNot(lex.RBrace) {
		fields = append(fields, p.parseParameter())
	}
	p.need(lex.RBrace)
	return &Node{attrs: attrs, op: opStructDcl, token: id, stmts: fields}
}

func (p *Parser) parseFn(attrs attributes, id *lex.Token, isAnon bool) *Node {
	if !isAnon {
		id = p.need(lex.Identifier)
	}
	var typeList *Node
	if p.Kind() == lex.LGmet {
		types, start := p.parseTypeList()
		typeList = &Node{op: opTypeList, token: start, params: types }
	}
	n := &Node{attrs: attrs, token: id, params: p.parseParameters()}
	n.right = typeList
	if p.is(lex.Fn, lex.LBrack, lex.Identifier) {
		n.left = p.parseType()
	}
	switch p.Kind() {
	case lex.LBrace:
		n.op = opBlockFnDcl
		n.stmts = p.parseBlock()
	case lex.As:
		p.next()
		n.op = opExprFnDcl
		n.stmts = []*Node{p.parseExpr(0)}
	default:
		if !isAnon {
			n.op = opExternFnDcl
		} else {
			p.syntaxError(lex.KindValues[lex.LBrace] + " or " + lex.KindValues[lex.As])
			p.next()
		}
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

	case kind == lex.For:
		return p.parseFor()

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

	case kind == lex.Match:
		return p.parseMatch()

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

func (p *Parser) parseFor() *Node {
	token := p.need(lex.For)
	value := p.parseIdentifier()
	p.need(lex.In)
	collection := p.parseIdentifier()
	return &Node{op: opFor, token: token, left: value, right: collection, stmts: p.parseBlock()}
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

func (p *Parser) parseMatch() *Node {
	tok := p.need(lex.Match)
	expr := p.parseExpr(0)

	// Parse each case block
	var caseBlocks []*Node
	p.need(lex.LBrace)
	for p.isNot(lex.RBrace) {

		p.need(lex.Case)
		caseBlock := &Node{op: opCase, token: p.need(lex.Identifier), params: p.parseIdentifiers()}
		p.need(lex.Colon)
		for p.isNot(lex.Case, lex.RBrace) {
			caseBlock.stmts = append(caseBlock.stmts, p.parseStatement())
		}
		caseBlocks = append(caseBlocks, caseBlock)
	}
	p.need(lex.RBrace)
	return &Node{op: opMatch, token: tok, left: expr, stmts: caseBlocks}
}

func (p *Parser) parseAssignment(n *Node) *Node {
	return &Node{op: opAs, token: p.need(lex.As), left: n, right: p.parseExpr(0)}
}

func (p *Parser) parseDeclarationAssignment(n *Node) *Node {
	return &Node{op: opDas, token: p.need(lex.Das), left: n, right: p.parseExpr(0)}
}

func (p *Parser) parseIdentifier() *Node {
	return &Node{op: opIdentifier, token: p.need(lex.Identifier)}
}

// ---------------------------------------------------------------------------------------
// Pratt Parsers

func parseBinaryOperator(p *Parser, left *Node, token *lex.Token) *Node {
	precedence := token.Kind.Precedence()
	if token.Kind.Associativity() == lex.Right {
		precedence -= 1 //
	}
	return &Node{op: infixKindToOp[token.Kind], token: token, left: left, right: p.parseExpr(precedence)}
}

func parseTernaryOperator(p *Parser, left *Node, token *lex.Token) *Node {
	ifExpr := p.parseExpr(0)
	p.need(lex.Colon)
	return &Node {op: opTernary, token: token, left: left, stmts: []*Node{ ifExpr, p.parseExpr(token.Kind.Precedence() - 1) }}
}

func parseArray(p *Parser, left *Node, token *lex.Token) *Node {
	idx := p.parseExpr(0)
	p.need(lex.RBrack)
	return &Node {op: opArray, token: token, left: left, right: idx}
}

func parseFunction(p *Parser, token *lex.Token) *Node {
	return p.parseFn(attributes(0), token,true)
}

func parseCall(p *Parser, left *Node, token *lex.Token) *Node {
	var args []*Node
	if p.isNot(lex.RParen) {
		for ok := true; ok; ok = p.match(lex.Comma) {
			args = append(args, p.parseExpr(0))
		}
	}
	p.need(lex.RParen)
	return &Node {op: opFuncCall, token: token, left: left, stmts: args}
}

func parseTypedCall(p *Parser, left *Node, token *lex.Token) *Node {
	var types []*Node
	if p.isNot(lex.RGmet) {
		for ok := true; ok; ok = p.match(lex.Comma) {
			types = append(types, p.parseType())
		}
	}
	p.need(lex.RGmet)
	p.need(lex.LParen)
	n := parseCall(p, left, token)
	n.params = types
	return n
}

func parseIdentifier(_ *Parser, token *lex.Token) *Node {
	return &Node{op: opIdentifier, token: token}
}

func parseLiteral(_ *Parser, token *lex.Token) *Node {
	return &Node{op: opLit, token: token}
}

func parseGroup(p *Parser, _ *lex.Token) *Node {
	expr := p.parseExpr(0)
	p.need(lex.RParen)
	return expr
}

func parsePrefixOperator(p *Parser, token *lex.Token) *Node {
	return &Node{op: prefixKindToOp[token.Kind], token: token, left: p.parseExpr(token.Kind.Precedence())}
}

func parseArrayType(p *Parser, token *lex.Token) *Node {
	p.need(lex.RBrack)
	return &Node{op: opArrayType, token: token, left: p.parseType()}
}

var prefixKindToOp = map[lex.Kind]int{
	lex.Not:  opNot,
	lex.BNot: opBNot,
	lex.Min:  opNeg,
	// Neg is created by parser!
}
var infixKindToOp = map[lex.Kind]int{
	lex.Plus: opAdd,
	lex.Min: opSub,
	lex.Mul: opMul,
	lex.Div: opDiv,
	lex.BLeft: opBLeft,
	lex.BRight: opBRight,
	lex.BAnd: opBAnd,
	lex.BOr: opBOr,
	lex.BXor: opBXor,
	lex.Gt: opGt,
	lex.Gte: opGte,
	lex.Lt: opLt,
	lex.Lte: opLte,
	lex.Eq: opEq,
	lex.Dot: opDot,
	lex.Or: opOr,
	lex.And: opAnd,
}

var infixParsers = make(map[lex.Kind]func(*Parser, *Node, *lex.Token) *Node)
var prefixParsers = make(map[lex.Kind]func(*Parser, *lex.Token) *Node)

func init() {
	// Special handling
	prefixParsers[lex.Identifier] = parseIdentifier
	prefixParsers[lex.LParen] = parseGroup
	prefixParsers[lex.Integer] = parseLiteral
	prefixParsers[lex.String] = parseLiteral
	prefixParsers[lex.True] = parseLiteral
	prefixParsers[lex.False] = parseLiteral
	prefixParsers[lex.Fn] = parseFunction
	prefixParsers[lex.LBrack] = parseArrayType

	infixParsers[lex.LParen] = parseCall
	infixParsers[lex.LGmet] = parseTypedCall
	infixParsers[lex.LBrack] = parseArray
	infixParsers[lex.Question] = parseTernaryOperator

	binaryOperators(lex.Dot, lex.Plus, lex.Min, lex.Mul, lex.Div,
		lex.BLeft, lex.BRight, lex.BRight, lex.BAnd, lex.BOr,
		lex.BXor, lex.Gt, lex.Gte, lex.Lt, lex.Lte, lex.Eq,
		lex.Or, lex.And)
	prefixOperators(lex.Not, lex.BNot, lex.Min)
}

func binaryOperators(kinds ... lex.Kind) {
	for _, k := range kinds {
		infixParsers[k] = parseBinaryOperator
	}
}

func prefixOperators(kinds ... lex.Kind) {
	for _, k := range kinds {
		prefixParsers[k] = parsePrefixOperator
	}
}

func (p *Parser) parseExpr(precedence int) *Node {
	token := p.next()
	prefix := prefixParsers[token.Kind];

	// Skip token & continue
	if prefix == nil {
		p.syntaxError("<expression>")
		return &Node{op: opError, token: p.next()}
	}

	nextPrecedence := func(next lex.Kind) int {
		if _, ok := infixParsers[next]; !ok {
			return 0
		}
		return next.Precedence()
	}

	left := prefix(p, token)
	for precedence < nextPrecedence(p.Kind()) {
		token := p.next()
		left = infixParsers[token.Kind](p, left, token)
	}
	return left;
}

// ---------------------------------------------------------------------------------

func (p *Parser) parseTypeList() (types []*Node, start *lex.Token) {
	start = p.need(lex.LGmet)
	if p.isNot(lex.RGmet) {
		for ok := true; ok; ok = p.match(lex.Comma) {
			types = append(types, p.parseType())
		}
	}
	p.need(lex.RGmet)
	return types, start
}

func (p *Parser) parseIdentifiers() []*Node {
	return p.parseParenList(func() *Node {
		return &Node{op: opIdentifier, token: p.need(lex.Identifier)}
	})
}

func (p *Parser) parseParameters() []*Node {
	return p.parseParenList(p.parseParameter)
}

func (p *Parser) parseParenList(n func() *Node) (x []*Node) {
	p.need(lex.LParen)
	if p.isNot(lex.RParen) {
		for ok := true; ok; ok = p.match(lex.Comma) {
			x = append(x, n())
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
			for ok := true; ok; ok = p.match(lex.Comma) {
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
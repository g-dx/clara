package lex

import (
	"fmt"
	"github.com/g-dx/clarac/console"
	"strings"
	"unicode"
	"unicode/utf8"
)

// Kind of lex tokens we emit
type Kind int32
const (
	Err = -1

	RBrace = '}'
	LBrace = '{'
	RParen = ')'
	LParen = '('
	RBrack = ']'
	LBrack = '['
	Comma  = ','
	Colon  = ':'
	Dot    = '.'

	Comment = 256 + iota // Start outside ascii range
	Identifier
	String
	Integer

	// -----------------------------------------------------------------------------------------------------------------
	// Unary Operators
	Neg // NOTE: Lexer does not emit these. The parser rewrites `Min` tokens to `Neg` when appropriate

	// -----------------------------------------------------------------------------------------------------------------
	// Binary Operators

	Plus
	Mul
	Div
	Min

	// -----------------------------------------------------------------------------------------------------------------
	// Bitwisre Operators
	BAnd
	BOr
	BXor
	BLeft
	BRight

	// -----------------------------------------------------------------------------------------------------------------
	// Comparison Operators
	Gt
	Lt
	Eq

	// -----------------------------------------------------------------------------------------------------------------

	Space
	EOL
	EOF
	Das // Declaration & assignment
	As  // Assignment

	// -----------------------------------------------------------------------------------------------------------------

	// Order is v.important here!
	keyword
	Fn
	Return
	If
	ElseIf
	Else
	True
	False
	Not
	And
	Or
	Struct
	While
	Enum
	Match
	Case
)

func (k Kind) IsBinaryOperator() bool {
	switch k {
	case Plus, Gt, Lt, And, Or, Mul, Div, Min, Eq, Dot, BAnd, BOr, BXor, BLeft, BRight:
		return true
	default:
		return false
	}
}

func (k Kind) IsUnaryOperator() bool {
	switch k {
	case Not, Min, Neg:
		return true
	default:
		return false
	}
}

func (k Kind) IsExprStart() bool {
	switch k {
	case Integer, String, Identifier, True, False, Not, LParen, Fn:
		return true
	default:
		return false
	}
}

func (k Kind) Precedence() int {

	// TODO: other operators should get added here
	switch k {
	case Dot:
		return 12
	case Not, Neg:
		return 11
	case Mul, Div:
		return 10
	case Plus, Min:
		return 9
	case BLeft, BRight:
		return 8
	case Gt, Lt:
		return 7
	case Eq:
		return 6
	case BAnd:
		return 5
	case BXor:
		return 4
	case BOr:
		return 3
	case And:
		return 2
	case Or:
		return 1
	default:
		return 0 // Any other token
	}
}

type Associative byte
const (
	Left = Associative(iota)
	Right
	None
)

func (k Kind) Associativity() Associative {
	switch k {
	case LParen, Plus, And, Or, Mul, Div, Min, Eq, Dot, Neg, BAnd, BOr, BXor, BLeft, BRight:
		return Left
	case Not:
		return Right
	case Gt, Lt:
		return None
	default:
		return None
	}
}

var key = map[string]Kind{
	"fn":     Fn,
	"return": Return,
	"if":     If,
	"elseif": ElseIf,
	"else":   Else,
	"true":   True,
	"false":  False,
	"not":    Not,
	"and":    And,
	"or":     Or,
	"struct": Struct,
	"while":  While,
	"enum":   Enum,
	"match":  Match,
	"case":   Case,
}

var KindValues = map[Kind]string{
	LBrace:     "{",
	RBrace:     "}",
	LParen:     "(",
	RParen:     ")",
	LBrack:     "[",
	RBrack:     "]",
	Identifier: "<identifier>",
	String:     "<string lit>",
	Integer:    "<integer lit>",
	Fn:         "fn",
	Return:     "return",
	If:         "if",
	ElseIf:     "elseif",
	Else:       "else",
	Gt:         ">",
	Lt:         "<",
	BAnd:       "&",
	BOr:        "|",
	BXor:       "^",
	BLeft:      "<<",
	BRight:     ">>",
	Mul:        "*",
	Plus:       "+",
	Div:        "/",
	Min:        "- (binary)",
	Neg:        "- (unary)",
	True:       "true",
	False:      "false",
	Not:        "not",
	And:        "and",
	Eq:         "==",
	Das:        ":=",
	As:         "=",
	Comma:      ",",
	Colon:      ":",
	Dot:        ".",
	Space:      "<space>",
	EOL:        "<EOL>",
	EOF:        "<EOF>",
	Struct:     "struct",
	While:      "while",
	Enum:       "enum",
	Match:      "match",
	Case:       "case",
	Err:        "<error>",
}

type Token struct {
	Kind Kind
	Val  string
	Pos  int
	Line int
	File string
}

func WithVal(token *Token, val string) *Token {
	return &Token{token.Kind, val, token.Pos, token.Line, token.File}
}

func (t Token) String() string {
	val := ""
	switch {
	case t.Kind == EOF:
		val = "EOF"
	case t.Kind > keyword:
		val = fmt.Sprintf("<%s>", t.Val)
	case t.Kind == Integer:
		val = fmt.Sprintf("%s", t.Val)
	case t.Kind == Err:
		val = t.Val
	default:
		val = fmt.Sprintf("%q", t.Val)
	}
	return fmt.Sprintf("%s:%v%v:%v:%v, %v%v%v %v", t.File, console.Yellow, t.Line, t.Pos,
		console.Disable, console.NodeTypeColour, val, console.Disable, KindValues[t.Kind])
}

// This is almost entirely inspired by the template lexer in Go. Src here:
// https://github.com/golang/go/blob/master/src/text/template/parse/go
type Lexer struct {
	// Next state & input string
	state stateFn
	input string
	file string

	// Maintain scanning position
	start int
	pos int
	width int

	// Outgoing tokens
	tokens chan *Token
}

const eof = -1

type stateFn func(*Lexer) stateFn

func Lex(input string, file string) *Lexer {
	l := &Lexer{input : input, file : file, tokens : make(chan *Token)}
	go l.run()
	return l
}

func (l *Lexer) run() {
	for l.state = lexText; l.state != nil; {
		l.state = l.state(l)
	}
	close(l.tokens)
}

func lexText(l *Lexer) stateFn {
	for {
		switch r := l.next(); {
		case r == ' ':
			return lexSpace
		case r == '(':
			l.emit(LParen)
		case r == ')':
			l.emit(RParen)
		case r == '{':
			l.emit(LBrace)
		case r == '}':
			l.emit(RBrace)
		case r == '[':
			l.emit(LBrack)
		case r == ']':
			l.emit(RBrack)
		case r == ',':
			l.emit(Comma)
		case r == ':':
			if l.peek() == '=' {
				l.next()
				l.emit(Das)
			} else {
				l.emit(Colon)
			}
		case r == '+':
			l.emit(Plus)
		case r == '-':
			l.emit(Min)
		case r == '*':
			l.emit(Mul)
		case r == '^':
			l.emit(BXor)
		case r == '|':
			l.emit(BOr)
		case r == '&':
			l.emit(BAnd)
		case r == '>':
			if l.peek() == '>' {
				l.next()
				l.emit(BRight)
			} else {
				l.emit(Gt)
			}
		case r == '<':
			if l.peek() == '<' {
				l.next()
				l.emit(BLeft)
			} else {
				l.emit(Lt)
			}
		case r == '.':
			l.emit(Dot)
		case r == '"':
			return lexString
		case r == '=':
			if l.peek() == '=' {
				l.next()
				l.emit(Eq)
			} else {
				l.emit(As)
			}
		case r == '/':
			if l.peek() == '/' {
				l.next()
				return lexComment
			}
			l.emit(Div)
		case r == '0':
			return lexHexOrDecInteger
		case '1' <= r  && r <= '9':
			return lexDecInteger
		case isAlphabetic(r):
			return lexIdentifier
		case isEndOfLine(r):
			l.emit(EOL)
		case r == eof:
			l.emit(EOF)
			return nil
		default:
			return l.errorf("Unexpected character %#U", r)
		}
	}
}

// '//' has already been consumed
func lexComment(l *Lexer) stateFn {
	for l.peek() != eof && !isEndOfLine(l.peek()) {
		l.next()
	}
	l.emit(Comment)
	return lexText
}

// Opening " has already been consumed
func lexString(l *Lexer) stateFn {
	for l.peek() != eof && l.peek() != '"' {
		l.next()
	}

	// Check for closing quote
	if l.next() != '"' {
		return l.errorf("Unclosed string literal")
	}
	l.emit(String)
	return lexText
}

// Opening digit or negation sign has already been consumed
func lexInteger(pred func(rune) bool, l *Lexer) stateFn {
	for l.peek() != eof && pred(l.peek()) {
		l.next()
	}
	l.emit(Integer)
	return lexText
}

func lexHexOrDecInteger(l *Lexer) stateFn {
	switch r := l.peek(); {
	case r == 'x':
		l.next()
		if !isHexadecimal(l.peek()) {
			return l.errorf("Hexadecimal literal has no digits")
		}
		return lexInteger(isHexadecimal, l)
	default:
		return lexInteger(isNumeric, l)
	}
}

func lexDecInteger(l *Lexer) stateFn {
	return lexInteger(isNumeric, l)
}

// A single space character has been consumed already.
func lexSpace(l *Lexer) stateFn {
	for l.peek() != eof && l.peek() == ' ' {
		l.next()
	}
	l.emit(Space)
	return lexText
}

// First character has already been consumed
func lexIdentifier(l *Lexer) stateFn {

	// Consume until no more alphanumerics
	for l.peek() != eof && isAlphaNumeric(l.peek()) {
		l.next()
	}
	if !l.atTerminator() {
		return l.errorf("Unexpected character %#U", l.peek())
	}

	// Differentiate between known keywords and identifiers
	word := l.input[l.start:l.pos]
	switch {
	case key[word] > keyword:
		l.emit(key[word])
	default:
		l.emit(Identifier)
	}
	return lexText
}

func (l *Lexer) atTerminator() bool {
	r := l.peek()
	// TODO: Extract some helpers to ask isOperator(), isNewline(), etc...
	return r == '(' || r == ' ' || r == ':' || r == ',' || r == ')' || r == '\r' || r == '\n' ||
		r == '.' || r == '+' || r == '-' || r == '*' || r == '/' || r == '>' || r == '[' || r == ']' || r == eof
}

func (l *Lexer) peek() rune {
	r := l.next()
	l.pos -= l.width
	return r
}

func (l *Lexer) next() rune {
	if l.pos >= len(l.input) {
		l.width = 0
		return eof
	}
	r, w := utf8.DecodeRuneInString(l.input[l.pos:])
	l.width = w
	l.pos += l.width
	return r
}

func (l *Lexer) emit(kind Kind) {
	l.tokens <- &Token{kind, l.input[l.start:l.pos], l.linePos(l.start), l.lineNumber(), l.file}
	l.start = l.pos
}

func (l *Lexer) errorf(format string, args ...interface{}) stateFn {
	l.tokens <- &Token{Err, fmt.Sprintf(format, args...), l.linePos(l.pos), l.lineNumber(), l.file}
	return nil
}

func (l *Lexer) linePos(start int) int {
	return start - strings.LastIndex(l.input[:start], "\n")
}

func (l *Lexer) lineNumber() int {
	return 1 + strings.Count(l.input[:l.start], "\n")
}

func (l *Lexer) NextToken() *Token {
	return <-l.tokens
}

func isAlphaNumeric(r rune) bool {
	return isAlphabetic(r) || isNumeric(r)
}

func isAlphabetic(r rune) bool {
	return r == '_' || unicode.IsLetter(r)
}

func isNumeric(r rune) bool {
	return unicode.IsDigit(r)
}

func isHexadecimal(r rune) bool {
	return isNumeric(r) || r == 'a' || r == 'b' || r == 'c' || r == 'd' || r == 'e' || r == 'f' ||
		r == 'A' || r == 'B' || r == 'C' || r == 'D' || r == 'E' || r == 'F'
}

func isEndOfLine(r rune) bool {
	return r == '\r' || r == '\n'
}
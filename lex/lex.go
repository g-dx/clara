package lex

import (
	"fmt"
	"strings"
	"unicode/utf8"
	"unicode"
	"github.com/g-dx/clarac/console"
)

// Kind of lex tokens we emit
type Kind int32
const (
	Err = -1

	RBrace = '}'
	LBrace = '{'
	RParen = ')'
	LParen = '('
	Comma = ','

	Comment = 256 + iota // Start outside ascii range
	Identifier
	String
	Integer

	// -----------------------------------------------------------------------------------------------------------------
	// Binary Operators

	Plus

	// -----------------------------------------------------------------------------------------------------------------

	Space
	EOL
	EOF

	// Order is v.important here!
	Keyword
	Fn
)

var key = map[string]Kind{
	"fn": Fn,
}

var KindValues = map[Kind]string {
	LBrace: "{",
	RBrace: "}",
	LParen: "(",
	RParen: ")",
	Identifier: "<identifier>",
	String: "<string lit>",
	Integer: "<integer lit>",
	Fn: "fn",
	Comma: ",",
	Space : "<space>",
	EOL : "<EOL>",
	EOF : "<EOF>",
}

type Token struct {
	Kind Kind
	Val  string
	Pos  int
	Line int
	File string
}

func (t Token) String() string {
	val := ""
	switch {
	case t.Kind == EOF:
		val = "EOF"
	case t.Kind > Keyword:
		val = fmt.Sprintf("<%s>", t.Val)
	case t.Kind == Integer:
		val = fmt.Sprintf("%s", t.Val)
	case t.Kind == Err:
		val = t.Val
	default:
		val = fmt.Sprintf("%q", t.Val)
	}
	return fmt.Sprintf("%s:%v%v:%v%v, %v%v%v", t.File, console.Yellow, t.Line, t.Pos,
		console.Disable, console.NodeTypeColour, val, console.Disable)
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
		case r == ',':
			l.emit(Comma)
		case r == '+':
			l.emit(Plus)
		case r == '"':
			return lexString
		case r == '/':
			if l.peek() != '/' {
				return l.errorf("Unexpected character %#U", r)
			}
			l.next()
			return lexComment
		case isNumeric(r):
			return lexInteger
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

// Opening digit has already been consumed
func lexInteger(l *Lexer) stateFn {
	for l.peek() != eof && isNumeric(l.peek()) {
		l.next()
	}
	l.emit(Integer)
	return lexText
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
	case key[word] > Keyword:
		l.emit(key[word])
	default:
		l.emit(Identifier)
	}
	return lexText
}

func (l *Lexer) atTerminator() bool {
	// NOTE: the only valid identifier is currently a function name or keyword and as
	// such the only valid terminators are a space or a left paren. When variables
	// and types are added this will need to change.
	r := l.peek()
	return r == '(' || r == ' '
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

func (l *Lexer) linePos(n int) int {
	return l.start - strings.LastIndex(l.input[:n], "\n")
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

func isEndOfLine(r rune) bool {
	return r == '\r' || r == '\n'
}
package main

import (
	"fmt"
	"strings"
	"unicode/utf8"
	"unicode"
)

// Kind of lex tokens we emit
type lexKind uint8
const (
	kindError lexKind = iota
	kindRightBrace
	kindLeftBrace
	kindRightParen
	kindLeftParen
	kindComment
	kindIdentifier
	kindString
	kindInteger
	kindSpace
	kindComma
	kindEOL
	kindEOF
	// Order is v.important here!
	kindKeyword
	kindFn
)

var key = map[string]lexKind{
	"fn": kindFn,
}

var kindValues = map[lexKind]string {
	kindLeftBrace: "{",
	kindRightBrace: "}",
	kindLeftParen: "(",
	kindRightParen: ")",
	kindIdentifier: "<identifier>",
	kindString: "<string lit>",
	kindInteger: "<integer lit>",
	kindFn: "fn",
	kindComma: ",",
	kindSpace : "<space>",
	kindEOL : "<EOL>",
	kindEOF : "<EOF>",
}

type Token struct {
	kind lexKind
	val  string
	pos  int
	line int
	file string
}

func (t Token) String() string {
	val := ""
	switch {
	case t.kind == kindEOF:
		val = "EOF"
	case t.kind > kindKeyword:
		val = fmt.Sprintf("<%s>", t.val)
	case t.kind == kindInteger:
		val = fmt.Sprintf("%s", t.val)
	case t.kind == kindError:
		val = t.val
	default:
		val = fmt.Sprintf("%q", t.val)
	}
	return fmt.Sprintf("%s:%v%v:%v%v, %v%v%v", t.file, yellowColour, t.line, t.pos,
		disableConsoleColour, nodeTypeColour, val, disableConsoleColour)
}

// This is almost entirely inspired by the template lexer in Go. Src here:
// https://github.com/golang/go/blob/master/src/text/template/parse/lex.go
type lexer struct {
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

type stateFn func(*lexer) stateFn

func lex(input string, file string) *lexer {
	l := &lexer{input : input, file : file, tokens : make(chan *Token)}
	go l.run()
	return l
}

func (l *lexer) run() {
	for l.state = lexText; l.state != nil; {
		l.state = l.state(l)
	}
	close(l.tokens)
}

func lexText(l *lexer) stateFn {
	for {
		switch r := l.next(); {
		case r == ' ':
			return lexSpace
		case r == '(':
			l.emit(kindLeftParen)
		case r == ')':
			l.emit(kindRightParen)
		case r == '{':
			l.emit(kindLeftBrace)
		case r == '}':
			l.emit(kindRightBrace)
		case r == ',':
			l.emit(kindComma)
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
			l.emit(kindEOL)
		case r == eof:
			l.emit(kindEOF)
			return nil
		default:
			return l.errorf("Unexpected character %#U", r)
		}
	}
}

// '//' has already been consumed
func lexComment(l *lexer) stateFn {
	for l.peek() != eof && !isEndOfLine(l.peek()) {
		l.next()
	}
	l.emit(kindComment)
	return lexText
}

// Opening " has already been consumed
func lexString(l *lexer) stateFn {
	for l.peek() != eof && l.peek() != '"' {
		l.next()
	}

	// Check for closing quote
	if l.next() != '"' {
		return l.errorf("Unclosed string literal")
	}
	l.emit(kindString)
	return lexText
}

// Opening digit has already been consumed
func lexInteger(l *lexer) stateFn {
	for l.peek() != eof && isNumeric(l.peek()) {
		l.next()
	}
	l.emit(kindInteger)
	return lexText
}

// A single space character has been consumed already.
func lexSpace(l *lexer) stateFn {
	for l.peek() != eof && l.peek() == ' ' {
		l.next()
	}
	l.emit(kindSpace)
	return lexText
}

// First character has already been consumed
func lexIdentifier(l *lexer) stateFn {

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
	case key[word] > kindKeyword:
		l.emit(key[word])
	default:
		l.emit(kindIdentifier)
	}
	return lexText
}

func (l *lexer) atTerminator() bool {
	// NOTE: the only valid identifier is currently a function name or keyword and as
	// such the only valid terminators are a space or a left paren. When variables
	// and types are added this will need to change.
	r := l.peek()
	return r == '(' || r == ' '
}

func (l *lexer) peek() rune {
	r := l.next()
	l.pos -= l.width
	return r
}

func (l *lexer) next() rune {
	if l.pos >= len(l.input) {
		l.width = 0
		return eof
	}
	r, w := utf8.DecodeRuneInString(l.input[l.pos:])
	l.width = w
	l.pos += l.width
	return r
}

func (l *lexer) emit(kind lexKind) {
	l.tokens <- &Token{kind, l.input[l.start:l.pos], l.linePos(l.start), l.lineNumber(), l.file}
	l.start = l.pos
}

func (l *lexer) errorf(format string, args ...interface{}) stateFn {
	l.tokens <- &Token{kindError, fmt.Sprintf(format, args...), l.linePos(l.pos), l.lineNumber(), l.file}
	return nil
}

func (l *lexer) linePos(n int) int {
	return l.start - strings.LastIndex(l.input[:n], "\n")
}

func (l *lexer) lineNumber() int {
	return 1 + strings.Count(l.input[:l.start], "\n")
}

func (l *lexer) nextToken() *Token {
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
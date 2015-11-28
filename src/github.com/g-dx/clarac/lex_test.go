package main

import "testing"

const errorString = "\nInput   : %q\nPosition: %d\nExpected: %v\nActual  : %v"

func TestLex(t *testing.T) {

	tokens := func(t...lexKind) []lexKind { return t }
	tests := []struct {
		in       string
		expected []lexKind
	}{
		// Simple tests
		{"", tokens(kindEOF)},
		{"\n", tokens(kindEOL, kindEOF)},
		{"{}", tokens(kindLeftBrace, kindRightBrace, kindEOF)},
		{"()", tokens(kindLeftParen, kindRightParen, kindEOF)},

		// Identifiers & terminators
		{"abc ", tokens(kindIdentifier, kindSpace, kindEOF)},
		{"abc(", tokens(kindIdentifier, kindLeftParen, kindEOF)},

		// Integer literals
		{"123 456", tokens(kindInteger, kindSpace, kindInteger, kindEOF)},

		// String literals
		{"\"string\" \"literal\"", tokens(kindString, kindSpace, kindString, kindEOF)},
		{"\"£$%_ä€ß\"", tokens(kindString, kindEOF)},

		// Comments
		{"// {}()123//abc\"def\"fn/\n", tokens(kindComment, kindEOL, kindEOF)},

		// Keywords
		{"fn ", tokens(kindFn, kindSpace, kindEOF)},

		// Errors
		{"&", tokens(kindError)}, // Unexpected character
		{"abc", tokens(kindError)}, // Identifier not terminated
		{"\"abc", tokens(kindError)}, // Unclosed string literal
		{"/", tokens(kindError)}, // Unexpected character

		// Programs
		{"// Comment\nfn x() {\n y()\n }\n",
			tokens(kindComment, kindEOL, kindFn, kindSpace, kindIdentifier, kindLeftParen,
				kindRightParen, kindSpace, kindLeftBrace, kindEOL, kindSpace, kindIdentifier,
				kindLeftParen, kindRightParen, kindEOL, kindSpace, kindRightBrace, kindEOL,
				kindEOF)},
	}

	// Run all tests
	loop:
	for _, test := range tests {

		// Check tokens in order expected
		lexer := lex(test.in, "<test file>")
		for i := 0; i < len(test.expected); i++ {
			actual := lexer.nextToken()
			if actual.kind != test.expected[i] {
				t.Errorf(errorString, test.in, i+1, kindValues[test.expected[i]], kindValues[actual.kind])
				continue loop // Skip rest of test
			}
		}

		// Check no more tokens
		token := lexer.nextToken()
		if token != nil {
			t.Errorf(errorString, test.in, -1, "<end>", kindValues[token.kind])
		}
	}
}

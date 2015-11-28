package main

import "testing"

const errorString = "\nInput   : %q\nPosition: %d\nExpected: %v\nActual  : %v"

func TestLex(t *testing.T) {

	toks := func(t...lexKind) []lexKind { return t }
	tests := []struct {
		in       string
		expected []lexKind
	}{
		// Simple tests
		{"", toks(kindEOF)},
		{"\n", toks(kindEOL, kindEOF)},
		{"{}", toks(kindLeftBrace, kindRightBrace, kindEOF)},
		{"()", toks(kindLeftParen, kindRightParen, kindEOF)},

		// Identifiers & terminators
		{"abc ", toks(kindIdentifier, kindSpace, kindEOF)},
		{"abc(", toks(kindIdentifier, kindLeftParen, kindEOF)},

		// Integer literals
		{"123 456", toks(kindInteger, kindSpace, kindInteger, kindEOF)},

		// String literals
		{"\"string\" \"literal\"", toks(kindString, kindSpace, kindString, kindEOF)},
		{"\"£$%_ä€ß\"", toks(kindString, kindEOF)},

		// Comments
		{"// {}()123//abc\"def\"fn/\n", toks(kindComment, kindEOL, kindEOF)},

		// Keywords
		{"fn ", toks(kindFn, kindSpace, kindEOF)},

		// Errors
		{"&", toks(kindError)}, // Unexpected character
		{"abc", toks(kindError)}, // Identifier not terminated
		{"\"abc", toks(kindError)}, // Unclosed string literal
		{"/", toks(kindError)}, // Unexpected character

		// Programs
		{"// Comment\nfn x() {\n y()\n }\n",
			toks(kindComment, kindEOL, kindFn, kindSpace, kindIdentifier, kindLeftParen,
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

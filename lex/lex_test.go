package lex

import "testing"

const errorString = "\nInput   : %q\nPosition: %d\nExpected: %v\nActual  : %v"

func TestLex(t *testing.T) {

	tokens := func(t...Kind) []Kind { return t }
	tests := []struct {
		in       string
		expected []Kind
	}{
		// Simple tests
		{"", tokens(EOF)},
		{"\n", tokens(EOL, EOF)},
		{"{}", tokens(LBrace, RBrace, EOF)},
		{"()", tokens(LParen, RParen, EOF)},
		{"  ", tokens(Space, EOF)},
		{",", tokens(Comma, EOF)},

		// Identifiers & terminators
		{"abc ", tokens(Identifier, Space, EOF)},
		{"abc(", tokens(Identifier, LParen, EOF)},
		{"abc,", tokens(Identifier, Comma, EOF)},
		{"abc:", tokens(Identifier, Colon, EOF)},
		{"abc)", tokens(Identifier, RParen, EOF)},
		{"abc\n", tokens(Identifier, EOL, EOF)},
		{"abc\r", tokens(Identifier, EOL, EOF)},

		// Integer literals
		{"123 456", tokens(Integer, Space, Integer, EOF)},

		// String literals
		{"\"string\" \"literal\"", tokens(String, Space, String, EOF)},
		{"\"£$%_ä€ß\"", tokens(String, EOF)},

		// Comments
		{"// {}()123//abc\"def\"fn/\n", tokens(Comment, EOL, EOF)},

		// Keywords
		{"fn ", tokens(Fn, Space, EOF)},
		{"return ", tokens(Return, Space, EOF)},

		// Errors
		{"\"abc", tokens(Err)}, // Unclosed string literal

		// Programs
		{"// Comment\nfn x() {\n y(1,\"\")\n }\n",
			tokens(Comment, EOL, Fn, Space, Identifier, LParen,
				RParen, Space, LBrace, EOL, Space, Identifier,
				LParen, Integer, Comma, String, RParen,
				EOL, Space, RBrace, EOL, EOF)},
	}

	// Run all tests
	loop:
	for _, test := range tests {

		// Check tokens in order expected
		lexer := Lex(test.in, "<test file>")
		for i := 0; i < len(test.expected); i++ {
			actual := lexer.NextToken()
			if actual.Kind != test.expected[i] {
				t.Errorf(errorString, test.in, i+1, KindValues[test.expected[i]], KindValues[actual.Kind])
				continue loop // Skip rest of test
			}
		}

		// Check no more tokens
		token := lexer.NextToken()
		if token != nil {
			t.Errorf(errorString, test.in, -1, "<end>", KindValues[token.Kind])
		}
	}
}

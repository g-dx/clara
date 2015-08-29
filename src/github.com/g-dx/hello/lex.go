package main

import (
	"errors"
	"fmt"
	"regexp"
	"strings"
)

const (
	fnBodyStart   = "FN_BODY_START"
	fnBodyEnd     = "FN_BODY_END"
	fnArgsStart   = "FN_ARGS_START"
	fnArgsEnd     = "FN_ARGS_END"
	fnName        = "FN_NAME"
	strLit        = "STRING_LIT"
	keyword       = "KEYWORD"
	fnKeyword     = "fn"
	argsSeparator = "COMMA"
	tokenEOF      = "EOF"
)

var kindValues = map[string]string {
	fnBodyStart : "{",
	fnBodyEnd : "}",
	fnArgsStart : "(",
	fnArgsEnd : ")",
	fnName: "<identifier>",
	strLit: "<string lit>",
	keyword: "fn", // TODO: Fix me!
	argsSeparator : ",",
	tokenEOF : "<EOF>",
}

var patterns = [][]string{
	{fnKeyword, keyword}, // TODO: add other keywords
	{"\"[\\s!\\w]*\"", strLit},
	{"[a-zA-Z]+", fnName},
	{"\\{", fnBodyStart},
	{"\\}", fnBodyEnd},
	{"\\(", fnArgsStart},
	{"\\)", fnArgsEnd},
	{",", argsSeparator},
	{"\\r?\\n", "NEWLINE"},
	{"\\s+", "WHITESPACE"},
	{"//.*", "COMMENT"},
}

func lex(prog string) ([]*Token, error) {

	// Build regex parts
	var parts []string
	for _, p := range patterns {
		parts = append(parts, fmt.Sprintf("(?P<%s>%s)", p[1], p[0]))
	}

	// Build regex
	regex := regexp.MustCompile(strings.Join(parts, "|"))
	names := regex.SubexpNames()

	// Track progress
	pos := 0
	line := 1
	linePos := 0
	s := prog
	var tokens []*Token
	for pos != len(prog) {
		res := regex.FindStringSubmatchIndex(s)
		if res[0] != 0 {
			// Failed to match some input - return error
			return nil, errors.New(fmt.Sprintf("Failed to match (%s) against any rule!", s[0:res[0]]))
		}

		// Get value and type
		for i := 2; i < len(res); i += 2 {
			if res[i] == -1 {
				continue
			}

			// Create token (skip whitespace, newlines and comments)
			kind := names[i/2]
			val := s[res[i]:res[i+1]]
			if kind != "NEWLINE" && kind != "WHITESPACE" && kind != "COMMENT"{
				t := &Token{kind, val, line, pos - linePos}
				tokens = append(tokens, t)
			}

			// Update state
			if kind == "NEWLINE" {
				line++
				linePos = pos
			}
			pos += res[i+1]
			s = s[res[i+1]:]
			break
		}
	}
	// Add EOF token to mark end of stream
	tokens = append(tokens, &Token{tokenEOF, "EOF", line, pos - linePos})
	return tokens, nil
}

type Token struct {
	kind string
	val  string
	line int
	pos  int
}

func (t *Token) String() string {
	return fmt.Sprintf("%-14s[%v:%-2v] => '%s'", t.kind, t.line, t.pos, t.val)
}

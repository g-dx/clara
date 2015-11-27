package main
import (
	"fmt"
	"strconv"
)

const (
	symStrLit = iota
	symIntegerLit
	symFnDecl
)

type Symbol interface {
	name() string
	kind() int
}

type IntegerLiteralSymbol struct {
	val int64
}

func (i *IntegerLiteralSymbol) name() string {
	return strconv.FormatInt(i.val, 10)
}

func (i *IntegerLiteralSymbol) kind() int {
	return symIntegerLit
}

type StringLiteralSymbol struct {
	val string
	rva uint32
}

func (str *StringLiteralSymbol) name() string {
	return str.val
}

func (str *StringLiteralSymbol) kind() int {
	return symStrLit
}


func (str *StringLiteralSymbol) Val() string {
	return str.val
}

func (str *StringLiteralSymbol) Rva() uint32 {
	return str.rva
}

type Function struct {
	fnName string
	fnArgCount int
	rva uint32
}

func (fn *Function) name() string {
	return fn.fnName
}

func (fn *Function) argCount() int {
	return fn.fnArgCount
}

func (fn *Function) Rva() *uint32 {
	return &fn.rva
}

func (fn *Function) kind() int {
	return symFnDecl
}

type SymTab struct {
	symbols map[string]Symbol
}

func NewSymtab() SymTab {
	s := SymTab{ make(map[string]Symbol) }
	return s
}

func (s *SymTab) Define(sym Symbol) {
	s.symbols[fmt.Sprintf("%v.%v", sym.kind(), sym.name())] = sym
}

func (s *SymTab) Resolve(symType int, name string) (Symbol, bool) {
	sym, ok := s.symbols[fmt.Sprintf("%v.%v", symType, name)]
	return sym, ok
}

func (s *SymTab) Walk(f func(Symbol)) {
	for _, s := range s.symbols {
		f(s)
	}
}
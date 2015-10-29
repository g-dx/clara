package main
import (
	"fmt"
)

const (
	symStrLit = iota
	symFnDecl
)

type Symbol interface {
	name() string
	kind() int
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

type BuiltinFunction struct {
	Function
}

type SymTab struct {
	symbols map[string]Symbol
}

func NewSymtab() SymTab {
	s := SymTab{ make(map[string]Symbol) }
	s.AddRuntime()
	return s
}

func (s *SymTab) AddRuntime() {
	// Only "print" built in
	s.Define(&BuiltinFunction{Function{"print", 1, 0 }})
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
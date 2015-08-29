package main

type Symbol interface {
	name() string
}

type StringLiteralSymbol struct {
	symName string
}

func (str *StringLiteralSymbol) name() string {
	return str.symName
}

type BuiltInFunction struct {
	fnName string
	fnArgCount int
}

func (fn *BuiltInFunction) name() string {
	return fn.fnName
}

func (fn *BuiltInFunction) argCount() int {
	return fn.fnArgCount
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
	s.Define(&BuiltInFunction{ "print", 1 })
}

func (s *SymTab) Define(sym Symbol) {
	s.symbols[sym.name()] = sym
}

func (s *SymTab) Resolve(name string) Symbol {
	return s.symbols[name]
}
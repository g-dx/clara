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

type Symtab struct {
	symbols map[string]Symbol
}

func NewSymtab() Symtab {
	s := Symtab{ make(map[string]Symbol) }
	s.AddRuntime()
	return s
}

func (s *Symtab) AddRuntime() {
	// Only "print" built in
	s.Define(&BuiltInFunction{ "print", 1 })
}

func (s *Symtab) Define(sym Symbol) {
	s.symbols[sym.name()] = sym
}

func (s *Symtab) Resolve(name string) Symbol {
	return s.symbols[name]
}
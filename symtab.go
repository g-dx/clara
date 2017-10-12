package main
import (
	"fmt"
)

//----------------------------------------------------------------------------------------------------------------------

var intType = &Type{ Kind: Integer, Data: &IntType{} }
var boolType = &Type{ Kind: Boolean, Data: &BoolType{} }
var stringType = &Type{ Kind: String, Data: &StringType{} }

//----------------------------------------------------------------------------------------------------------------------

type TypeKind byte
const (
	Struct = TypeKind(iota)
	Function2
	Integer
	Boolean
	String
)

var typeKindNames = map[TypeKind]string {
	Struct: "struct",
	Function2: "function",
	Integer: "int",
	Boolean: "bool",
	String: "string",
}

func (tk TypeKind) String() string {
	s, ok := typeKindNames[tk]
	if !ok {
		s = "<unknown type kind>"
	}
	return s
}

//----------------------------------------------------------------------------------------------------------------------

type Type struct {
	Kind TypeKind
	Data interface{}
}

func (t *Type) AsStruct() *StructType {
	return nil
}

func (t *Type) AsFunction() *FunctionType {
	return t.Data.(*FunctionType)
}

//----------------------------------------------------------------------------------------------------------------------

type StructType struct {

}

//----------------------------------------------------------------------------------------------------------------------

type FunctionType struct {
	Name          string
	ArgCount      int
	isVariadic    bool
	args          *SymTab
	ret           *TypeSymbol
	isConstructor bool
}

//----------------------------------------------------------------------------------------------------------------------

type IntType struct {

}

//----------------------------------------------------------------------------------------------------------------------

type StringType struct {

}

//----------------------------------------------------------------------------------------------------------------------

type BoolType struct {

}

//----------------------------------------------------------------------------------------------------------------------

const (
	symStructDecl = iota
	symType
	symVar
)

var symTypes = map[int]string{
	symStructDecl: "Struct Decl",
	symType:       "Type",
	symVar:        "Variable",
}

type Symbol interface {
	name() string
	kind() int
	Type() *Type
}

//----------------------------------------------------------------------------------------------------------------------

type StructSymbol struct {
	val string
	fields []*IdentSymbol // TODO: What about structs in structs?
	typ *Type
}

func (s *StructSymbol) name() string {
	return s.val
}

func (s *StructSymbol) kind() int {
	return symStructDecl
}

func (s *StructSymbol) width() int {
	i := 0
	for _, s := range s.fields {
		i += s.typ.width
	}
	return i
}

func (s *StructSymbol) offset(v *IdentSymbol) int {
	off := 0
	for _, field := range s.fields {
		if field == v {
			return off
		}
		off += field.typ.width
	}
	return -1 // Not found
}

func (s *StructSymbol) Type() *Type {
	return s.typ
}

//----------------------------------------------------------------------------------------------------------------------

type TypeSymbol struct {
	val   string
	width int
	typ *Type
}

func (t *TypeSymbol) name() string {
	return t.val
}

func (t *TypeSymbol) kind() int {
	return symType
}

func (t *TypeSymbol) Type() *Type {
	return t.typ
}

//----------------------------------------------------------------------------------------------------------------------

type IdentSymbol struct {
	val  string
	addr int
	isStack bool
	typ  *TypeSymbol // TODO: What about StructSymbol?
	typ2 *Type
}

func (i *IdentSymbol) name() string {
	return i.val
}

func (i *IdentSymbol) kind() int {
	return symVar
}

func (i *IdentSymbol) Type() *Type {
	return i.typ2
}

type SymTab struct {
	parent *SymTab
	children []*SymTab
	symbols map[string]Symbol
}

//----------------------------------------------------------------------------------------------------------------------

func NewSymtab() *SymTab {
	return &SymTab{nil, nil, make(map[string]Symbol)}
}

func (s *SymTab) Define(sym Symbol) {
	s.symbols[fmt.Sprintf("%v.%v", sym.kind(), sym.name())] = sym
}

func (s *SymTab) Resolve(symType int, name string) (Symbol, bool) {
	sym, ok := s.symbols[fmt.Sprintf("%v.%v", symType, name)]
	if !ok && s.parent != nil {
		sym, ok = s.parent.Resolve(symType, name)
	}
	return sym, ok
}

func (s *SymTab) Parent() *SymTab {
	return s.parent
}

func (s *SymTab) Child() *SymTab {
	child := NewSymtab()
	child.parent = s
	s.children = append(s.children, child)
	return child
}

func (s *SymTab) Walk(f func(Symbol)) {
	// Walk children first then this node
	for _, child := range s.children {
		child.Walk(f)
	}
	for _, s := range s.symbols {
		f(s)
	}
}
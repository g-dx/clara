package main
import (
	"fmt"
	"strconv"
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

func (t *Type) AsFunction() *StructType {
	return nil
}

//----------------------------------------------------------------------------------------------------------------------

type StructType struct {

}

//----------------------------------------------------------------------------------------------------------------------

type FunctionType struct {

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
	symStrLit = iota
	symIntegerLit
	symFnDecl
	symStructDecl
	symType
	symVar
)

var symTypes = map[int]string{
	symFnDecl:     "Func Decl",
	symStructDecl: "Struct Decl",
	symStrLit:     "String Lit",
	symIntegerLit: "Integer Lit",
	symType:       "Type",
	symVar:        "Variable",
}

type Symbol interface {
	name() string
	kind() int
	Type() *Type
}

//----------------------------------------------------------------------------------------------------------------------

type IntegerLiteralSymbol struct {
	val int64
}

func (i *IntegerLiteralSymbol) name() string {
	return strconv.FormatInt(i.val, 10)
}

func (i *IntegerLiteralSymbol) kind() int {
	return symIntegerLit
}

func (i *IntegerLiteralSymbol) Type() *Type {
	return intType
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
}

func (i *IdentSymbol) name() string {
	return i.val
}

func (i *IdentSymbol) kind() int {
	return symVar
}

func (i *IdentSymbol) Type() *Type {
	return nil // Nothing to return
}

//----------------------------------------------------------------------------------------------------------------------

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

func (str *StringLiteralSymbol) Type() *Type {
	return stringType
}

//----------------------------------------------------------------------------------------------------------------------

type Function struct {
	fnName string
	fnArgCount int
	isVariadic bool
	rva uint32
	args *SymTab
	ret *TypeSymbol
	isConstructor bool
	typ *Type
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

func (fn *Function) Type() *Type {
	return fn.typ
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
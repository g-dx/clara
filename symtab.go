package main
import (
	"fmt"
)

//----------------------------------------------------------------------------------------------------------------------

var intType = &Type{ Kind: Integer, Data: &IntType{ Width: 8 } }
var boolType = &Type{ Kind: Boolean, Data: &BoolType{ Width: 8 } }
var stringType = &Type{ Kind: String, Data: &StringType{ Width: 8 } }

//----------------------------------------------------------------------------------------------------------------------

type TypeKind byte
const (
	Struct = TypeKind(iota)
	Function
	Integer
	Boolean
	String
)

var typeKindNames = map[TypeKind]string {
	Struct:   "struct",
	Function: "function",
	Integer:  "int",
	Boolean:  "bool",
	String:   "string",
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
	Name string // TODO: Add this!
	Data interface{}
}

func (t *Type) AsStruct() *StructType {
	return t.Data.(*StructType)
}

func (t *Type) AsFunction() *FunctionType {
	return t.Data.(*FunctionType)
}

func (t *Type) Width() int {
	switch x := t.Data.(type) {
	case *StructType: return x.Width
	case *IntType: return x.Width
	case *BoolType: return x.Width
	case *StringType: return x.Width
	default:
		panic(fmt.Sprintf("Type.Width() called for unknown data type: %T", t.Data))
	}
}

//----------------------------------------------------------------------------------------------------------------------

type StructType struct {
	Name string
	Fields []*IdentSymbol
	Width int
}

func (st *StructType) Offset(i *IdentSymbol) int {
	off := 0
	for _, field := range st.Fields {
		if field == i {
			return off
		}
		off += 8 // field.typ.width TODO: Fix this width calculation!
	}
	return -1 // Not found
}

//----------------------------------------------------------------------------------------------------------------------

type FunctionType struct {
	Name          string
	ArgCount      int
	isVariadic    bool
	args          *SymTab
	ret           *Type
	isConstructor bool
}

//----------------------------------------------------------------------------------------------------------------------

type IntType struct {
	Width int
}

//----------------------------------------------------------------------------------------------------------------------

type StringType struct {
	Width int
}

//----------------------------------------------------------------------------------------------------------------------

type BoolType struct {
	Width int
}

//----------------------------------------------------------------------------------------------------------------------

const (
	symVar = iota
)

var symTypes = map[int]string{
	symVar:        "Variable",
}

type Symbol interface {
	name() string
	kind() int
	Type() *Type
}


//----------------------------------------------------------------------------------------------------------------------

type IdentSymbol struct {
	val     string
	addr    int
	isStack bool
	typ     *Type
}

func (i *IdentSymbol) name() string {
	return i.val
}

func (i *IdentSymbol) kind() int {
	return symVar
}

func (i *IdentSymbol) Type() *Type {
	return i.typ
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
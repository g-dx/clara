package main
import (
	"fmt"
	"github.com/g-dx/clarac/lex"
)

//----------------------------------------------------------------------------------------------------------------------

var byteType = &Type{ Kind: Integer, Data: &IntType{ Width: 1 } } // TODO: Possibly introduce this globally
var intType = &Type{ Kind: Integer, Data: &IntType{ Width: 8 } }
var boolType = &Type{ Kind: Boolean, Data: &BoolType{ Width: 8 } }
var stringType = &Type{ Kind: String, Data: &StringType{ Width: 8 } }
var nothingType = &Type{ Kind: Nothing, Data: &NothingType{ Width: 0 } }

//----------------------------------------------------------------------------------------------------------------------

type TypeKind byte
const (
	Struct = TypeKind(iota)
	Function
	Integer
	Boolean
	String
	Array
	Nothing
)

var typeKindNames = map[TypeKind]string {
	Struct:   "struct",
	Function: "function",
	Integer:  "int",
	Boolean:  "bool",
	String:   "string",
	Array:    "[]",
	Nothing:   "Nothing",
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

func (t *Type) Is(kind TypeKind) bool {
	return t.Kind == kind
}

func (t *Type) AsStruct() *StructType {
	return t.Data.(*StructType)
}

func (t *Type) AsFunction() *FunctionType {
	return t.Data.(*FunctionType)
}

func (t *Type) AsArray() *ArrayType {
	return t.Data.(*ArrayType)
}

func (t *Type) Name() string {
	switch t.Kind {
	case Array: return t.Kind.String() + t.AsArray().Elem.String()
	case Struct: return t.AsStruct().Name
	default:
		return t.Kind.String()
	}
}

func (t *Type) String() string {
	switch t.Kind {
	case Array: return t.Kind.String() + t.AsArray().Elem.String()
	case Struct: return fmt.Sprintf("struct:%v", t.AsStruct().Name)
	default:
		return t.Kind.String()
	}
}

func (t *Type) Width() int {
	switch x := t.Data.(type) {
	case *StructType: return x.Width
	case *IntType: return x.Width
	case *BoolType: return x.Width
	case *StringType: return x.Width
	case *ArrayType: return x.Width()
	default:
		panic(fmt.Sprintf("Type.Width() called for unknown data type: %T", t.Data))
	}
}

//----------------------------------------------------------------------------------------------------------------------

type TypeLinker struct {
	types map[string][]**Type
	tokens map[string][]*lex.Token
}

func NewTypeLinker() *TypeLinker {
	return &TypeLinker{ types: make(map[string][]**Type), tokens: make(map[string][]*lex.Token) }
}

func (tl *TypeLinker) Add(token *lex.Token, t **Type) {
	tl.types[token.Val] = append(tl.types[token.Val], t)
	tl.tokens[token.Val] = append(tl.tokens[token.Val], token)
}

func (tl *TypeLinker) Link(token *lex.Token, t *Type) {
	for _, typ := range tl.types[token.Val] {
		*typ = t
	}
	delete(tl.types, token.Val)
	delete(tl.tokens, token.Val)
}

//----------------------------------------------------------------------------------------------------------------------

type StructType struct {
	Name string
	Fields []*Symbol
	Width int
}

func (st *StructType) Offset(name string) (*Symbol, int) {
	off := 0
	for _, field := range st.Fields {
		if field.Name == name {
			return field, off
		}
		off += 8 // field.Type.width TODO: Fix this width calculation!
	}
	return nil, -1 // Not found
}

//----------------------------------------------------------------------------------------------------------------------

type FunctionType struct {
	Name          string
	Args          []*Symbol
	Defaults 	  []*Node
	isVariadic    bool
	ret           *Type
	isConstructor bool
	IsExternal 	  bool  // Provided at linktime - no code gen required
}

func (ft *FunctionType) MandatoryParams() int {
	i := 0
	for _, def := range ft.Defaults {
		if def == nil {
			i += 1
		}
	}
	return i
}

//----------------------------------------------------------------------------------------------------------------------

type ArrayType struct {
	Elem *Type
}

func (at *ArrayType) Width() int {
	return at.Elem.Width()
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

type NothingType struct {
	Width int
}
//----------------------------------------------------------------------------------------------------------------------

type Symbol struct {
	Name      string
	Addr      int
	IsStack   bool
	IsLiteral bool
	Type      *Type
}

//----------------------------------------------------------------------------------------------------------------------

type SymTab struct {
	parent *SymTab
	children []*SymTab
	symbols map[string]*Symbol
}

//----------------------------------------------------------------------------------------------------------------------

func NewSymtab() *SymTab {
	return &SymTab{nil, nil, make(map[string]*Symbol)}
}

func (s *SymTab) Define(sym *Symbol) {
	s.symbols[sym.Name] = sym
}

func (s *SymTab) Resolve(name string) (*Symbol, bool) {
	sym, ok := s.symbols[name]
	if !ok && s.parent != nil {
		sym, ok = s.parent.Resolve(name)
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

func (s *SymTab) Walk(f func(*Symbol)) {
	// Walk children first then this node
	for _, child := range s.children {
		child.Walk(f)
	}
	for _, s := range s.symbols {
		f(s)
	}
}
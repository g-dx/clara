package main
import (
	"bytes"
	"fmt"
	"strings"
)

//----------------------------------------------------------------------------------------------------------------------

const ptrSize = 8 // 64-bit pointer size in bytes
const fnPrefix = "clara_"
//----------------------------------------------------------------------------------------------------------------------

var byteType = &Type{ Kind: Byte, Data: &IntType{ Width: 1 } }
var intType = &Type{ Kind: Integer, Data: &IntType{ Width: 8 } }
var boolType = &Type{ Kind: Boolean, Data: &BoolType{ Width: 8 } }
var stringType = &Type{ Kind: String, Data: &StringType{ Width: 8 } }
var nothingType = &Type{ Kind: Nothing, Data: &NothingType{ Width: 0 } }
var byteArrayType = &Type{ Kind: Array, Data: &ArrayType{ Elem: byteType } }
var intArrayType = &Type{ Kind: Array, Data: &ArrayType{ Elem: intType } }
var stringArrayType = &Type{ Kind: Array, Data: &ArrayType{ Elem: stringType } }
var pointerType = &Type{ Kind: Pointer, Data: IntType{ Width: 8} }

//----------------------------------------------------------------------------------------------------------------------

type TypeKind byte
const (
	Struct = TypeKind(iota)
	Enum
	Function
	Integer
	Byte
	Boolean
	String
	Array
	Parameter
	Nothing
	Pointer
)

var typeKindNames = map[TypeKind]string {
	Struct:   "struct",
	Enum:     "enum",
	Function: "function",
	Integer:  "int",
	Byte:     "byte",
	Boolean:  "bool",
	String:   "string",
	Array:    "[]",
	Nothing:   "nothing",
	Parameter: "T",
	Pointer:   "pointer",
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

var emptyMap = make(map[*Type]*Type)

func (t *Type) Matches(x *Type) bool {
	return t.MatchesImpl(x, false, emptyMap)
}

func (t *Type) PolyMatch(x *Type, bound map[*Type]*Type) bool {
	return t.MatchesImpl(x, true, bound)
}

func (t *Type) MatchesImpl(x *Type, allowBinding bool, bound map[*Type]*Type) bool {

	// Bind x to type t & continue matching
	if x.Kind == Parameter && allowBinding {
		xt, ok := bound[x]
		if ok {
			return t.MatchesImpl(xt, allowBinding, bound)
		}
		bound[x] = t
		return true
	}

	// Normal type matching

	switch t.Kind {
	case Struct:
		return x.Kind == Struct && t.AsStruct().Name == x.AsStruct().Name
	case Enum:
		return x.Kind == Enum && t.AsEnum().Name == x.AsEnum().Name
	case Integer, Byte:
		return x.Kind == Integer || x.Kind == Byte // Int & bytes can be used interchangeably...
	case Boolean, String, Nothing:
		return t.Kind == x.Kind
	case Array:
		if x.Kind != Array {
			return false
		}
		te := t.AsArray().Elem
		xe := x.AsArray().Elem
		// SPECIAL CASE: Int & Byte are _not_ interchangeable in arrays!
		if te.Kind == Integer || te.Kind == Byte {
			return te.Kind == xe.Kind
		} else {
			return te.Matches(xe)
		}
	case Function:
		if x.Kind != Function  {
			return false
		}
		xf := x.AsFunction()
		tf := t.AsFunction()
		if len(xf.Params) != len(tf.Params) {
			return false
		}
		for i, param := range xf.Params {
			if !param.MatchesImpl(tf.Params[i], allowBinding, bound) {
				return false
			}
		}
		return tf.ret.MatchesImpl(xf.ret, allowBinding, bound)
	case Parameter:
		if x.Is(Parameter) {
			return t.AsParameter().Name == x.AsParameter().Name
		}
		for k, tt := range bound {
			if k.Matches(t) {
				return tt.MatchesImpl(x, allowBinding, bound)
			}
		}
		return false
	case Pointer:
		return x.Kind == Pointer
	default:
		panic("Unknown or unexpected type comparison!")
	}
}

func (t *Type) Is(kind TypeKind) bool {
	return t.Kind == kind
}

func (t *Type) IsAny(kinds ... TypeKind) bool {
	for _, kind := range kinds {
		if t.Is(kind) {
			return true
		}
	}
	return false
}

func (t *Type) IsArray(kind TypeKind) bool {
	return t.Is(Array) && t.AsArray().Elem.Is(kind)
}

func (t *Type) IsFunction(kind TypeKind) bool {
	return t.Is(Function) && t.AsFunction().ret.Is(kind)
}

func (t *Type) IsPointer() bool {
	return t.IsAny(Array, Struct, String, Function, Enum, Parameter)
}

func (t *Type) AsStruct() *StructType {
	return t.Data.(*StructType)
}

func (t *Type) AsEnum() *EnumType {
	return t.Data.(*EnumType)
}

func (t *Type) AsFunction() *FunctionType {
	return t.Data.(*FunctionType)
}

func (t *Type) AsArray() *ArrayType {
	return t.Data.(*ArrayType)
}

func (t *Type) AsParameter() *ParameterType {
	return t.Data.(*ParameterType)
}

func (t *Type) String() string {
	switch t.Kind {
	case Array: return t.Kind.String() + t.AsArray().Elem.String()
	case Struct: return t.AsStruct().Name
	case Enum: return t.AsEnum().Name
	case Function:
		var types []string
		fn := t.AsFunction()
		for _, param := range fn.Params {
			types = append(types, param.String())
		}
		return fmt.Sprintf("fn(%v) %v", strings.Join(types, ","), fn.ret.String())
	case Parameter:
		return t.AsParameter().Name
	default:
		return t.Kind.String()
	}
}

func (t *Type) AsmName() string {
	switch t.Kind {
	case Array: return fmt.Sprintf("array$%v$", t.AsArray().Elem.AsmName())
	case Struct: return t.AsStruct().Name
	case Enum: return t.AsEnum().Name
	case Function:
		fn := t.AsFunction()
		buf := bytes.NewBufferString("fn")
		if len(fn.Params) > 0 {
			buf.WriteString(fmt.Sprintf("$%v", fn.Params[0].AsmName()))
			for i := 1; i < len(fn.Params); i += 1 {
				buf.WriteString(fmt.Sprintf(".%v", fn.Params[i].AsmName()))
			}
			buf.WriteString("$")
		}
		return buf.String()
	default:
		return t.Kind.String()
	}
}

func (t *Type) Width() int {
	switch x := t.Data.(type) {
	case *IntType: return x.Width
	case *BoolType: return x.Width
	case *StructType, *EnumType, *StringType, *ArrayType, *ParameterType: return ptrSize
	default:
		panic(fmt.Sprintf("Type.Width() called for unknown data type: %T", t.Data))
	}
}

func (t *Type) IsPrimitive() bool {
	switch t.Kind {
	case Integer, Boolean, Byte:
		return true
	case Function:
		f := t.AsFunction()
		for _, p := range f.Params {
			if p.IsPrimitive() {
				return true
			}
		}
		if f.ret.IsPrimitive() {
			return true
		}
		return false
	case Array:
		return t.AsArray().Elem.IsPrimitive()
	default:
		return false
	}
}

//----------------------------------------------------------------------------------------------------------------------

type StructType struct {
	Name string
	Fields []*Symbol
}

func (st *StructType) GetField(name string) *Symbol {
	for _, field := range st.Fields {
		if field.Name == name {
			return field
		}
	}
	return nil // Not found
}

func (st *StructType) HasField(name string) bool {
	return st.GetField(name) != nil
}

//----------------------------------------------------------------------------------------------------------------------

type EnumType struct {
	Name    string
	Members []*FunctionType
}

func (et *EnumType) HasMember(fn *FunctionType) bool {
	for _, con := range et.Members {
		if con == fn {
			return true
		}
	}
	return false
}

//----------------------------------------------------------------------------------------------------------------------

type FuncKind byte
const (
	Normal = FuncKind(iota)
	External // Provided at linktime - no code gen required
	Closure
	StructCons
	EnumCons
)

type FunctionType struct {
	Kind       FuncKind
	Data       interface{}
	Params     []*Type
	Types      []*Type
	ret        *Type
	isVariadic bool
	RawValues  bool
}

// Used during codegen to avoid clashes with shared library functions
func (ft *FunctionType) AsmName(name string) string {
	if ft.Is(External) {
		return name
	}
	if name == "main" {
		return fnPrefix + "main"
	}

	// Build name safe for usage in ASM
	buf := bytes.NewBufferString(fnPrefix)
	buf.WriteString(name)
	for _, param := range ft.Params {
		buf.WriteString(".")
		buf.WriteString(param.AsmName())
	}
	return buf.String()
}

func (ft *FunctionType) Describe(name string) string {
	if ft.Is(External) {
		return fmt.Sprintf("%v (external)", name)
	}

	// Build name safe for usage in ASM
	buf := bytes.NewBufferString(name)
	buf.WriteString("(")
	var types []string
	for _, param := range ft.Params {
		types = append(types, param.String())
	}
	buf.WriteString(strings.Join(types, ", "))
	buf.WriteString(")")
	return buf.String()
}

func (ft *FunctionType) Is(kinds ... FuncKind) bool {
	for _, kind := range kinds {
		if ft.Kind == kind {
			return true
		}
	}
	return false
}

func (ft *FunctionType) AsClosure() *ClosureFunc {
	return ft.Data.(*ClosureFunc)
}

func (ft *FunctionType) AsEnumCons() *EnumConsFunc {
	return ft.Data.(*EnumConsFunc)
}

func (ft *FunctionType) HasTypeParameter(t *Type) bool {
	if !t.Is(Parameter) {
		panic(fmt.Sprintf("Type %v is concrete - cannot be used as a type parameter!", t.String()))
	}
	for _, tt := range ft.Types {
		if tt.Matches(t) {
			return true
		}
	}
	return false
}

//----------------------------------------------------------------------------------------------------------------------

type ClosureFunc struct {
}

//----------------------------------------------------------------------------------------------------------------------

type EnumConsFunc struct {
	Tag int
}

//----------------------------------------------------------------------------------------------------------------------

type ArrayType struct {
	Elem *Type
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

type ParameterType struct {
	Width int
	Name string
}

//----------------------------------------------------------------------------------------------------------------------

type Symbol struct {
	Name      string
	Addr      int
	IsStack   bool
	IsGlobal  bool
	IsLiteral bool
	IsType    bool
	Type      *Type
	Next 	  *Symbol // Only valid for function symbols!
}

func NewStackSym(name string, t *Type) *Symbol {
	return &Symbol{Name: name, IsStack: true, Type: t}
}

func (s *Symbol) Describe() string {
	switch s.Type.Kind {
	case Function:
		f := s.Type.AsFunction()
		var types []string
		for _, param := range f.Params {
			types = append(types, param.String())
		}
		return fmt.Sprintf("%v(%v) %v", s.Name, strings.Join(types, ", "), f.ret.String())
	default:
		panic("Not implemented")
	}
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

func (st *SymTab) Define(s *Symbol) (*Symbol, bool) {
	if s, ok := st.symbols[s.Name]; ok {
		return s, true
	}
	st.symbols[s.Name] = s
	return s, false
}

func (st *SymTab) Owns(s *Symbol) bool {
	sym, ok := st.symbols[s.Name]
	return ok && sym == s
}

func (st *SymTab) Resolve(name string) (*Symbol, bool) {
	s, ok := st.symbols[name]
	if !ok && st.parent != nil {
		s, ok = st.parent.Resolve(name)
	}
	return s, ok
}

func (st *SymTab) ResolveAll(name string, pred func(*Symbol)bool) (*Symbol, bool) {
	for curr := st; curr != nil; curr = curr.parent {
		s, ok := curr.symbols[name]
		if ok && pred(s) {
			return s, true
		}
	}
	return nil, false
}

func (st *SymTab) MustResolve(name string) *Symbol {
	s, ok := st.Resolve(name)
	if !ok {
		panic(fmt.Sprintf("Required symbol '%v' not found!", name))
	}
	return s
}

func (st *SymTab) Parent() *SymTab {
	return st.parent
}

func (st *SymTab) Child() *SymTab {
	child := NewSymtab()
	child.parent = st
	st.children = append(st.children, child)
	return child
}

func (st *SymTab) Walk(f func(*Symbol)) {
	// Walk children first then this node
	for _, child := range st.children {
		child.Walk(f)
	}
	for _, s := range st.symbols {
		f(s)
	}
}

// Unique list of all types in table
func (st *SymTab) allTypes() []*Type {
	t := make(map[string]*Type)
	st.Walk(func(s *Symbol) {
		x := s.Type.AsmName()
		if _, ok := t[x]; !ok {
			t[x] = s.Type
		}
	})
	var typs []*Type
	for _, typ := range t {
		typs = append(typs, typ)
	}
	return typs
}
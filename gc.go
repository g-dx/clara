package main

import (
	"bytes"
)

const closureGc = fnPrefix + "closure_gc"
const gcHeaderSize = ptrSize
// ---------------------------------------------------------------------------------------------------------------------

type GcState struct {
	scopes []int
	roots GcRoots
}

// ---------------------------------------------------------------------------------------------------------------------

type GcRoots []GcRoot

// ---------------------------------------------------------------------------------------------------------------------

type GcRoot struct {
	off int
	typ *Type
}

// ---------------------------------------------------------------------------------------------------------------------

func (gr GcRoots) String() string {
	buf := bytes.NewBufferString("")
	for _, root := range gr {
		buf.WriteString(".")
		buf.WriteString(root.typ.AsmName())
	}
	return buf.String()
}

func (gs *GcState) OpenScope() {
	gs.scopes = append(gs.scopes, len(gs.roots))
}

func (gs *GcState) CloseScope() {
	gs.roots = gs.roots[:gs.scopes[len(gs.scopes)-1]]
}

func (gs *GcState) Add(off int, t *Type) {
	if t.IsPointer() {
		gs.roots = append(gs.roots, GcRoot{ off: off, typ: t })
	}
}

func (gs *GcState) Snapshot() GcRoots {
	return append([]GcRoot{}, gs.roots...)
}

// ---------------------------------------------------------------------------------------------------------------------

type GcTypes struct {
	types []*Type
}

func (gt *GcTypes) AddBuiltins(symtab *SymTab) {

	// Id = 0
	gt.types = append(gt.types, &Type{Kind:Struct, Data: &StructType{Name: "<unknown>"}})

	// ---------------------------------------------------------------------------------
	// NOTE: The following 2 types must appear in this order as their ID is defined in
	// in arrays.clara source code
	//

	// Id = 1
	b := symtab.MustResolve("[]byte")
	gt.types = append(gt.types, b.Type)

	// Id = 2
	i := symtab.MustResolve("[]int")
	gt.types = append(gt.types, i.Type)

	// Id = 3
	sa := symtab.MustResolve("[]string")
	gt.types = append(gt.types, sa.Type)

	// ---------------------------------------------------------------------------------
	// Other heap types which do not have an explicit constructor

	// Id = 4
	s := symtab.MustResolve("string")
	gt.types = append(gt.types, s.Type)
}

func (gt *GcTypes) AssignId(typ *Type) int {
	gt.types = append(gt.types, typ)
	return len(gt.types) - 1
}
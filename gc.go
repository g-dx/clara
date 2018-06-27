package main

import (
	"bytes"
)

const noGc = fnPrefix + "no_gc"
const closureGc = fnPrefix + "closure_gc"
const gcHeaderSize = 8
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
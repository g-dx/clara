package x64
import (
    "bytes"
    "encoding/binary"
    "fmt"
)

// Background
//
// http://www.swansontec.com/sregisters.html
// http://eli.thegreenplace.net/2011/09/06/stack-frame-layout-on-x86-64/
// http://unixwiz.net/techtips/win32-callconv-asm.html/

// Tutorial
// http://www.tptp.cc/mirrors/siyobik.info/view/x86-tutorial/index.html

// main registers of x64
const (

    // x86 (previously prefixed with 'e' instead of 'r') general purpose registers
    Rax = iota
    Rcx
    Rdx
    Rbx

    Rsp // Stack Pointer
    Rbp // Base Pointer
    Rsi // Source Index
    Rdi // Destination Index

    // x64 general purpose registers
    R8
    R9
    R10
    R11
    R12
    R13
    R14
    R15
)

//======================================================================================================================

type Opcode interface {
	Len() uint8
	Bytes() []byte
}

type Rva *uint64

//======================================================================================================================

type SimpleOpcode []byte

func (op SimpleOpcode) Len() uint8 {
	return uint8(len(op))
}

func (op SimpleOpcode) Bytes() []byte {
	return []byte(op)
}

// Relative 'call' opcode
type RelativeCallOpcode struct {
    pos uint64
    dest *uint32
}

func NewRelativeCallOpcode(dest *uint32, pos uint64) *RelativeCallOpcode {
    return &RelativeCallOpcode{pos : pos, dest : dest}
}

func (co RelativeCallOpcode) Len() uint8 {
	return 5 // 1-byte opcode + 32-bit displacement
}

func (co RelativeCallOpcode) Bytes() []byte {
    // Calculate displacement
    dis := int32(uint64(*co.dest) - (co.pos + uint64(co.Len())))
    return op(0xE8).Write(dis).Build().Bytes()
}

//======================================================================================================================

type OpcodeList struct {
    Ops []Opcode
    rva uint64
}

func NewOpcodeList(rva uint64) *OpcodeList {
    return &OpcodeList{rva : rva}
}

// Return current RVA
func (ol *OpcodeList) Rva() uint64 {
    return ol.rva
}

func (ol *OpcodeList) Add(op Opcode) {
	ol.Ops = append(ol.Ops, op)
	ol.rva = ol.rva + uint64(op.Len())
}

func (ol *OpcodeList) ToBuffer() []byte {
	var buf bytes.Buffer
	for _, op := range ol.Ops {
		buf.Write(op.Bytes())
	}
	return buf.Bytes()
}

// Instructions

// Call near, relative, displacement relative to next instruction.
func (ol *OpcodeList) CALL(rva *uint32) {
    ol.Add(NewRelativeCallOpcode(rva, ol.rva))
}

// Call near, absolute indirect, address given in r/m32.
func (ol *OpcodeList) CALLPTR(rva uint64) {
    ol.Add(op(0xFF).Bytes(0x14, 0x25).Write(uint32(rva)).Build())
}

func (ol *OpcodeList) MOV(srcReg uint8, destReg uint8) {
    // set register direct mode
    mod := uint8(0xC0)
    mod = mod | (srcReg << 3)
    mod = mod | destReg
    ol.Add(op(0x8B).Bytes(mod).Build()) // TODO: Verify! What about r8 - r15?
}

// Move data from memory location to register
func (ol *OpcodeList) MOVM(destReg int, srcReg int, dis uint8) {
    if srcReg != Rbp {
        panic("MOVM not implemented for source register other than RBP")
    }
    switch destReg {
        case Rdx:
            ol.Add(op(0x8B).Bytes(0x55).Write(dis).Build())
        case R8:
            ol.Add(op(0x44).Bytes(0x8B, 0x45).Write(dis).Build())
        default:
            panic(fmt.Sprintf("MOVM: %x, %x, displacement:%x - register not implemented yet.", destReg, srcReg, dis))
    }
}

// Move immediate value to register
// TODO:
//    1. Properly handle register addressing
//    2. Update to support 64 bit values
//    3. Double check the representation of negative number is what Microsoft ABI expects
func (ol *OpcodeList) MOVI(destReg int, val int32) {

    switch destReg {
        case Rcx:
            ol.Add(op(0xB9).Write(val).Build())
        case R9:
            ol.Add(op(0x41).Bytes(0xB9).Write(val).Build())
        default:
            panic(fmt.Sprintf("MOVI: %x, %x - register not implemented yet.", destReg, val))
    }
}

// Push from register to top of stack
func (ol *OpcodeList) PUSH(srcReg byte) {
    ol.Add(op(0x50 + srcReg).Build()) // TODO: Verify! What about r8 - r15?
}

// Push immediate value to top of stack
func (ol *OpcodeList) PUSHI(val uint32) {
    ol.Add(op(0x68).Write(val).Build())
}

// Pop from top of stack to register
func (ol *OpcodeList) POP(destReg byte) {
    ol.Add(op(0x58 + destReg).Build()) // TODO: Verify! What about r8 - r15
}

// Return from function
func (ol *OpcodeList) RET() {
    ol.Add(op(0xC3).Build())
}

func (ol *OpcodeList) SUB(reg byte, n uint32) {
    ol.Add(SimpleOpcode{})
}

//======================================================================================================================

type opcodeBuilder struct {
	buf bytes.Buffer
}

func op(opCode uint8) *opcodeBuilder {
	builder := &opcodeBuilder{}
	return builder.Bytes(opCode)
}

func (ob *opcodeBuilder) Bytes(bytes...uint8) *opcodeBuilder {
	ob.Write(bytes)
	return ob
}

func (ob *opcodeBuilder) Build() Opcode {
	return SimpleOpcode(ob.buf.Bytes())
}

func (ob *opcodeBuilder) Write(data interface{}) *opcodeBuilder {
	err := binary.Write(&ob.buf, binary.LittleEndian, data)
	if err != nil {
		panic(err)
	}
	return ob
}
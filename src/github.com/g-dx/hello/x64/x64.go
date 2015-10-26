package x64
import (
    "bytes"
    "encoding/binary"
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

type OpcodeList struct {
    ops []Opcode
    rva uint32
}

func NewOpcodeList(rva uint32) *OpcodeList {
    return &OpcodeList{rva : rva}
}

func (ol *OpcodeList) Add(op Opcode) {
    ol.ops = append(ol.ops, op)
    ol.rva = ol.rva + op.Len()
}

func (ol *OpcodeList) Rva() uint32 {
    return ol.rva
}

//======================================================================================================================

type Opcode interface {
    Len() uint32
}

//======================================================================================================================

type SimpleOpcode []byte

func (op SimpleOpcode) Len() uint32 {
    return uint32(len(op))
}

func CALL(rva uint32) Opcode {
    // 0xFF, (0x14 0x25) is probably the mode to use
    return SimpleOpcode{}
}

func MOV(srcReg int, destReg uint) Opcode {
    // What kind of mov?
    // 0x41 is REX prefix required for referencing rd9, rd8 and other registers
    return SimpleOpcode{}
}

// Move data from memory location to register
func MOVM(srcReg int, destReg uint) Opcode {
    // What kind of mov?
    // 0x41 is REX prefix required for referencing rd9, rd8 and other registers
    return SimpleOpcode{}
}

// Move immediate value to register
func MOVI(destReg int, val int32) Opcode {
    // What kind of mov?
    // 0x41 is REX prefix required for referencing rd9, rd8 and other registers
    return SimpleOpcode{}
}

// Push from register to top of stack
func PUSH(srcReg byte) Opcode {

    // needs to store symbol
    return SimpleOpcode{}
}

// Push immediate value to top of stack
func PUSHI(val uint32) Opcode {

    opCode := 0x68
    if val <= 255 {
        opCode = 0x6A
    }
    var buf bytes.Buffer
    binary.Write(&buf, binary.LittleEndian, opCode)
    binary.Write(&buf, binary.LittleEndian, val)
    return SimpleOpcode(buf.Bytes())
}

// Pop from top of stack to register
func POP(destReg byte) Opcode {

    // needs to store symbol
    return SimpleOpcode{}
}

// Return from function
func RET() Opcode {
    return SimpleOpcode([]byte{0xC3}) // TODO: Verify this is correct
}

func SUB(reg byte, n uint32) Opcode {
    return SimpleOpcode{}
}
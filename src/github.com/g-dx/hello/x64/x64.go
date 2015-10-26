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

type OpcodeList struct {
    Ops []Opcode
    rva uint64
}

func NewOpcodeList(rva uint64) *OpcodeList {
    return &OpcodeList{rva : rva}
}

func (ol *OpcodeList) Add(op Opcode) {
    ol.Ops = append(ol.Ops, op)
    ol.rva = ol.rva + uint64(op.Len())
}

func (ol *OpcodeList) Rva() uint64 {
    return ol.rva
}

//======================================================================================================================

type Opcode interface {
    Len() uint8
}

//======================================================================================================================

type SimpleOpcode []byte

func (op SimpleOpcode) Len() uint8 {
    return uint8(len(op))
}

func CALL(rva uint64) Opcode {
    var buf bytes.Buffer
    leWrite(&buf, []byte { 0xFF, 0x14, 0x25 })
    leWrite(&buf, rva)
    return SimpleOpcode(buf.Bytes())
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
// TODO:
//    1. Properly handle register addressing
//    2. Update to support 64 bit values
//    3. Double check the representation of negative number is what Microsoft ABI expects
func MOVI(destReg int, val int32) Opcode {

    opCode := uint8(0)
    switch destReg {
        case Rcx: opCode = 0xB9
        default:
            panic(fmt.Sprintf("MOVI: %x, %x - register not implemented yet.", destReg, val))
    }

    var buf bytes.Buffer
    leWrite(&buf, opCode)
    leWrite(&buf, val)
    return SimpleOpcode(buf.Bytes())
}

// Push from register to top of stack
func PUSH(srcReg byte) Opcode {

    // needs to store symbol
    return SimpleOpcode{}
}

// Push immediate value to top of stack
func PUSHI(val uint32) Opcode {

    opCode := uint8(0x68)
    if val <= 255 {
        opCode = 0x6A
    }
    var buf bytes.Buffer
    leWrite(&buf, opCode)
    leWrite(&buf, val)
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

func leWrite(buf *bytes.Buffer, data interface{}) {
    err := binary.Write(buf, binary.LittleEndian, data)
    if err != nil {
        panic(err)
    }
}
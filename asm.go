package main

import (
	"io"
	"bufio"
	"fmt"
	"strings"
	"encoding/binary"
	"math"
	"strconv"
	"bytes"
)

type operand interface {
	Print() string
}

// ---------------------------------------------------------------------------------------------------------------------

type memOp struct {
	base       reg
	offset     reg
	displace   int16
	multiplier int16
}

func (mo memOp) Print() string  {

	var buf bytes.Buffer
	if mo.displace != 0 {
		buf.WriteString(strconv.Itoa(int(mo.displace)))
	}
	buf.WriteString("(")
	buf.WriteString(regNames[mo.base]) // TODO: Base is not actually required. Future work may need to omit it
	if mo.offset != 0 {
		buf.WriteString(",")
		buf.WriteString(regNames[mo.offset])
		if mo.multiplier != 0 {
			buf.WriteString(",")
			buf.WriteString(strconv.Itoa(int(mo.multiplier)))
		}
	}
	buf.WriteString(")")
	return buf.String()
}

// ---------------------------------------------------------------------------------------------------------------------

type labelOp string

func (so labelOp) Print() string  {
	return fmt.Sprintf("%v", so)
}

// ---------------------------------------------------------------------------------------------------------------------

type litOp string

func (lo litOp) Print() string  {
	return fmt.Sprintf("$%v", lo)
}


func intOp(i int) operand {
	return litOp(strconv.Itoa(i))
}

func strOp(s string) operand {
	return litOp(s)
}

var _false = intOp(0)
var _true = intOp(1)

// ---------------------------------------------------------------------------------------------------------------------

type reg byte

func (r reg) Print() string  {
	return regNames[r]
}

// Fixed register op
const (
	rax = reg(iota + 1)
	rbx
	rcx
	rdx
	rsi
	rdi
	rbp
	rsp
	r8
	r9
	r10
	r11
	r12
	r13
	r14
	r15
)

var regNames = map[reg]string {
	rax: "%rax",
	rbx: "%rbx",
	rcx: "%rcx",
	rdx: "%rdx",
	rsi: "%rsi",
	rdi: "%rdi",
	rbp: "%rbp",
	rsp: "%rsp",
	r8:  "%r8",
	r9:  "%r9",
	r10: "%r10",
	r11: "%r11",
	r12: "%r12",
	r13: "%r13",
	r14: "%r14",
	r15: "%r15",
}

func (r reg) offset(offset reg) memOp {
	return memOp{ base: r, offset: offset }
}

func (r reg) displace(i int) memOp {
	if i > math.MaxInt16 || i < math.MinInt16 {
		panic(fmt.Sprintf("Cannot represent displacement: %v", i))
	}
	return memOp{ base: r, displace: int16(i) }
}

func (r reg) deref() memOp {
	return memOp{ base: r }
}

// ---------------------------------------------------------------------------------------------------------------------
type inst byte
const (
	movq = iota
	popq
	pushq

	// Pointer management
	leaq

	// Comparisons
	notq
	orq
	andq
	cmpq
	cmovg
	cmovl
	cmove

	// Arithmetic
	addq
	subq
	imulq
	idivq

	// Looping instructions
	jmp
	jne

	// Function support
	leave
	ret
	call
)

var instNames = map[inst]string{
	movq: "movq",
	popq: "popq",
	pushq: "pushq",
	leaq: "leaq",
	notq: "notq",
	orq: "orq",
	andq: "andq",
	cmpq: "cmpq",
	cmovg: "cmovg",
	cmovl: "cmovl",
	cmove: "cmove",
	addq: "addq",
	subq: "subq",
	imulq: "imulq",
	idivq: "idivq",
	jmp: "jmp",
	jne: "jne",
	leave: "leave",
	ret: "ret",
	call: "call",
}

type assembler interface {

	// General
	tab(s... string) // TODO: This is a directive really...
	spacer()
	stringLit(s string) operand
	label(s string)
	newLabel(s string) string
	raw(s string) // Remove me!

	function(name string, temps int)

	op(i inst, ops ... operand)
}

// GNU AS assembler (https://en.wikibooks.org/wiki/X86_Assembly/GAS_Syntax)
type gasWriter struct {
	w *bufio.Writer
	debug bool
	sIndex, lIndex int
}

func NewGasWriter(io io.Writer, debug bool) *gasWriter {
	return &gasWriter { w : bufio.NewWriter(io), debug: debug }
}

func (gw *gasWriter) write(asm string, a...interface{}) {
	asm = fmt.Sprintf(asm, a...)
	if gw.debug {
		fmt.Print(asm)
	}
	_, err := gw.w.WriteString(asm)
	if err != nil {
		panic(err)
	}
	gw.w.Flush()
}

func (gw *gasWriter) tab(s ... string) {
	gw.write(fmt.Sprintf("\t" + strings.Join(s, "\t") + "\n"))
}

func (gw *gasWriter) spacer() {
	gw.write(fmt.Sprintf("\n%s\n\n", strings.Repeat(";", 120)))
}

func (gw *gasWriter) function(name string, temps int) {
	gw.write("\t.globl\t%s\n\t.type\t%s, @function\n%s:\n\tenter\t$(8 * %v), $0\n", name, name, name, temps)
}

func (gw *gasWriter) stringLit(s string) operand {

	label := fmt.Sprintf(".LC%v", gw.sIndex)
	gw.sIndex++

	// Encode length in little endian format
	b := make([]byte, 8)
	binary.LittleEndian.PutUint64(b, uint64(len(s)-2)) // Trim double quotes

	// Generate escaped Go string of raw hex values
	size := fmt.Sprintf("\\x%x\\x%x\\x%x\\x%x\\x%x\\x%x\\x%x\\x%x",
		b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7])

	gw.write("%s:\n\t.ascii \"%v\"\"%v\\x0\"\n", label, size, s[1:len(s)-1])
	return litOp(label)
}

func (gw *gasWriter) newLabel(s string) string {
	label := fmt.Sprintf("%v_%v", s, gw.lIndex)
	gw.lIndex++
	return label
}

func (gw *gasWriter) label(s string) {
	gw.write(fmt.Sprintf("%s:\n", s))
}

func (gw *gasWriter) raw(s string) {
	gw.write(fmt.Sprintf("%v\n", s))
}

func (gw *gasWriter) op(i inst, ops ...operand) {
	s := make([]string, len(ops))
	for i := 0; i < len(s); i++ {
		s[i] = ops[i].Print()
	}
	gw.write("\t%s\t%s\n", instNames[i], strings.Join(s, ", "))
}
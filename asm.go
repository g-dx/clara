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
	base   reg
	off    reg
	disp   int16
	mult   int16
	indir  bool
	deref  bool
}

func (mo memOp) offset(r reg) memOp {
	mo.off = r
	return mo
}

func (mo memOp) multiplier(m int) memOp {
	mo.mult = int16(m)
	return mo
}

func (mo memOp) displace(m int) memOp {
	mo.disp = int16(m)
	return mo
}

func (mo memOp) indirect() memOp {
	mo.indir = true
	return mo
}

func (mo memOp) Print() string  {

	var buf bytes.Buffer
	if mo.indir {
		buf.WriteString("*")
	}
	if mo.disp != 0 {
		buf.WriteString(strconv.Itoa(int(mo.disp)))
	}
	if mo.deref {
		buf.WriteString("(")
	}
	buf.WriteString(regNames[mo.base]) // TODO: Base is not actually required. Future work may need to omit it
	if mo.off != 0 {
		buf.WriteString(",")
		buf.WriteString(regNames[mo.off])
		if mo.mult != 0 {
			buf.WriteString(",")
			buf.WriteString(strconv.Itoa(int(mo.mult)))
		}
	}
	if mo.deref {
		buf.WriteString(")")
	}
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

	// 8-bit
	dil  // rdi
	sil  // rsi
	dl   // rdx
	cl   // rcx
	r8l  // r8
	r9l  // r9
	al   // rax
)

var regNames = map[reg]string{

	// 64-bit
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

	// 8-bit
	dil: "%dil",
	sil: "%sil",
	dl:  "%dl",
	cl:  "%cl",
	r8l: "%r8l",
	r9l: "%r9l",
	al:  "%al",
}

func (r reg) offset(off reg) memOp {
	return r.deref().offset(off)
}

func (r reg) indirect() memOp {
	return memOp{ base: r, indir: true }
}

func (r reg) displace(i int) memOp {
	if i > math.MaxInt16 || i < math.MinInt16 {
		panic(fmt.Sprintf("Cannot represent displacement: %v", i))
	}
	return r.deref().displace(i)
}

func (r reg) deref() memOp {
	return memOp{ base: r, deref: true }
}

func (r reg) _8bit() reg {
	switch r {
	case rax:
		return al
	case rdi:
		return dil
	case rsi:
		return sil
	case rdx:
		return dl
	case rcx:
		return cl
	case r8:
		return r8l
	case r9:
		return r9l
	default:
		panic("No 8-bit version defined for: " + regNames[r])
	}
}

// ---------------------------------------------------------------------------------------------------------------------
type inst byte
const (
	movq = iota
	movb
	movsbq
	popq
	pushq

	// Pointer management
	leaq

	// Comparisons
	notq
	negq
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
	jae
	je

	// Function support
	leave
	enter
	ret
	call
)

var instNames = map[inst]string{
	movq:   "movq",
	movb:   "movb",
	movsbq: "movsbq",
	popq:   "popq",
	pushq:  "pushq",
	leaq:   "leaq",
	notq:   "notq",
	negq:   "negq",
	orq:    "orq",
	andq:   "andq",
	cmpq:   "cmpq",
	cmovg:  "cmovg",
	cmovl:  "cmovl",
	cmove:  "cmove",
	addq:   "addq",
	subq:   "subq",
	imulq:  "imulq",
	idivq:  "idivq",
	jmp:    "jmp",
	jne:    "jne",
	jae:    "jae",
	je:     "je",
	leave:  "leave",
	enter:  "enter",
	ret:    "ret",
	call:   "call",
}

type asmWriter interface {

	// General
	tab(s... string) // TODO: This is a directive really...
	spacer()
	stringLit(s string) operand
	label(s string)
	newLabel(s string) string
	raw(s string) // Remove me!
	addr(sym string)
	function(name string)
	ins(i inst, ops []operand, desc string)
}

// Writer for GNU AS format (https://en.wikibooks.org/wiki/X86_Assembly/GAS_Syntax)
type gasWriter struct {
	w *bufio.Writer
	debug bool
	sIndex, lIndex int
}

func NewGasWriter(io io.Writer, debug bool) *gasWriter {
	return &gasWriter { w : bufio.NewWriter(io), debug: debug }
}

func (gw *gasWriter) write(asm string, a...interface{}) {
	_, err := gw.w.WriteString(fmt.Sprintf(asm, a...))
	if err != nil {
		panic(err)
	}
	gw.w.Flush()
}

func (gw *gasWriter) addr(sym string) {
	gw.write(fmt.Sprintf("   .8byte   %v\n", sym))
}

func (gw *gasWriter) tab(s ... string) {
	gw.write(fmt.Sprintf("   " + strings.Join(s, "   ") + "\n"))
}

func (gw *gasWriter) spacer() {
	gw.write(fmt.Sprintf("\n%s\n\n", strings.Repeat(";", 120)))
}

func (gw *gasWriter) function(name string) {
	gw.tab(".globl", name)
	gw.tab(".type", fmt.Sprintf("%v, @function", name))
	gw.raw(fmt.Sprintf("%v:", name))
}

func (gw *gasWriter) stringLit(s string) operand {

	label := fmt.Sprintf(".LC%v", gw.sIndex)
	gw.sIndex++

	// Encode length in little endian format
	b := make([]byte, 8)
	raw, err := strconv.Unquote(s)
	if err != nil {
		panic(err)
	}
	binary.LittleEndian.PutUint64(b, uint64(len(raw)))

	// Generate escaped Go string of raw hex values
	size := fmt.Sprintf("\\x%x\\x%x\\x%x\\x%x\\x%x\\x%x\\x%x\\x%x",
		b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7])

	// string literal GC header (2 == readonly)
	header := "\\x2\\x0\\x0\\x0\\x0\\x0\\x0\\x0"

	gw.write("%s:\n   .ascii \"%v\"\"%v\"\"%v\\x0\"\n", label, header, size, s[1:len(s)-1])
	return litOp(label+"+8") // Ensure the address points _after_ the header
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

func (gw *gasWriter) ins(i inst, ops []operand, desc string) {
	s := make([]string, len(ops))
	for i := 0; i < len(s); i++ {
		s[i] = ops[i].Print()
	}
	gw.write("   %-8s%-50s", instNames[i], strings.Join(s, ", "))
	if gw.debug {
		gw.write("# %v", desc)
	}
	gw.write("\n")
}
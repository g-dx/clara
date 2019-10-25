package main

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"math"
	"runtime"
	"strconv"
	"strings"
)

type operand interface {
	Print() string
}

// ---------------------------------------------------------------------------------------------------------------------

type memOp struct {
	base  reg
	idx   reg
	disp  int16
	scl   int16
	indir bool
	deref bool
}

func (mo memOp) index(r reg) memOp {
	mo.idx = r
	return mo
}

func (mo memOp) scale(m int) memOp {
	mo.scl = int16(m)
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
	if mo.base != 0 {
		buf.WriteString(regNames[mo.base])
	}
	if mo.idx != 0 {
		buf.WriteString(",")
		buf.WriteString(regNames[mo.idx])
	}
	if mo.scl > 1 {
		buf.WriteString(",")
		buf.WriteString(strconv.Itoa(int(mo.scl)))
	}
	if mo.deref {
		buf.WriteString(")")
	}
	return buf.String()
}

// ---------------------------------------------------------------------------------------------------------------------

type fnOp string

func (fo fnOp) Print() string  {
	if runtime.GOOS == "darwin" {
		return "_" + string(fo) // OSX requires that all global symbols are prefixed with underscores
	}
	return string(fo)
}

// ---------------------------------------------------------------------------------------------------------------------

type symOp string

func (so symOp) Print() string  {
	if runtime.GOOS == "darwin" {
		return fmt.Sprintf("$_%v", so) // OSX requires that all global symbols are prefixed with underscores
	}
	return "$" + string(so)
}
// ---------------------------------------------------------------------------------------------------------------------

type labelOp string

func (so labelOp) Print() string  {
	return string(so)
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
	bl   // rbx
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
	bl:  "%bl",
}

func (r reg) index(off reg) memOp {
	return r.deref().index(off)
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
	case rbx:
		return bl
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
	movq = inst(iota)
	movb
	movsbq
	movabs
	popq
	pushq

	// Pointer management
	leaq

	// Comparisons
	notq
	negq
	orq
	xorq
	andq
	cmpq
	setg
	setl
	sete
	shlq
	sarq

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
	movabs: "movabs",
	popq:   "popq",
	pushq:  "pushq",
	leaq:   "leaq",
	notq:   "notq",
	negq:   "negq",
	orq:    "orq",
	xorq:   "xorq",
	sarq:   "sarq",
	shlq:   "shlq",
	andq:   "andq",
	cmpq:   "cmpq",
	setg:   "setg",
	setl:   "setl",
	sete:   "sete",
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
	addr(op operand)
	function(name string)
	ins(i inst, ops ...operand)
	flush()
	roSymbol(name string, f func(w asmWriter)) operand
	gcMap(name string, offsets []int) labelOp
}

// Writer for GNU AS format (https://en.wikibooks.org/wiki/X86_Assembly/GAS_Syntax)
type gasWriter struct {
	w              *bufio.Writer
	debug          bool
	sIndex, lIndex int
	literals       map[string]string
}

func NewGasWriter(io io.Writer, debug bool) *gasWriter {
	return &gasWriter { w : bufio.NewWriter(io), debug: debug, literals: make(map[string]string) }
}

func (gw *gasWriter) write(asm string, a...interface{}) {
	_, err := gw.w.WriteString(fmt.Sprintf(asm, a...))
	if err != nil {
		panic(err)
	}
	gw.w.Flush()
}

func (gw *gasWriter) addr(op operand) {
	switch op.(type) {
	case symOp:
		gw.tab(".8byte", op.Print()[1:]) // Trim '$'
	case fnOp, labelOp:
		gw.tab(".8byte", op.Print())
	case litOp:
		panic("Cannot output the address of literal")
	}
}

func (gw *gasWriter) tab(s ... string) {
	gw.write(fmt.Sprintf("   " + strings.Join(s, "   ") + "\n"))
}

func (gw *gasWriter) spacer() {
	gw.write(fmt.Sprintf("\n%s\n\n", strings.Repeat(";", 120)))
}

func (gw *gasWriter) function(name string) {

	gw.tab(".text")
	gw.tab(".8byte", "0x2") // "Read-only" GC header

	if runtime.GOOS == "darwin" {
		name = "_" + name
	}
	gw.tab(".globl", name)
	if runtime.GOOS == "linux" {
		gw.tab(".type", fmt.Sprintf("%v, @function", name))
	}
	gw.raw(fmt.Sprintf("%v:", name))
}

func (gw *gasWriter) stringLit(s string) operand {
	const suffix = "+8" // Ensure the address points _after_ the header
	if label, ok := gw.literals[s]; ok {
		return litOp(label + suffix)
	}

	// Create new label
	label := fmt.Sprintf(".LC%v", gw.sIndex)
	gw.sIndex++
	gw.literals[s] = label
	return litOp(label + suffix)
}

func (gw *gasWriter) flush() {
	gw.tab(".data")
	for s, label := range gw.literals {

		raw, err := strconv.Unquote(s)
		if err != nil {
			panic(err)
		}

		// string literal header (2 == readonly, type ID = 4 (string))
		gw.write("%s:\n   .8byte %v\n   .8byte %v\n   .ascii \"%v\\0\"\n",
			label, "0x4000000000002", len(raw), s[1:len(s)-1])
	}
	gw.literals = make(map[string]string) // Clear values
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

func (gw *gasWriter) ins(i inst, ops ...operand) {
	s := make([]string, len(ops))
	for i := 0; i < len(s); i++ {
		s[i] = ops[i].Print()
	}
	gw.write("   %-8s%-50s\n", instNames[i], strings.Join(s, ", "))
}

func (gw *gasWriter) roSymbol(name string, f func(w asmWriter)) operand {
	gw.tab(".8byte", "0x2") // "Read-only" GC header, TODO: Set type ID here
	gw.label(fnOp(name).Print()) // TODO: Fix this mess!
	f(gw)
	return symOp(name)
}

func (gw *gasWriter) gcMap(name string, offsets []int) labelOp {
	gw.label(name)
	gw.tab(".8byte", strconv.Itoa(len(offsets)))
	var s []string
	for _, off := range offsets {
		s = append(s, strconv.Itoa(off))
	}
	gw.tab(".byte", strings.Join(s, ","))
	gw.spacer()
	return labelOp(name)
}

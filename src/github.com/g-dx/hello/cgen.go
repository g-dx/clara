package main
import (
	"encoding/binary"
	"io"
	"fmt"
	"bytes"
	"math"
	"github.com/g-dx/hello/pe"
	"github.com/g-dx/hello/x64"
)

const printlnFuncName = "println"

// Used in GetStdHandle call
const STD_OUTPUT_HANDLE = -11

type ImportList struct {
	imageBase uint64
	imports *pe.Imports
}

func (il * ImportList) funcRva(fn string) uint64 {
	return il.imageBase + il.imports.Rva(fn)
}

func cgenFnCall(node *Node, ops *x64.OpcodeList) {

	// Check function
	fn, ok := node.sym.(*Function)
	if !ok {
		panic(fmt.Sprintf("Unknown symbol: %v, node: %v", node.sym, node))
	}

	switch fn.fnName {
	default:
		ops.CALL(fn.Rva())
	case printlnFuncName:
		cgenPrintCall(node, ops)
	}
}

func cgenFnDecl(node *Node, imports ImportList, ops *x64.OpcodeList) {

	// Get function & set RVA
	fn, ok := node.sym.(*Function)
	if !ok {
		panic(fmt.Sprintf("Unknown symbol: %v, node: %v", node.sym, node.token))
	}
	fn.rva = uint32(ops.Rva())

	ops.PUSH(x64.Rbp)
	ops.MOV(x64.Rbp, x64.Rsp)

	// Generate calls for all functions
	for _, n := range node.stats {
		cgenFnCall(n, ops)
	}

	// Check if we are main - we need to exit!
	if node.token.val == "main" {
		ops.MOVI(x64.Rcx, 0)
		ops.CALLPTR(imports.funcRva("ExitProcess"))
	} else {
		// TODO: When local variables added must update this!
		ops.MOV(x64.Rsp, x64.Rbp)
		ops.POP(x64.Rbp)
		ops.RET()
	}
}

func cgenPrintCall(node *Node, ops *x64.OpcodeList) {

	// Get symbol
	fn, ok := node.sym.(*Function)
	if !ok {
		panic(fmt.Sprintf("Unknown symbol: %v", node.sym))
	}

	// Push the base/frame pointer register value onto stack and overwrite base/frame pointer register to point to
	// the top of stack. This stack pointer can now be modified by us and restored at the end of the function to its
	// previous state

	// TODO: commented out as we must inline this call
//	ops.PUSH(x64.Rbp)
//	ops.MOV(x64.Rbp, x64.Rsp)

	// Push values onto stack (string RVA & length)

	str, ok := node.stats[0].sym.(*StringLiteralSymbol)
	if !ok {
		panic(fmt.Sprintf("print function parameter not string literal! - %v", str))
	}
	ops.PUSHI(str.Rva())
	ops.PUSHI(uint32(len(str.Val())))

	// Call print

	ops.CALL(fn.Rva())

	// Clean up stack by overwriting the stack pointer register with the base/frame pointer register. This discards
	// the two values we pushed onto the stack. Then pop the old base/frame pointer value off the stack & back into
	// the base/frame pointer register. This restores the caller's stack.
	// TODO: on x64 we should use 'leave' instruction

//	ops.MOV(x64.Rsp, x64.Rbp)
//	ops.POP(x64.Rbp)
//	ops.RET()
}

func cgenPrintDecl(node *Node, imports ImportList, ops *x64.OpcodeList) {

	// Get function & set RVA
	fn, ok := node.sym.(*Function)
	if !ok {
		panic(fmt.Sprintf("Unknown symbol: %v", node.sym))
	}
	fn.rva = uint32(ops.Rva())

	ops.PUSH(x64.Rbp)
	ops.MOV(x64.Rbp, x64.Rsp)

	// Get the output handle

	ops.MOVI(x64.Rcx, STD_OUTPUT_HANDLE)
	ops.CALLPTR(imports.funcRva("GetStdHandle"))

	// Write to console
	// See [https://msdn.microsoft.com/en-us/library/ms235286.aspx] for more information

	ops.MOV(x64.Rcx, x64.Rax)      // Copy output handle result from AX into register
	ops.MOVM(x64.Rdx, x64.Rbp, 24) // Move string literal RVA into register
	ops.MOVM(x64.R8, x64.Rbp, 16)  // Move string length into register
	ops.MOVI(x64.R9, 0)            // TODO: This should be a local variable on the stack!
	ops.PUSHI(0)
	ops.CALLPTR(imports.funcRva("WriteConsoleA"))

	// Clean stack

	ops.MOV(x64.Rsp, x64.Rbp)
	ops.POP(x64.Rbp)
	ops.RET()
}

func codegen(symtab SymTab, tree *Node, writer io.Writer) error {

	// Wrap for convenience
	w := &leWriter{w : writer, debug : true}

	// Write PE header
	w.Write(pe.NewDosHeader())

	// Configure optional header
	optionalHeader := pe.NewOptionalHeader64()
	optionalHeader.AddressOfEntryPoint = 0x3000
	optionalHeader.SectionAlignment = 0x1000
	optionalHeader.FileAlignment = 0x200
	optionalHeader.SizeOfImage = 0x4000 // Address of entry point (0x1000) + (3 sections @ 0x1000 VM size)
	optionalHeader.SizeOfHeaders = 0x200

	// Define an "Imports" data directory
	optionalHeader.Rvas[1].VirtualAddress = 0x1000 // Declared below when writing RDATA section

	// Configure file header
	fileHeader := pe.NewFileHeader64(&optionalHeader)
	fileHeader.NumberOfSections = 3

	// Build & write PE header
	peHeader := pe.NewPeHeader64()
	peHeader.FileHeader = fileHeader
	peHeader.OptionalHeader = optionalHeader
	w.Info("\n.PE Header, Optional Header & Data directories")
	w.Write(peHeader)

	// Write sections (RDATA, DATA & TEXT)
	rdataSection := pe.SectionHeader{}
	copy(rdataSection.Name[:], ".rdata\x00")
	rdataSection.VirtualSize = 0x1000
	rdataSection.VirtualAddress = 0x1000
	rdataSection.SizeOfRawData = 0x200
	rdataSection.PointerToRawData = 0x200
	rdataSection.Characteristics = 0x40000040
	w.Info("\n.RDATA Section")
	w.Write(rdataSection)

	dataSection := pe.SectionHeader{}
	copy(dataSection.Name[:], ".data\x00")
	dataSection.VirtualSize = 0x1000
	dataSection.VirtualAddress = 0x2000
	dataSection.SizeOfRawData = 0x200
	dataSection.PointerToRawData = 0x400
	dataSection.Characteristics = 0xC0000040
	w.Info("\n.DATA Section")
	w.Write(dataSection)

	textSection := pe.SectionHeader{}
	copy(textSection.Name[:], ".text\x00")
	textSection.VirtualSize = 0x1000
	textSection.VirtualAddress = 0x3000
	textSection.SizeOfRawData = 0x200
	textSection.PointerToRawData = 0x600
	textSection.Characteristics = 0x60000020
	w.Info("\n.TEXT Section")
	w.Write(textSection)

	// Pad
	w.Info("\nPadding")
	w.Pad(uint64(peHeader.OptionalHeader.FileAlignment), 0x00)

	// =================================================================================================================

	w.Info("\n.RDATA")

	// Create imports for DLLs
	imports := pe.NewImports(rdataSection.VirtualAddress)
	imports.Module("kernel32.dll", "ExitProcess", "GetStdHandle", "WriteConsoleA")
	w.Write(imports.ToBuffer().Bytes())
	w.Pad(0x400, 0x00)

	// =================================================================================================================
	addDataSection(optionalHeader.ImageBase, dataSection, w, symtab)

	w.Pad(0x600, 0x00)

	// =================================================================================================================

	w.Info("\n.TEXT (x64 assembly)")

	// Create import list
	im := ImportList{imports : imports, imageBase : optionalHeader.ImageBase}

//	tree.Walk(func(n *Node) {
//		fmt.Printf("Node: %v, Type: %v, Sym: %v\n", n.token.val, nodeTypes[n.op], n.sym)
//	})

	// Generate code!
	ops := x64.NewOpcodeList(optionalHeader.ImageBase + uint64(textSection.VirtualAddress))
	tree.Walk(func(n *Node) {
		switch n.op {
		case opFuncDcl:
			if (n.token.val == printlnFuncName) {
				cgenPrintDecl(n, im, ops)
			} else {
				cgenFnDecl(n, im, ops)
			}
		case opFuncCall:
			if (n.token.val == printlnFuncName) {
				cgenPrintCall(n, ops)
			}
		default:
			fmt.Printf("Skipping: %v\n", nodeTypes[n.op])
		}
	})

	// Write opcode & buffer with no-ops
	w.Write(ops.ToBuffer())
	w.Pad(0x800, 0x90)

	// =================================================================================================================

	w.Finish()
	return w.err
}

func addDataSection(imageBase uint64, dataSection pe.SectionHeader, w *leWriter, symtab SymTab) {

	w.Info("\n.DATA (strings)")

	// RVA position
	pos := uint32(imageBase) + dataSection.VirtualAddress

	// Walk symbol table and assign RVAs
	symtab.Walk(func(sym Symbol) {
		if str, ok := sym.(*StringLiteralSymbol); ok {
			// Set RVA & write
			str.rva = pos
			// Remove leading and trailing double quotes and add newline
			str.val = str.val[1:len(str.val)-1] + "\x0D\x0A"
			w.Write([]byte(str.val))
			pos += uint32(len(str.val))
		}
	})
}

// From https://blog.golang.org/errors-are-values
type leWriter struct {
	w   io.Writer
	err error
	debug bool
	pos uint64
}

func (ew *leWriter) Write(data interface{}) {
	if ew.err != nil {
		return
	}

	if ew.debug {
		// Write
		var buf bytes.Buffer
		binary.Write(&buf, binary.LittleEndian, data)

		// Chop into 30 byte chunks
		for i := 0; i < buf.Len(); i+=30 {
			b := buf.Bytes()[i:int(math.Min(float64(buf.Len()), float64(i+30)))]
			if i == 0 {
				fmt.Printf("0x%-4x | %-3d | %x\n", ew.pos, buf.Len(), b)
			} else {
				fmt.Printf("       |     | %x\n", b)
			}
		}

		// Update pos
		ew.pos += uint64(buf.Len())
	}

	ew.err = binary.Write(ew.w, binary.LittleEndian, data)
}

func (ew *leWriter) Pad(pos uint64, val byte) {
	nop := make([]byte, pos-ew.pos)
	for i, _ := range nop {
		nop[i] = val
	}
	ew.Write(nop)
}

func (ew *leWriter) Info(s string) {
	if ew.debug {
		fmt.Println(s)
	}
}

func (ew *leWriter) Finish() {
	if ew.debug {
		fmt.Printf("\nDone @ 0x%-4x\n", ew.pos)
	}
}
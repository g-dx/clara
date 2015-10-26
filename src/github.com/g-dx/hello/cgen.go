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


/*

	Windows syscalls:

	- When encountering a print("...") in AST generate ASM to invoke this function
	 -- What about RVA of string literal? We don't know this when generating opcodes. We
	 could just pick an image base (default to 0x400000?) and build a .data section as
	 we go?

	 baseRva := uint64(0x400000)
	 strings := Strings { baseRva }
	 string.Add(<string lit>)  Adds NULL termination
	 // Add all string literals

	 //...

	 // Generate opcodes for print
	 node.name == 'print'
	 instructions.Add(x64.MOV{ x64.rd9, Val(0) })
	 instructions.Add(x64.MOV{ x64.rd9, StringRva(node.stats[0].val) })
	 instructions.Add(x64.CALL{ FuncRva("ConsoleWrite") })

	 type OpCode {
	 	resolveRVAs(s Strings, Functions fns)
	 }


 */

func cgen(rva uint) {

	// TODO: need opcode writer to keep track of rva and to write little endian

	// - Ensure all string literals have RVAs
	// - Walk AST from leaf nodes up generating function declarations. This way
	//   all functions will have an RVA _before_ we have to call them.
	// - Main() should be the last function generated
	// - Return RVA of main and set this value = optionalHeader.AddressOfEntryPoint

	//	opcodes = append(opcodes, x64.)

	var ops x64.OpcodeList


	// MOVI 0
	ops.Add(x64.MOVI(x64.Rcx, 0))
	// CALL (ExitProcess)
	ops.Add(x64.CALL(0)) // TODO: need RVA for MSDN function!
}

func fnCall(fn *Function, ops x64.OpcodeList) {

	// TODO: If this function had parameters we would need to push them here

	ops.Add(x64.CALL(fn.Rva()))
}

func fnDec(node *Node, ops x64.OpcodeList) {

	// TODO: Store current RVA into *Node so other functions can call us!

	// PUSH RBP
	ops.Add(x64.PUSH(x64.Rbp))
	// MOV RBP RSP
	ops.Add(x64.MOV(x64.Rbp, x64.Rsp))

	// TODO:
	// for _, fn := range dec.stats {
	//    fnCall(fn)
	// }

	// TODO: When local variables added must update this!
	// POP RBP
	ops.Add(x64.POP(x64.Rbp))
	// RET
	ops.Add(x64.RET())
}

func printCall(fn *Function, str *StringLiteralSymbol, ops x64.OpcodeList) {

	// Push the base/frame pointer register value onto stack and overwrite base/frame pointer register to point to
	// the top of stack. This stack pointer can now be modified by us and restored at the end of the function to its
	// previous state

	// PUSH RBP
	ops.Add(x64.PUSH(x64.Rbp))
	// MOV RBP RSP
	ops.Add(x64.MOV(x64.Rbp, x64.Rsp))

	// Push values onto stack

	// MOVL RDI (string address)
	ops.Add(x64.PUSHI(str.Rva()))
	// MOVL RDI (string length)
	ops.Add(x64.PUSHI(uint32(len(str.Val()))))

	// Call print

	ops.Add(x64.CALL(fn.Rva()))

	// Clean up stack by overwriting the stack pointer register with the base/frame pointer register. This discards
	// the two values we pushed onto the stack. Then pop the old base/frame pointer value off the stack & back into
	// the base/frame pointer register. This restores the caller's stack.
	// TODO: on x64 we should use 'leave' instruction

	// MOV RSP RBP
	ops.Add(x64.MOV(x64.Rsp, x64.Rbp))
	// POP RBP
	ops.Add(x64.POP(x64.Rbp))
	// RET
	ops.Add(x64.RET())
}

func printDecl(call *Node, ops x64.OpcodeList) {

	// PUSH RBP
	ops.Add(x64.PUSH(x64.Rbp))
	// MOV RBP RSP
	ops.Add(x64.MOV(x64.Rbp, x64.Rsp))

	// Get the output handle

	// MOVI (STD_OUTPUT_HANDLE)
	ops.Add(x64.MOVI(x64.Rcx, -11)) // TODO: how to we pass negative values?
	// CALL (GetStdHandle)
	ops.Add(x64.CALL(0))  // TODO: need RVA for MSDN function!

	// Write to console

	// PUSH 0
	ops.Add(x64.PUSHI(0))
	// MOVL 0
	ops.Add(x64.MOVI(x64.R9, 0)) // TODO: Does this need to be a local variable?
	// MOVM r8 [string length value] - copy value at address
	ops.Add(x64.MOVM(x64.R8, 0)) // String length
	// MOVM rdx [string memory location]
	ops.Add(x64.MOVM(x64.Rdx, 0)) // String RVA
	// MOVI
	ops.Add(x64.MOV(x64.Rdx, x64.Rax)) // Copy output handle result from AX to input parameter
	// CALL (WriteConsoleA)
	ops.Add(x64.CALL(0)) // TODO: need RVA for MSDN function!

	// Clean stack

	// MOV RSP RBP
	ops.Add(x64.MOV(x64.Rsp, x64.Rbp))
	// POP RBP
	ops.Add(x64.POP(x64.Rbp))
	// RET
	ops.Add(x64.RET())
}

func writePE(writer io.Writer) error {

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
	fmt.Println("\n.PE Header, Optional Header & Data directories")
	w.Write(peHeader)

	// Write sections (RDATA, DATA & TEXT)
	rdataSection := pe.SectionHeader{}
	copy(rdataSection.Name[:], ".rdata\x00")
	rdataSection.VirtualSize = 0x1000
	rdataSection.VirtualAddress = 0x1000
	rdataSection.SizeOfRawData = 0x200
	rdataSection.PointerToRawData = 0x200
	rdataSection.Characteristics = 0x40000040
	fmt.Println("\n.RDATA Section")
	w.Write(rdataSection)

	dataSection := pe.SectionHeader{}
	copy(dataSection.Name[:], ".data\x00")
	dataSection.VirtualSize = 0x1000
	dataSection.VirtualAddress = 0x2000
	dataSection.SizeOfRawData = 0x200
	dataSection.PointerToRawData = 0x400
	dataSection.Characteristics = 0xC0000040
	fmt.Println("\n.DATA Section")
	w.Write(dataSection)

	textSection := pe.SectionHeader{}
	copy(textSection.Name[:], ".text\x00")
	textSection.VirtualSize = 0x1000
	textSection.VirtualAddress = 0x3000
	textSection.SizeOfRawData = 0x200
	textSection.PointerToRawData = 0x600
	textSection.Characteristics = 0x60000020
	fmt.Println("\n.TEXT Section")
	w.Write(textSection)

	// Pad
	fmt.Println("\nPadding")
	w.Pad(uint64(peHeader.OptionalHeader.FileAlignment), 0x00)

	fmt.Println("\n.RDATA")

	// Create imports for DLLs
	imports := pe.NewImports(rdataSection.VirtualAddress)
	imports.Module("kernel32.dll", "ExitProcess", "GetStdHandle", "WriteConsoleA")
	w.Write(imports.ToBuffer().Bytes())
	w.Pad(0x400, 0x00)

	fmt.Println("\n.DATA (strings)")

	// Calculate RVA
	//	startingRva := optionalHeader.ImageBase + dataSection.VirtualAddress
	w.Write([]byte("Hello world!\x00"))
	// TODO: Add this RVA to the symbol in the SymbolTable for this string literal
	w.Pad(0x600, 0x00)

	fmt.Println("\n.TEXT (x64 assembly)")

	w.Write([]byte { 0xB9, 0x00, 0x00, 0x00, 0x00  })
	w.Write([]byte { 0xFF, 0x14, 0x25 })
	w.Write(optionalHeader.ImageBase + imports.Rva("ExitProcess"))

	w.Pad(0x800, 0x90) // Write all no-ops
	w.Finish()
	return w.err
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

func (ew *leWriter) Finish() {
	if ew.debug {
		fmt.Printf("\nDone @ 0x%-4x\n", ew.pos)
	}
}
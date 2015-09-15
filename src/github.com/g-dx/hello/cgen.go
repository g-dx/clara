package main
import (
	"encoding/binary"
	"io"
	"fmt"
	"bytes"
	"math"
	"github.com/g-dx/hello/pe"
)

/**
Code Generation:

WINDOWS TODO:

 - Need to generate:
  * PE Header
  * .rdata section with all string literals
  * .text section with entry point and all function definitions

MACH-O TODO:

- Need to generate:
  *

*/

/*

	Windows syscalls:

	- Always import kernel32.ConsoleWrite
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

	 // Once PE built we

	 // How does funcs get initialised?

 */

func cgen() {

}

func writePE(writer io.Writer) error {

	// Wrap for convenience
	w := &leWriter{w : writer, debug : true}

	// Write PE header
	w.Write(pe.NewDosHeader())

	// Configure optional header
	optionalHeader := pe.NewOptionalHeader64()
	optionalHeader.AddressOfEntryPoint = 0x1000
	optionalHeader.SectionAlignment = 0x1000
	optionalHeader.FileAlignment = 0x200
	optionalHeader.SizeOfImage = 0x4000 // Address of entry point (0x1000) + (3 sections @ 0x1000 VM size)
	optionalHeader.SizeOfHeaders = 0x200

	// Define an "Imports" data directory
	optionalHeader.Rvas[1].VirtualAddress = 0x2000 // Declared below when writing RDATA section

	// Configure file header
	fileHeader := pe.NewFileHeader64(&optionalHeader)
	fileHeader.NumberOfSections = 3

	// Build & write PE header
	peHeader := pe.NewPeHeader64()
	peHeader.FileHeader = fileHeader
	peHeader.OptionalHeader = optionalHeader
	fmt.Println("\n.PE Header, Optional Header & Data directories")
	w.Write(peHeader)

	// Write sections (TEXT, RDATA & DATA)
	textSection := pe.SectionHeader{}
	copy(textSection.Name[:], ".text\x00")
	textSection.VirtualSize = 0x1000
	textSection.VirtualAddress = 0x1000
	textSection.SizeOfRawData = 0x200
	textSection.PointerToRawData = 0x200
	textSection.Characteristics = 0x60000020
	fmt.Println("\n.TEXT Section")
	w.Write(textSection)

	rdataSection := pe.SectionHeader{}
	copy(rdataSection.Name[:], ".rdata\x00")
	rdataSection.VirtualSize = 0x1000
	rdataSection.VirtualAddress = 0x2000
	rdataSection.SizeOfRawData = 0x200
	rdataSection.PointerToRawData = 0x400
	rdataSection.Characteristics = 0x40000040
	fmt.Println("\n.RDATA Section")
	w.Write(rdataSection)

	dataSection := pe.SectionHeader{}
	copy(dataSection.Name[:], ".data\x00")
	dataSection.VirtualSize = 0x1000
	dataSection.VirtualAddress = 0x3000
	dataSection.SizeOfRawData = 0x200
	dataSection.PointerToRawData = 0x600
	dataSection.Characteristics = 0xC0000040
	fmt.Println("\n.DATA Section")
	w.Write(dataSection)

	// Pad
	fmt.Println("\nPadding")
	w.Pad(uint64(peHeader.OptionalHeader.FileAlignment), 0x00)

	fmt.Println("\n.TEXT (x64 assembly)")
	// TODO: Assembly code goes here!

	/**
	- Call ConsoleWrite(...) in kernel with string

mov     r10, rcx
mov     eax, (syscall number)
syscall
retn
	*/
//
	w.Write([]byte { 0xB9, 0x00, 0x00, 0x00, 0x00  })
	w.Write([]byte { 0xFF, 0x14, 0x25 })
	w.Write(uint32(0x402040)) // TODO: This is hack!

	w.Pad(0x400, 0x90) // Write all no-ops

	fmt.Println("\n.RDATA")

	var buf bytes.Buffer

	// Write descriptors
	kernel32 := pe.ImportDescriptor{}
	binary.Write(&buf, binary.LittleEndian, kernel32)
	binary.Write(&buf, binary.LittleEndian, pe.ImportDescriptor{}) // Terminating descriptor

	// Write Thunk arrays
	originalFirstThunk := make([]uint64, 3) // Import 2 func rva + 1 zero rva = size (3)
	firstThunk := make([]uint64, 3)

	startingRva := rdataSection.VirtualAddress
	kernel32.OriginalFirstThunk = startingRva + uint32(buf.Len())
	binary.Write(&buf, binary.LittleEndian, originalFirstThunk)

	kernel32.FirstThunk = startingRva + uint32(buf.Len())
	fmt.Printf("ExitProcess: %x\n", kernel32.FirstThunk)
	binary.Write(&buf, binary.LittleEndian, firstThunk)

	// Write ImportByName structures
	originalFirstThunk[0] = uint64(startingRva) + uint64(buf.Len())
	firstThunk[0] = uint64(startingRva) + uint64(buf.Len())

	exitProcess := pe.NewImportByName("ExitProcess");
	binary.Write(&buf, binary.LittleEndian, exitProcess)

	originalFirstThunk[1] = uint64(startingRva) + uint64(buf.Len())
	firstThunk[1] = uint64(startingRva) + uint64(buf.Len())

	writeConsole := pe.NewImportByName("WriteConsoleA");
	binary.Write(&buf, binary.LittleEndian, writeConsole)

	// Write names of DLL to import
	kernel32.Name = startingRva + uint32(buf.Len())
	binary.Write(&buf, binary.LittleEndian, []uint8("kernel32.dll\x00"));

	// Now write populated structures into output
	w.Write(kernel32)
	w.Write(pe.ImportDescriptor{})
	w.Write(originalFirstThunk)
	w.Write(firstThunk)
	w.Write(exitProcess)
	w.Write(writeConsole)
	w.Write([]uint8("kernel32.dll\x00"));

	w.Pad(0x600, 0x00)

	fmt.Println("\n.DATA (strings)")

	// Calculate RVA
//	startingRva := optionalHeader.ImageBase + dataSection.VirtualAddress
	w.Write([]byte("Hello world!\x00"))
	// TODO: Add this RVA to the symbol in the SymbolTable for this string literal
	w.Pad(0x800, 0x00)
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
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
	optionalHeader.SizeOfImage = 0x3000 // Address of entry point (0x1000) + (2 sections @ 0x1000 VM size)
	optionalHeader.SizeOfHeaders = 0x200

	// Configure file header
	fileHeader := pe.NewFileHeader64(&optionalHeader)
	fileHeader.NumberOfSections = 2

	// Build & write PE header
	peHeader := pe.NewPeHeader64()
	peHeader.FileHeader = fileHeader
	peHeader.OptionalHeader = optionalHeader
	fmt.Println("\n.PE Header, Optional Header & Data directories")
	w.Write(peHeader)

	// Write sections (TEXT & DATA)
	textSection := pe.SectionHeader{}
	textSection.Name = [8]byte { 0x2E, 0x74, 0x65, 0x78, 0x74, 0, 0, 0 } // Annoying!
	textSection.VirtualSize = 0x1000
	textSection.VirtualAddress = 0x1000
	textSection.SizeOfRawData = 0x200
	textSection.PointerToRawData = 0x200
	textSection.Characteristics = 0x60000020
	fmt.Println("\n.TEXT Section")
	w.Write(textSection)

	dataSection := pe.SectionHeader{}
	dataSection.Name = [8]byte { 0x2E, 0x64, 0x61, 0x74, 0x61, 0, 0, 0 } // Annoying!
	dataSection.VirtualSize = 0x1000
	dataSection.VirtualAddress = 0x2000
	dataSection.SizeOfRawData = 0x200
	dataSection.PointerToRawData = 0x400
	dataSection.Characteristics = 0xC0000040
	fmt.Println("\n.DATA Section")
	w.Write(dataSection)

	// Pad
	fmt.Println("\nPadding")
	w.Pad(uint64(peHeader.OptionalHeader.FileAlignment))

	fmt.Println("\n.TEXT (x64 assembly)")
	// TODO: Assembly code goes here!
	w.Pad(0x400)

	fmt.Println("\n.DATA (strings)")
	w.Write([]byte("Hello world!\x00"))
	w.Pad(0x600)
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

func (ew *leWriter) Pad(pos uint64) {
	ew.Write(make([]byte, pos-ew.pos))
}

func (ew *leWriter) Finish() {
	if ew.debug {
		fmt.Printf("\nDone @ 0x%-4x\n", ew.pos)
	}
}
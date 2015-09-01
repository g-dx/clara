package main
import (
	"encoding/binary"
	"io"
	"fmt"
	"bytes"
	"math"
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
	w.Write([]byte("MZ"))
	w.Write(make([]byte, 58)) // Padding
	w.Write([]byte{0x40, 0, 0, 0})
	fmt.Println("\nPE Header")

	w.Write([]byte("PE\x00\x00"))
	w.Write(uint16(0x8684)) // AMD 64 arch
	w.Write(uint16(2))      // No of sections
	w.Write(make([]byte, 12))
	w.Write(uint16(0xF0))       // Size of option
	w.Write(uint16(0x102)) // TODO: Should be 64b exe!, Maybe not!
	fmt.Println("\nOptional Header")

	// Write Optional Header
	w.Write(byte(0x20)) // 64-bit app
	w.Write(make([]byte, 14))
	w.Write(uint16(0x1000)) // Entry point
	w.Write(make([]byte, 8))
	w.Write(uint32(0x400000)) // Mapping address
	w.Write(uint32(0x1000)) // Section alignment
	w.Write(uint32(0x200)) // File alignment
	w.Write(make([]byte, 8))
	w.Write(uint16(4))
	w.Write(make([]byte, 6))
	w.Write(uint32(0x4000)) // Total memory space required
	w.Write(uint32(0x200)) // Total size of headers
	w.Write(make([]byte, 4))
	w.Write(uint16(3)) // Subsystem GUI,commandline, etc
	w.Write(make([]byte, 22))
	w.Write(uint32(0x10)) // No of RVA and sizes
	fmt.Println("\nData Directories")

	// Write data directories
	w.Write(make([]byte, 8))
	w.Write(uint32(0x10)) // RVA of data directories
	w.Write(make([]byte, 0x130-w.pos)) // Add padding!!!!
	fmt.Println("\n.TEXT Section")

	// Write sections (TEXT)
	w.Write(make([]byte, 8))
	w.Write([]byte(".text\x00\x00\x00"))
	w.Write(uint32(0x1000))
	w.Write(uint32(0x1000))
	w.Write(uint32(0x200))
	w.Write(uint32(0x200))
	w.Write(make([]byte, 12))
	w.Write([]byte{0x20, 0, 0, 0x60})
	fmt.Println("\n.DATA Section")

	w.Write([]byte(".data\x00\x00\x00"))
	w.Write(uint32(0x1000))
	w.Write(uint32(0x2000))
	w.Write(uint32(0x200))
	w.Write(uint32(0x400))
	w.Write([]byte{0x40, 0, 0, 0xC0})
	w.Write(make([]byte, 0x200-w.pos)) // Add padding!!!!
	fmt.Println("\n.TEXT (x64 assembly)")

	// Actually write .TEXT Section
	// TODO: Must have correct physical offset 0x200 & 0x400 so may need to pad?
	// TODO: Assembly code goes here!

	w.Write(make([]byte, 0x400-w.pos)) // Add padding!!!!
	fmt.Println("\n.DATA (strings)")

	w.Write([]byte("Hello world!\x00"))

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

func (ew *leWriter) Finish() {
	if ew.debug {
		fmt.Printf("\nDone @ 0x%-4x\n", ew.pos)
	}
}
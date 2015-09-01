package main
import (
	"encoding/binary"
	"io"
	"fmt"
	"bytes"
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

	// Write header
	w.Write([]byte("MZ"))
	w.Write(make([]byte, 58))
	w.Write([]byte{0x40, 0, 0, 0})
	w.Write([]byte("PE"))
	w.Write([]byte{0, 0})
	w.Write(uint16(0x8684)) // AMD 64 arch
	w.Write(uint16(2))      // No of sections
	w.Write(make([]byte, 12))
	w.Write(uint16(0xF0))       // Size of option
	w.Write(uint16(0x102)) // TODO: Should be 64b exe!

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
		n := binary.Size(data)
		var buf bytes.Buffer
		binary.Write(&buf, binary.LittleEndian, data)
		fmt.Printf("0x%-4x | %-2d | %x\n", ew.pos, n, buf.Bytes())
		ew.pos += uint64(n)
	}

	ew.err = binary.Write(ew.w, binary.LittleEndian, data)
}

func (ew *leWriter) Finish() {
	if ew.debug {
		fmt.Printf("0x%-4x\n", ew.pos)
		fmt.Println("Done")
	}
}
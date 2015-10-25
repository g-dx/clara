package x64

// Background
//
// http://www.swansontec.com/sregisters.html
// http://eli.thegreenplace.net/2011/09/06/stack-frame-layout-on-x86-64/
// http://unixwiz.net/techtips/win32-callconv-asm.html/

/*

 Register: Stack Pointer (Contains Memory Address)
 Register: Frame Pointer (Contains Memory Address)

 */


// main registers of x64
const (

	// x86 (previously prefixed with 'e' instead of 'r') general purpose registers
	rax = iota
	rbx,
	rcx,
	rdx,

	rbp, // Base Pointer
	rsp, // Stack Pointer
	rsi, // Source Index
	rdi, // Destination Index

	// x64 general purpose registers
	r8,
	r9,
	r10,
	r11,
	r12,
	r13,
	r14,
	r15
)

// x64 opcodes
const (


)
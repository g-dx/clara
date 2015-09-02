package pe
import "encoding/binary"

type DosHeader struct {
	e_magic [2]byte;
	padding [58]byte;  // TODO: add actual fields here
	e_lfanew uint32;
}

func NewDosHeader() *DosHeader {
	return &DosHeader{ [2]byte{ 0x4d, 0x5a }, [58]byte{}, 0x40 }
}

type PeHeader struct {
	Signature      uint32
	FileHeader     FileHeader
	OptionalHeader OptionalHeader
}

type PeHeader64 struct {
	Signature      uint32
	FileHeader     FileHeader
	OptionalHeader OptionalHeader64
}

func NewPeHeader64() PeHeader64 {
	return PeHeader64{
		Signature: 0x4550, // 'PE', 0, 0
	}
}

type FileHeader struct {
	Machine              uint16
	NumberOfSections     uint16
	TimeDateStamp        uint32
	PointerToSymbolTable uint32
	NumberOfSymbols      uint32
	SizeOfOptionalHeader uint16
	Characteristics      uint16
}

func NewFileHeader64(optionalHeader *OptionalHeader64) FileHeader {
	if optionalHeader == nil {
		panic("Optional header cannot be nil")
	}

	// https://msdn.microsoft.com/en-us/library/windows/desktop/ms680313(v=vs.85).aspx
	return FileHeader{
		Machine: 0x8664,            // x64
		SizeOfOptionalHeader: uint16(binary.Size(optionalHeader)),
		Characteristics: 0x102,     // Executable that supports 32-bit words
	}
}

type OptionalHeader struct {
	Magic                       uint16
	MajorLinkerVersion          uint8
	MinorLinkerVersion          uint8
	SizeOfCode                  uint32
	SizeOfInitializedData       uint32
	SizeOfUninitializedData     uint32
	AddressOfEntryPoint         uint32
	BaseOfCode                  uint32
	BaseOfData                  uint32
	ImageBase                   uint32
	SectionAlignment            uint32
	FileAlignment               uint32
	MajorOperatingSystemVersion uint16
	MinorOperatingSystemVersion uint16
	MajorImageVersion           uint16
	MinorImageVersion           uint16
	MajorSubsystemVersion       uint16
	MinorSubsystemVersion       uint16
	Win32VersionValue           uint32
	SizeOfImage                 uint32
	SizeOfHeaders               uint32
	CheckSum                    uint32
	Subsystem                   uint16
	DllCharacteristics          uint16
	SizeOfStackReserve          uint32
	SizeOfStackCommit           uint32
	SizeOfHeapReserve           uint32
	SizeOfHeapCommit            uint32
	LoaderFlags                 uint32
	NumberOfRvaAndSizes         uint32
	Rva 						[16]DataDirectory;
}

type OptionalHeader64 struct {
	Magic                       uint16
	MajorLinkerVersion          uint8
	MinorLinkerVersion          uint8
	SizeOfCode                  uint32
	SizeOfInitializedData       uint32
	SizeOfUninitializedData     uint32
	AddressOfEntryPoint         uint32
	BaseOfCode                  uint32
	ImageBase                   uint64
	SectionAlignment            uint32
	FileAlignment               uint32
	MajorOperatingSystemVersion uint16
	MinorOperatingSystemVersion uint16
	MajorImageVersion           uint16
	MinorImageVersion           uint16
	MajorSubsystemVersion       uint16
	MinorSubsystemVersion       uint16
	Win32VersionValue           uint32
	SizeOfImage                 uint32
	SizeOfHeaders               uint32
	CheckSum                    uint32
	Subsystem                   uint16
	DllCharacteristics          uint16
	SizeOfStackReserve          uint64
	SizeOfStackCommit           uint64
	SizeOfHeapReserve           uint64
	SizeOfHeapCommit            uint64
	LoaderFlags                 uint32
	NumberOfRvaAndSizes         uint32 // Always 16?
	Rvas 						[16]DataDirectory;
	
	// Directories are:
	// - ExportTable;
	// - ImportTable;
	// - ResourceTable;
	// - ExceptionTable;
	// - CertificateTable;
	// - BaseRelocationTable;
	// - Debug;
	// - Architecture;
	// - GlobalPtr;
	// - TLSTable;
	// - LoadConfigTable;
	// - BoundImport;
	// - IAT;
	// - DelayImportDescriptor;
	// - CLRRuntimeHeader;
	// - Reserved;
}

func NewOptionalHeader64() OptionalHeader64 {
	rvas := [16]DataDirectory{}
	// We don't import, export, etc so set all to empty
	for i, _ := range rvas {
		rvas[i] = DataDirectory{}
	}

	// https://msdn.microsoft.com/en-us/library/windows/desktop/ms680339(v=vs.85).aspx
	return OptionalHeader64{
		Magic : 0x20B,             // 64-bit application
		ImageBase: 0x400000,       // Default value
		MajorSubsystemVersion: 4,  // NT 4 or later
		Subsystem: 3,              // CUI (Console application)
		NumberOfRvaAndSizes: 16,   // Always 16
		Rvas : rvas,
	}
}

type DataDirectory struct {
	VirtualAddress uint32;
	Size 		   uint32;
}

type SectionHeader struct {
	Name [8]byte;
	VirtualSize uint32;
	VirtualAddress uint32;
	SizeOfRawData uint32;
	PointerToRawData uint32;
	PointerToRelocations uint32;
	PointerToLineNumbers uint32;
	NumberOfRelocations uint16;
	NumberOfLineNumbers uint16;
	Characteristics uint32;
}
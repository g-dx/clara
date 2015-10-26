package pe
import (
	"encoding/binary"
	"bytes"
)

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

// https://en.wikibooks.org/wiki/X86_Disassembly/Windows_Executable_Files#Imports_and_Exports_-_Linking_to_other_modules
type ImportDescriptor struct {
	OriginalFirstThunk uint32; // RVA to start of array of RVAs to ImportByName structures
	TimeDateStamp uint32;
	ForwarderChain uint32;
	Name uint32;               // RVA to null-terminated string of DLL name
	FirstThunk uint32;         // RVA to start of array of RVAs to ImportByName structures
}

type ImportByName struct {
	Hint uint16;
	Name [64]byte; // NOTE: We set a fixed 64 byte size in the hope no name goes over this!
}

func NewImportByName(name string) ImportByName {
	i := ImportByName{}
	copy(i.Name[:], name + "\x00")
	return i
}

// Struct designed to simplify RVA calculations for imported DLL functions

type Imports struct {
	rva uint32
	Descriptors []ImportDescriptor
	Thunks [][]uint64
	FnNames [][]ImportByName
	ModuleNames []string
}

func NewImports(rva uint32) Imports {
	return Imports{ rva : rva }
}

func (im * Imports) Rva(fn string) uint32 {

	// Panic if function name not found
	return 0; // TODO: Fill me in!
}

func (im * Imports) Module(name string, fns ... string) {
	im.Descriptors = append(im.Descriptors, ImportDescriptor{})
	im.ModuleNames = append(im.ModuleNames, name + "\x00")

	var fnNames []ImportByName
	for _, fn := range fns {
		fnNames = append(fnNames, NewImportByName(fn))
	}
	im.FnNames = append(im.FnNames, fnNames)
	im.Thunks = append(im.Thunks, make([]uint64, len(fns) + 1))
}

func (im * Imports) ToBuffer() *bytes.Buffer {

	// Calculate starting offsets
	originalThunkRva := im.rva + (uint32(binary.Size(ImportDescriptor{}) * (len(im.Descriptors) + 1)))
	firstThunkRva := originalThunkRva + im.thunksSize()
	importByNameRva := firstThunkRva + im.thunksSize()
	importNameRva := importByNameRva + im.fnNamesSize()

	for i := 0; i < len(im.Descriptors); i++ {

		// Populate descriptor
		desc := &im.Descriptors[i]
		desc.OriginalFirstThunk = originalThunkRva
		desc.Name = importNameRva
		desc.FirstThunk = firstThunkRva

		// Populate thunk
		thunk := im.Thunks[i]
		for j := 0; j < len(thunk)-1; j++ {
			thunk[j] = uint64(importByNameRva)
			importByNameRva += uint32(binary.Size(ImportByName{}))
		}

		// Increment offsets
		originalThunkRva += uint32(binary.Size(im.Thunks[i]))
		firstThunkRva += uint32(binary.Size(im.Thunks[i]))
		importNameRva += uint32(binary.Size([]byte(im.ModuleNames[i])))
	}

	// Write
	var buf bytes.Buffer
	binary.Write(&buf, binary.LittleEndian, im.Descriptors)
	binary.Write(&buf, binary.LittleEndian, ImportDescriptor{}) // Terminating descriptor

	// First thunk
	for _, thunk := range im.Thunks {
		binary.Write(&buf, binary.LittleEndian, thunk)
	}

	// Original first thunk
	for _, thunk := range im.Thunks {
		binary.Write(&buf, binary.LittleEndian, thunk)
	}

	// Func names
	for _, fnName := range im.FnNames {
		binary.Write(&buf, binary.LittleEndian, fnName)
	}

	// Module names
	for _, module := range im.ModuleNames {
		binary.Write(&buf, binary.LittleEndian, []byte(module))
	}
	return &buf
}

func (im * Imports) thunksSize() uint32 {
	n := 0
	for _, thunk := range im.Thunks {
		n += binary.Size(thunk)
	}
	return uint32(n)
}

func (im * Imports) fnNamesSize() uint32 {
	n := 0
	for _, fns := range im.FnNames {
		n += binary.Size(fns)
	}
	return uint32(n)
}
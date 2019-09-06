#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include "shared.h"

// ---------------------------------------------------------------------------------------------------------------------
// Heap Support

intptr_t blocks;

intptr_t getBlocks() { return blocks; }
void setBlocks(intptr_t b) { blocks = b; }
int isValidBlock(intptr_t b) { return b != 0; } // NUL check
intptr_t emptyBlock() { return 0; } // NUL block

intptr_t inc(intptr_t b, int i)
{
    return b + i;
}

// ---------------------------------------------------------------------------------------------------------------------
// Stack frame support

intptr_t getFramePointer(); // Implemented by codegen.go
intptr_t stackBase;         // Stack base set on entry

void setStackBase(intptr_t frame)
{
    stackBase = frame;
}
// ---------------------------------------------------------------------------------------------------------------------

int isStackBase(intptr_t frame)
{
    return frame == stackBase;
}

// ---------------------------------------------------------------------------------------------------------------------
// Debug support

int debugGc; // Defined in shared.h
void debug(char *logType, char *format, ...)
{
    // Handle varags
    va_list args;
    va_start (args, format);

    // GC debugging
    if (strcasecmp(logType, "gc") == 0 && debugGc) {
        vprintf(format, args);
        fflush(stdout); // Flush immediately
    }

    va_end(args);
}

// ---------------------------------------------------------------------------------------------------------------------
// lib/arrays.clara
// ---------------------------------------------------------------------------------------------------------------------

// Casting support
intptr_t toIntArray(intptr_t arrayHeader) { return arrayHeader; }
intptr_t toByteArray(intptr_t arrayHeader) { return arrayHeader; }
intptr_t toHeader(intptr_t pointer) { return pointer; }

// ---------------------------------------------------------------------------------------------------------------------
// lib/strings.clara
// ---------------------------------------------------------------------------------------------------------------------

// Casting support
// NOTE: codegen always increments the pointer by 8 when passing an array or string to an external function. This allows
// printf to work. As such reverse that when returning the same pointer!
intptr_t asString(intptr_t byteArray) { return byteArray - 8; }

// ---------------------------------------------------------------------------------------------------------------------
// lib/enums.clara
// ---------------------------------------------------------------------------------------------------------------------

// Casting support
intptr_t asEnum(intptr_t pointer) { return pointer; }

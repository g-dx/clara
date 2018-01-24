#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <execinfo.h>
#include <unistd.h>

// ---------------------------------------------------------------------------------------------------------------------
// Runtime functions
// ---------------------------------------------------------------------------------------------------------------------

void indexOutOfBounds()
{
    register int64_t i asm("rbx"); // NOTE: Keep in sync with codegen.go!
    printf("\n// -------------------------------------------------------------------------------------------\n");
    printf("// Crash: Index (%ld) is out of bounds!\n", i);
    printf("// -------------------------------------------------------------------------------------------\n");
    printf("\nBacktrace:\nTODO!\n\n");
    // TODO: Output stacktrace
    abort();
}

// ---------------------------------------------------------------------------------------------------------------------
// lib/arrays.clara
// ---------------------------------------------------------------------------------------------------------------------
uint64_t intArray(int size)
{
    uint64_t *array = (uint64_t *) malloc(sizeof(uint64_t) * (size + 1));
    array[0] = size;
    return (uint64_t) array;
}

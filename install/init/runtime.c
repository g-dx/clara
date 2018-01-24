#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <execinfo.h>
#include <unistd.h>

// ---------------------------------------------------------------------------------------------------------------------
// C Runtime functions
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
    int n = sizeof(uint64_t) * size;
    uint64_t *array = (uint64_t *) malloc(n + 8); // + 8 for length
    array[0] = size;
    return (uint64_t) array;
}

// ---------------------------------------------------------------------------------------------------------------------

uint64_t byteArray(int size)
{
    int n = (8 - (size % sizeof(uint8_t)) + size); // Round up to nearest multiple of 8
    uint64_t *array = (uint64_t *) malloc(n + 8);  // + 8 for length
    array[0] = size;
    return (uint64_t) array;
}

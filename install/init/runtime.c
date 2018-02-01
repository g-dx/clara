#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

// ---------------------------------------------------------------------------------------------------------------------
// C Runtime functions
// ---------------------------------------------------------------------------------------------------------------------

void indexOutOfBounds(int index)
{
    printf("\n// -------------------------------------------------------------------------------------------\n");
    printf("// Crash: Index (%d) is out of bounds!\n", index);
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

// ---------------------------------------------------------------------------------------------------------------------
// lib/strings.clara
// ---------------------------------------------------------------------------------------------------------------------

intptr_t toString(intptr_t bytes)
{
    // Copy bytes into new array
    uint64_t size = ((uint64_t *) bytes)[0] + 8; // + 8 for length
    intptr_t *s = malloc(size + 1); // + 1 for NUL byte
    memcpy(s, (void *) bytes, size);
    return (intptr_t) s;
}

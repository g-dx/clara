#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

// ---------------------------------------------------------------------------------------------------------------------
// lib/arrays.clara
// ---------------------------------------------------------------------------------------------------------------------
uint64_t intArray(int size)
{
    uint64_t *array = (uint64_t *) malloc(sizeof(uint64_t) * (size + 1));
    array[0] = size;
    return (uint64_t) array;
}

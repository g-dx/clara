#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include "shared.h"

// ---------------------------------------------------------------------------------------------------------------------
// Heap Support

intptr_t blocks;

intptr_t getBlocks() { return blocks; }
void setBlocks(intptr_t b) { blocks = b; }

// ---------------------------------------------------------------------------------------------------------------------
// Stack frame support

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
// Runtime support

intptr_t runtime;         // Set on program start
void setRuntime(intptr_t r) { runtime = r; }
intptr_t getRuntime() { return runtime; }

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
// Error support

int errnum() { return errno; }

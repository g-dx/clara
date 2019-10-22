#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include "shared.h"

// ---------------------------------------------------------------------------------------------------------------------
// Clara program entry point
// ---------------------------------------------------------------------------------------------------------------------

int clara_asm_entrypoint(int argc, char** argv);

static struct option options[] =
{
    { "gc.debug", no_argument, 0, 'g' },
    { 0, 0, 0, 0 },
};

int main(int argc, char** argv)
{
    while (1)
    {
        // Parse next option & check for end of input
        int index = 0;
        int c = getopt_long(argc, argv, "g", options, &index);
        if (c == -1)
        {
            break;
        }

        // Process option
        switch (c)
        {
        // Enable GC logging
        case 'g':
            debugGc = 1;
            break;
        }
    }

    // Program entry point
    clara_asm_entrypoint(argc, argv);
    return 0;
}
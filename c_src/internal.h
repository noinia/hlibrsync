#include <stdio.h>
#include <librsync.h>


typedef struct inMemoryBuffer_t {
    char *buffer;
    size_t size;
    size_t inUse;
} inMemoryBuffer_t;

size_t DEFAULT_BUFFERSIZE = 16;

// we will produce the output in output. If output->buffer = NULL
// we will allocate it ourselves using DEFAULT_SIZE.
// Note: DONT keep pointers directly to output->buffer, because they
// may become invalid !!!!
rs_result genSig(char *filePath, inMemoryBuffer_t *output);


// generate a delta, based on the implementation of rdiff
rs_result genDelta(int sigFd, char* filePath, int deltaFd);

rs_result applyPatch(int deltaFd, char* inputPath, char* outputPath);

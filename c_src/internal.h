#include <stdio.h>
#include <librsync.h>

#include "buf.h"


// typedef rs_filebuf rs_filebuf_t;

typedef struct inMemoryBuffer_t {
    char *buffer;
    size_t size;
    size_t inUse;
} inMemoryBuffer_t;

typedef struct rsyncSourceState_t {
    FILE *f;
    rs_job_t *job;
    rs_buffers_t *buf;
    rs_filebuf_t *inBuf;
    inMemoryBuffer_t *outputBuf;
} rsyncSourceState_t;


size_t DEFAULT_BUFFERSIZE = 8;

/* // we will produce the output in output. If output->buffer = NULL */
/* // we will allocate it ourselves using DEFAULT_SIZE. */
/* // Note: DONT keep pointers directly to output->buffer, because they */
/* // may become invalid !!!! */
/* rs_result genSig(char *filePath, inMemoryBuffer_t *output); */


/* // generate a delta, based on the implementation of rdiff */
/* rs_result genDelta(int sigFd, char* filePath, int deltaFd); */

/* rs_result applyPatch(int deltaFd, char* inputPath, char* outputPath); */

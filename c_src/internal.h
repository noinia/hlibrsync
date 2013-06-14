#include <stdio.h>
#include <stdbool.h>
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
    rs_result status;
} rsyncSourceState_t;


size_t DEFAULT_BUFFERSIZE = 8;


////////////////////////////////////////////////////////////////////////////////
// Computing Signatures


/**
 * Initialize everything to compute a signature.
 */
void initSignature(char *filePath, rsyncSourceState_t *state);

/**
 * Continue computing a signature. This assumes state is all set up to compute
 * the next part of the signature.
 *
 * if resetBuf == True, this function will reset the the next_out pointers such that
 * we start writing to the beginning of the output buffer again.
 */
void signatureChunk(rsyncSourceState_t *state, int resetBuf);

/**
 * Handles cleaning up everything after computing a signature.
 */
void finalizeSignature(rsyncSourceState_t *state);





/* // generate a delta, based on the implementation of rdiff */
/* rs_result genDelta(int sigFd, char* filePath, int deltaFd); */

/* rs_result applyPatch(int deltaFd, char* inputPath, char* outputPath); */

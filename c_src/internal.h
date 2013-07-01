#ifndef FILE_INTERNAL_SEEN
#define FILE_INTERNAL_SEEN

#include <stdio.h>
#include <stdbool.h>
#include "librsync.h"

#include "buf.h"




// typedef rs_filebuf rs_filebuf_t;

typedef struct inMemoryBuffer_t {
    char *buffer;
    size_t size;
    size_t inUse;
} inMemoryBuffer_t;

typedef struct rsyncSignatureState_t {
    FILE *f;
    rs_job_t *job;
    rs_buffers_t *buf;
    rs_filebuf_t *inBuf;
    inMemoryBuffer_t *outputBuf;
    rs_result status;
} rsyncSignatureState_t;

typedef struct rsyncPatchState_t {
    FILE *inF;
    FILE *outF;
    rs_job_t *job;
    rs_buffers_t *buf;
    inMemoryBuffer_t *deltaBuf;
    int deltaEOF;
    rs_filebuf_t *outputBuf;
    rs_result status;
} rsyncPatchState_t;




/******************************************************************************
 *                        Computing Signatures
 *****************************************************************************/

/**
 * Initialize everything to compute a signature.
 */
void initSignature(char *filePath, rsyncSignatureState_t *state);

/**
 * Continue computing a signature. This assumes state is all set up to compute
 * the next part of the signature.
 *
 * if resetBuf == True, this function will reset the the next_out pointers such that
 * we start writing to the beginning of the output buffer again.
 */
void signatureChunk(rsyncSignatureState_t *state, int resetBuf);

/**
 * Handles cleaning up everything after computing a signature.
 */
void finalizeSignature(rsyncSignatureState_t *state);


/******************************************************************************
 *                        Computing Deltas
 *****************************************************************************/



/******************************************************************************
 *                        Applying Patches
 *****************************************************************************/

/**
 * Initialize everything to apply a patch.
 */
void initPatch(char *inFilePath, char* outFilePath, rsyncPatchState_t *state);

/**
 * Continue patching the file. This assumes state is all set up to apply the next
 * chunk of the delta to the file.
 *
 */
void patchChunk(rsyncPatchState_t *state);

/**
 * Handles cleaning up everything after computing a signature.
 */
void finalizePatch(rsyncPatchState_t *state);




/* // generate a delta, based on the implementation of rdiff */
/* rs_result genDelta(int sigFd, char* filePath, int deltaFd); */

/* rs_result applyPatch(int deltaFd, char* inputPath, char* outputPath); */

#endif

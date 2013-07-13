#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

#include "internal.h"

size_t RS_DEFAULT_BUFFERSIZE = 8;

/******************************************************************************
 *                        Debugging functions
 *****************************************************************************/

void toFile(char* path, char *str, size_t len) {
    FILE *f = fopen(path,"wb");
    if (!f)
        return;
    fwrite(str,1,len,f);
    fclose(f);
}

void printSig(char *str, size_t sigLength) {
    char *p = str;
    int n;
    for (n = 0; n < sigLength; ++n) {
        printf("%2.2x ", *p);
        ++p;
    }
    printf ("\n");
}

void printOut(inMemoryBuffer_t *output) {
    printf ("Buffer Length: %zu\n",output->size);
    printf ("Sig Length: %zu\n",output->inUse);
    printf ("Current Value\n");
    printSig(output->buffer,output->inUse);
    printf ("Buffer:\n");
    printSig(output->buffer,output->size);
    printf ("--------------------\n");
}

/******************************************************************************
 *                        Computing Signatures
 *****************************************************************************/

rs_result signatureCb(rs_job_t *job, rs_buffers_t *buf, void *opaque) {
    rsyncSignatureState_t *state = (rsyncSignatureState_t*) opaque;

    assert(state != NULL);

    inMemoryBuffer_t *output = state->outputBuf;

    assert(output != NULL);
    assert(buf->next_out != NULL);


    /* printf("sigCb called. Summary: "); */
    /* printf("output->inuse: %zu, output->size: %zu, buf->avail_out: %zu", output->inUse, output->size, buf->avail_out); */

    output->inUse = output->size - buf->avail_out;

    /* printf(", *NEW* inUse: %zu\n", output->inUse); */
    /* printOut(output); */
    /* printf("REMAINING IN: %zu\n",buf->avail_in); */


    if (buf->avail_out == 0) {
        /* printf("\nbuf full. Blocking\n"); */
        return RS_BLOCKED;
    }

    return RS_DONE;
}


void initSignature(char *filePath, rsyncSignatureState_t *state) {

    if (state->outputBuf == NULL) {
        state->outputBuf = malloc(sizeof(inMemoryBuffer_t));
        state->outputBuf->buffer = malloc(RS_DEFAULT_BUFFERSIZE);
        state->outputBuf->size = RS_DEFAULT_BUFFERSIZE;
        state->outputBuf->inUse = 0;
    }

    inMemoryBuffer_t *output = state->outputBuf;

    state->buf = malloc(sizeof(rs_buffers_t));

    // Set up so the job uses output->buffer as buffer
    state->buf->next_out = state->outputBuf->buffer;
    state->buf->avail_out = state->outputBuf->size;

    state->f = fopen(filePath, "rb");
    if (!state->f) {
        state->status = RS_IO_ERROR;
        return;
    }

    state->inBuf  = rs_filebuf_new(state->f, rs_inbuflen);

    state->job = rs_sig_begin(RS_DEFAULT_BLOCK_LEN, RS_DEFAULT_STRONG_LEN);

    state->status = RS_BLOCKED;

}

void finalizeSignature(rsyncSignatureState_t *state) {
    fclose(state->f);
    rs_job_free(state->job);
    free(state->buf);
    if (state->inBuf)
        rs_filebuf_free(state->inBuf);
    free(state->outputBuf->buffer);
    free(state->outputBuf);
}


/* Get the next chunk of the signature.
 */
void signatureChunk(rsyncSignatureState_t *state, int resetBuf) {
    // set up the output buffers
    assert(state != NULL);
    assert(state->job != NULL);
    assert(state->buf != NULL);
    assert(state->inBuf != NULL);
    assert(state->outputBuf != NULL);
    assert(state->outputBuf->buffer != NULL);

    /* printf("SigChunk. eof_in: %d avail_in: %zu\n, avail_out: %zu\n", state->buf->eof_in, state->buf->avail_in, state->buf->avail_out); */


    if (resetBuf) {
        // reset the input buffer if needed as well
        if (state->buf->avail_in == 0) {
            state->buf->next_in = NULL;
            state->buf->eof_in = 0; // ASSUME there is input left
        }

        state->outputBuf->inUse = 0;
        state->buf->avail_out = state->outputBuf->size;
        state->buf->next_out = state->outputBuf->buffer;
        /* printf("resetted buf\n"); */
    }

    state->status = rs_job_drive_as_is(state->job, state->buf,
                                       state->inBuf ? rs_infilebuf_fill : NULL,
                                       state->inBuf,
                                       signatureCb,
                                       state
                                       );

}


/******************************************************************************
 *                        Computing Deltas
 *****************************************************************************/

void initDelta(char *inFilePath, rsyncDeltaState_t *state) {
    //TODO
}

/**
 * Continue computing the delta of the file. This assumes that the state is all
 * set up to get the next chunk of the signature.
 *
 */
void deltaChunk(rsyncDeltaState_t *state) {
    //TODO
}

/**
 * Handles cleaning up everything after computing a delta
 */
void finalizeDelta(rsyncDeltaState_t *state) {
    //TODO
}


/******************************************************************************
 *                        Applying Patches
 *****************************************************************************/

void initPatch(char *inFilePath, char* outFilePath, rsyncPatchState_t *state) {

    state->buf = malloc(sizeof(rs_buffers_t));

    state->inF = fopen(inFilePath, "rb");
    state->outF = fopen(outFilePath, "wb");

    if ( !state->inF || !state->outF) {
        state->status = RS_IO_ERROR;
        return;
    }


    state->outputBuf = rs_filebuf_new(state->outF, rs_outbuflen);

    state->job = rs_patch_begin(rs_file_copy_cb, state->inF);

    state->status = RS_BLOCKED;

    printf ("Init done :D\n");

}

rs_result patchCb(rs_job_t *job, rs_buffers_t *buf, void *opaque) {
    rsyncPatchState_t *state = (rsyncPatchState_t*) opaque;

    assert(state != NULL);
    assert(state->buf != NULL);

    // we already set up state->buf->next_in and state->buf->avail_in here
    // so we only need to make sure we stop if there is no remaining input

    if (state->buf->avail_in == 0)
        return RS_BLOCKED;
    else
        return RS_DONE;
}

void patchChunk(rsyncPatchState_t *state) {

    state->buf->next_in  = state->deltaBuf->buffer;
    state->buf->avail_in = state->deltaBuf->inUse;
    state->buf->eof_in   = state->deltaEOF;

    printf ("getting chunk\n");
    printf ("eof: %d\n", state->buf->eof_in);
    printf ("%zu\n",state->buf->avail_in);

    state->status = rs_job_drive_as_is(state->job, state->buf,
                                       patchCb, state,
                                       rs_outfilebuf_drain, state->outputBuf);

    printf ("Done getting chunk\n");

}

void finalizePatch(rsyncPatchState_t *state) {

    rs_job_free(state->job);

    fclose(state->outF);
    fclose(state->inF);

    if (state->outputBuf)
        rs_filebuf_free(state->outputBuf);


    free(state->buf);
}

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <librsync.h>
#include <assert.h>

#include "internal.h"
//#include "buf.h"

// -----------------------------------------------------------------------------
// Debugging functions

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

// -----------------------------------------------------------------------------
// The acutal functiosn

rs_result signatureCb(rs_job_t *job, rs_buffers_t *buf, void *opaque) {
    rsyncSourceState_t *state = (rsyncSourceState_t*) opaque;

    assert(state != NULL);

    inMemoryBuffer_t *output = state->outputBuf;

    assert(output != NULL);

    // initialize the buf
    if (buf->next_out == NULL) {
        assert(buf->avail_out == 0);

        // allow us to directly write into the output stream
        buf->next_out = output->buffer;
        buf->avail_out = output->size;
        output->inUse = 0;

        return RS_DONE;
    }


    output->inUse = output->size - buf->avail_out;

    if (buf->avail_out == 0) {
        return RS_BLOCKED;
    }


    return RS_DONE;
}


rs_result startSignature(char *filePath, rsyncSourceState_t *state) {

    rs_buffers_t buf;

    if (state->outputBuf == NULL) {
        state->outputBuf = malloc(sizeof(inMemoryBuffer_t));
        state->outputBuf->buffer = malloc(DEFAULT_BUFFERSIZE);
        state->outputBuf->size = DEFAULT_BUFFERSIZE;
        state->outputBuf->inUse = 0;
    }

    inMemoryBuffer_t *output = state->outputBuf;

    /* printf("setting up buf\n"); */

    /* // Set up so the job uses output->buffer as buffer */
    /* buf->next_out = output->buffer; */
    /* buf->avail_out = output->size; */

    state->f = fopen(filePath, "rb");
    if (!state->f)
        return RS_IO_ERROR;

    state->inBuf  = rs_filebuf_new(state->f, rs_inbuflen);

    state->job = rs_sig_begin(RS_DEFAULT_BLOCK_LEN, RS_DEFAULT_STRONG_LEN);

    return  rs_job_drive(state->job, &buf,
                         state->inBuf  ? rs_infilebuf_fill : NULL, state->inBuf,
                         signatureCb, state
                         );
}

void endSignature(rsyncSourceState_t *state) {


    printf ("Generated the signature:\n");
    printOut(state->outputBuf);
    printf("DONE printing\n\n\n");

    fclose(state->f);
    rs_job_free(state->job);
    if (state->inBuf)
        rs_filebuf_free(state->inBuf);
    free(state->outputBuf->buffer);
    free(state->outputBuf);
}

rs_result signatureChunk(rsyncSourceState_t *state) {
    // set up the output buffers
    assert(state != NULL);
    assert(state->job != NULL);
    assert(state->inBuf != NULL);
    assert(state->outputBuf != NULL);
    assert(state->outputBuf->buffer != NULL);

    rs_buffers_t buf;

    return rs_job_drive(state->job, &buf,
                         state->inBuf  ? rs_infilebuf_fill : NULL, state->inBuf,
                         signatureCb, state
                         );
}




// -----------------------------------------------------------------------------
// For testing purposes

int main(int argc, char *argv[]) {

    int i;
    rs_result result;

    rsyncSourceState_t *state = malloc(sizeof(rsyncSourceState_t));

    printf ("Starting up \n");
    result= startSignature("/Users/frank/tmp/httpd-error.log", state);

    printf ("Started :)\n");
    printf("initial data:\n");
    printOut(state->outputBuf);


    for (i = 0; i < 6; ++i) {
        printf ("Getting a chunk");
        assert(state != NULL);
        assert(state->job != NULL);
        assert(state->inBuf != NULL);
        assert(state->outputBuf != NULL);
        assert(state->outputBuf->buffer != NULL);
        printf("..\n");
        result = signatureChunk(state);
        printOut(state->outputBuf);
    }

    endSignature(state);
    free(state);

    return 0;
}

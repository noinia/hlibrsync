#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
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
// The actual functions

rs_result signatureCb(rs_job_t *job, rs_buffers_t *buf, void *opaque) {
    rsyncSourceState_t *state = (rsyncSourceState_t*) opaque;

    assert(state != NULL);

    inMemoryBuffer_t *output = state->outputBuf;

    assert(output != NULL);
    assert(buf->next_out != NULL);

    /* initialize the buf */
    /* if (buf->next_out == NULL) { */
    /*     assert(buf->avail_out == 0); */

    /*     printf("reset buf\n"); */

    /*     // allow us to directly write into the output stream */
    /*     buf->next_out = output->buffer; */
    /*     buf->avail_out = output->size; */
    /*     output->inUse = 0; */

    /*     return RS_DONE; */
    /* } */

    printf("sigCb called. Summary: ");
    printf("output->inuse: %zu, output->size: %zu, buf->avail_out: %zu", output->inUse, output->size, buf->avail_out);

    output->inUse = output->size - buf->avail_out;
    printf(", *NEW* inUse: %zu\n", output->inUse);

    printOut(output);

    printf("REMAINING IN: %zu\n",buf->avail_in);


    if (buf->avail_out == 0) {
        printf("\nbuf full. Blocking\n");
        return RS_BLOCKED;
    }

    /* printf("continguing, with available %zu\n",buf->avail_out); */

    return RS_DONE;
}


rs_result startSignature(char *filePath, rsyncSourceState_t *state) {

    if (state->outputBuf == NULL) {
        state->outputBuf = malloc(sizeof(inMemoryBuffer_t));
        state->outputBuf->buffer = malloc(DEFAULT_BUFFERSIZE);
        state->outputBuf->size = DEFAULT_BUFFERSIZE;
        state->outputBuf->inUse = 0;
    }

    inMemoryBuffer_t *output = state->outputBuf;

    state->buf = malloc(sizeof(rs_buffers_t));

    // Set up so the job uses output->buffer as buffer
    state->buf->next_out = state->outputBuf->buffer;
    state->buf->avail_out = state->outputBuf->size;

    state->f = fopen(filePath, "rb");
    if (!state->f)
        return RS_IO_ERROR;

    state->inBuf  = rs_filebuf_new(state->f, rs_inbuflen);

    state->job = rs_sig_begin(RS_DEFAULT_BLOCK_LEN, RS_DEFAULT_STRONG_LEN);

    rs_result  res;

    res =  rs_job_drive_as_is(state->job, state->buf,
                         state->inBuf  ? rs_infilebuf_fill : NULL, state->inBuf,
                         signatureCb, state
                         );

    printf("remaining in after getting a chunk: %zu\n", state->buf->avail_in);

    return res;

}

void endSignature(rsyncSourceState_t *state) {


    printf ("Generated the signature:\n");
    printOut(state->outputBuf);
    printf("DONE printing\n\n\n");

    fclose(state->f);
    rs_job_free(state->job);
    free(state->buf);
    if (state->inBuf)
        rs_filebuf_free(state->inBuf);
    free(state->outputBuf->buffer);
    free(state->outputBuf);
}

rs_result signatureChunk(rsyncSourceState_t *state, bool resetBuf) {
    // set up the output buffers
    assert(state != NULL);
    assert(state->job != NULL);
    assert(state->buf != NULL);
    assert(state->inBuf != NULL);
    assert(state->outputBuf != NULL);
    assert(state->outputBuf->buffer != NULL);



    printf("SigChunk. eof_in: %d avail_in: %zu\n, avail_out: %zu\n", state->buf->eof_in, state->buf->avail_in, state->buf->avail_out);


    printf("INAVAIL: %zu\n",state->buf->avail_in);
    printf("INEOF: %d\n",state->buf->eof_in );



    if (resetBuf) {
        // reset the input buffer if needed as well
        if (state->buf->avail_in == 0) {
            state->buf->next_in = NULL;
            state->buf->eof_in = 0; // ASSUME there is input left
        }

        state->outputBuf->inUse = 0;
        state->buf->avail_out = state->outputBuf->size;
        state->buf->next_out = state->outputBuf->buffer;
        /* // zero the input buf */
        /* state->buf->next_in = NULL; */
        /* state->buf->avail_in = 0; */
        printf("resetted buf\n");
    }

    rs_result res;

    res = rs_job_drive_as_is(state->job, state->buf,
                         state->inBuf  ? rs_infilebuf_fill : NULL, state->inBuf,
                         signatureCb, state
                         );

    printf("INEOF after: %d\n",state->buf->eof_in );
    printf("remaining in after getting a chunk: %zu\n", state->buf->avail_in);

    return res;
}




// -----------------------------------------------------------------------------
// For testing purposes

int main(int argc, char *argv[]) {

    int i;
    rs_result result;

    rsyncSourceState_t *state = malloc(sizeof(rsyncSourceState_t));

    printf ("Starting up \n");
    result= startSignature("/Users/frank/tmp/httpd-error.log", state);

    printf("Chunk 1\n=====================\n===============\n");
    printOut(state->outputBuf);

    for (i = 2; i < 6; ++i) {
        assert(state != NULL);
        assert(state->job != NULL);
        assert(state->inBuf != NULL);
        assert(state->outputBuf != NULL);
        assert(state->outputBuf->buffer != NULL);
        printf ("Getting chunk %d\n==================\n==================\n",i);
        result = signatureChunk(state,true);
        printf("%s\n",rs_strerror(result));
        printOut(state->outputBuf);
    }

    endSignature(state);
    free(state);

    return 0;
}

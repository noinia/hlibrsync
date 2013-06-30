/* @(#)test.c
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

#include "internal.h"
#include "librsync.h"


void testSignature() {

    int i = 1;

    rsyncSourceState_t *state = malloc(sizeof(rsyncSourceState_t));

    printf ("Starting up \n");
    initSignature("/Users/frank/tmp/httpd-error.log", state);

    /* printf("Chunk 1\n=====================\n===============\n"); */
    /* printOut(state->outputBuf); */

    /* i = 2; */

    while (state->status != RS_DONE) {
        assert(state != NULL);
        assert(state->job != NULL);
        assert(state->inBuf != NULL);
        assert(state->outputBuf != NULL);
        assert(state->outputBuf->buffer != NULL);

        printf ("Getting chunk %d\n==================\n==================\n",i);
        signatureChunk(state, 1/*true*/);
        /* printf("%s\n",rs_strerror(result)); */
        printOut(state->outputBuf);
        i++;
    }

    finalizeSignature(state);
    free(state);

}

void testPatch() {
    int i = 1;

    rsyncSinkState_t *state = malloc(sizeof(rsyncSinkState_t));

    initPatch("/Users/frank/tmp/httpd-error_editted.log",
              "/tmp/httpd-error_patched.log", state);

    FILE* deltaF = fopen("/Users/frank/tmp/httpd-error_delta","rb");

    while (state->status != RS_DONE) {
        assert(state != NULL);
        assert(state->job != NULL);
        assert(state->deltaBuf != NULL);
        assert(state->deltaBuf->buffer != NULL);
        assert(state->outputBuf != NULL);


        printf ("Getting Chunk %d\n===============\n==============\n",i);

        if (!feof(deltaF)) {
            printf ("Reading data\n");
            state->deltaBuf->inUse = fread(state->deltaBuf->buffer,
                                           1,
                                           state->deltaBuf->size,
                                           deltaF);
        }

        printf ("Patching\n");
        patchChunk(state);

        i++;
    }

    fclose(deltaF);

    finalizePatch(state);

    free(state);
}




int main(int argc, char *argv[]) {

    testPatch();

    return 0;
}

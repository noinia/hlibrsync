/* @(#)test.c
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

#include "internal.h"
#include "librsync.h"


int main(int argc, char *argv[]) {

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

    return 0;
}

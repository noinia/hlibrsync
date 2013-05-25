#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <librsync.h>
#include <assert.h>

#include "internal.h"
#include "buf.h"

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

/*
 * Function to write the signature into a `bytestream': an array of bytes.
 * When we call this function we expect to get:
 *   - a pointer to a job : job,
 *   - a pointer to an rs_buffers struct: buf, and
 *   - a pointer to a inMemoryBuffer_t struct: opaque, the buffer+meta
 *              info about the buffer  where to write to
 *
 * Note that opaque is a void pointer to satisfy the type signature of the callback.
 *
 * We will set up buf to *directly* write the output stream, i.e. sigBuf->sig.
 * this means this function has two tasks: (1) initialize this properly, and (2)
 * if we run low on space in sigBuf->sig, allocate more space.
 *
 */
rs_result outputSignature(rs_job_t *job, rs_buffers_t *buf, void *opaque) {
    char *newBuffer;
    inMemoryBuffer_t *output = (inMemoryBuffer_t*) opaque;

    /* This is only allowed if either the buf has no output buffer
     * yet, or that buffer could possibly be BUF. */
    if (buf->next_out == NULL) {
        assert(buf->avail_out == 0);

        // allow us to directly write into the output stream
        buf->next_out = output->buffer;
        buf->avail_out = output->size;

        return RS_DONE;
    }

    // If we no longer have space left, double the storage space.
    if (buf->avail_out == 0) {

        printf("ran out of space");
        printOut(output);

        newBuffer = (char*) malloc(2*(output->size));
        memcpy(newBuffer,output->buffer, output->size);

        free(output->buffer);

        output->buffer = newBuffer;
        buf->avail_out = output->size;
        buf->next_out  = output->buffer + output->size;

        output->size *= 2;

        printf("output after enlarging buffer\n");
        printOut(output);


        return RS_DONE;
    }

    assert(buf->avail_out > 0);

    // that is it

    return RS_DONE;
}


rs_result genSig(char *filePath, inMemoryBuffer_t *output) {

    FILE *f;
    rs_job_t *job;
    rs_buffers_t buf;
    rs_filebuf_t *inBuf = NULL;
    rs_result result;

    if (output->buffer == NULL) {
        output->buffer = malloc(DEFAULT_BUFFERSIZE);
        output->size = DEFAULT_BUFFERSIZE;
    }

    output->inUse = 0;

    f = fopen(filePath, "rb");
    if (!f)
        return RS_IO_ERROR;

    inBuf  = rs_filebuf_new(f, rs_inbuflen);

    job = rs_sig_begin(RS_DEFAULT_BLOCK_LEN, RS_DEFAULT_STRONG_LEN);

    result = rs_job_drive(job, &buf,
                          inBuf  ? rs_infilebuf_fill : NULL, inBuf,
                          outputSignature, output
                          );

    if (inBuf)
        rs_filebuf_free(inBuf);

    // compute the final size of the signature
    output->inUse = output->size - buf.avail_out;

    printf ("Generated the signature:\n");
    printOut(output);
    printf("DONE printing\n\n\n");

    rs_job_free(job);
    fclose(f);

    return result;
}


// generate a delta, based on the implementation of rdiff
rs_result genDelta(int sigFd, char* filePath, int deltaFd) {
    FILE*           sigFile;
    FILE*           f;
    FILE*           deltaFile;
    rs_result       result;
    rs_signature_t* sumset;

    sigFile   = fdopen(sigFd, "rb");
    f         = fopen(filePath, "rb");
    deltaFile = fdopen(deltaFd, "wb");

    result = rs_loadsig_file(sigFile, &sumset, NULL);
    if (result != RS_DONE)
       return result;

    if ((result = rs_build_hash_table(sumset)) != RS_DONE)
       return result;

    result = rs_delta_file(sumset, f, deltaFile, NULL);

    rs_free_sumset(sumset);

    fclose(deltaFile);
    fclose(f);
    fclose(sigFile);

    return result;
}


rs_result applyPatch(int deltaFd, char* inputPath, char* outputPath) {
    FILE*      deltaFile;
    FILE*      inputFile;
    FILE*      outputFile;
    rs_result  result;

    inputFile  = fopen(inputPath, "rb");
    deltaFile  = fdopen(deltaFd, "rb");
    outputFile = fopen(outputPath, "wb");

    result = rs_patch_file(inputFile, deltaFile, outputFile, NULL);

    fclose(inputFile);
    fclose(deltaFile);
    fclose(outputFile);

    return result;
}

// -----------------------------------------------------------------------------
// For testing purposes

int main(int argc, char *argv[]) {
    inMemoryBuffer_t *sig = (inMemoryBuffer_t*) malloc(sizeof(inMemoryBuffer_t));

    rs_result result = genSig("/Users/frank/tmp/httpd-error.log",sig);

    printOut(sig);
    toFile("/tmp/signature",sig->buffer,sig->inUse);

    free(sig);

    return 0;
}

#include "internal.h"
#include <librsync.h>
#include <stdio.h>

rs_result genSig(char* filePath, int sigFd) {
    FILE*      f;
    FILE*      sigFile;
    rs_result  result;

    f       = fopen(filePath, "rb");
    sigFile = fdopen(sigFd, "wb");

    result = rs_sig_file(f, sigFile, 0, 0, null);
    // use default lengths, don't gather sats
    fclose(f);
    fclose(sigFile);

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

    result = rs_loadsig_file(sigFile, &sumset, null);
    if (result != RS_DONE)
       return result;

    if ((result = rs_build_hash_table(sumset)) != RS_DONE)
       return result;

    result = rs_delta_file(sumset, f, deltaFile, null);

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

    result = rs_patch_file(inputFile, deltaFile, outputFile, null);

    fclose(inputFile);
    fclose(deltaFile);
    fclose(outputFile);

    return result;
}

#include "internal.h"
#include <librsync.h>
#include <stdio.h>

rs_result genSig(char* filePath, int sigFd) {
    FILE*      f;
    FILE*      sigFile;
    rs_stats_t stats;
    rs_result  result;

    f       = fopen(filePath, "rb");
    sigFile = fdopen(sigFd, "wb");

    result = rs_sig_file(f, sigFile,
                         RS_DEFAULT_BLOCK_LEN, RS_DEFAULT_STRONG_LEN, &stats);
    fclose(f);

    // Note that we leave the sigfile open

    return result;
}


// generate a delta, based on the implementation of rdiff
rs_result genDelta(int sigFd, char* filePath, int deltaFd) {
    FILE*           sigFile;
    FILE*           f;
    FILE*           deltaFile;
    rs_result       result;
    rs_signature_t* sumset;
    rs_stats_t      stats;

    sigFile   = fdopen(sigFd, "rb");
    f         = fopen(filePath, "rb");
    deltaFile = fdopen(deltaFd, "wb");

    result = rs_loadsig_file(sigFile, &sumset, &stats);
    if (result != RS_DONE)
       return result;

    if ((result = rs_build_hash_table(sumset)) != RS_DONE)
       return result;

    result = rs_delta_file(sumset, f, deltaFile, &stats);

    rs_free_sumset(sumset);

    // fclose(deltaFile);
    fclose(f);
    fclose(sigFile);

    return result;
}


rs_result applyPatch(int deltaFd, char* inputPath, char* outputPath) {
    FILE*      deltaFile;
    FILE*      inputFile;
    FILE*      outputFile;
    rs_stats_t stats;
    rs_result  result;

    inputFile  = fopen(inputPath, "rb");
    deltaFile  = fdopen(deltaFd, "rb");
    outputFile = fopen(outputPath, "wb");

    result = rs_patch_file(inputFile, deltaFile, outputFile, &stats);

    fclose(inputFile);
    // keep the delta file open
    fclose(outputFile);

    return result;
}

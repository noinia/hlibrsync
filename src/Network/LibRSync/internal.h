#include <stdio.h>
#include <librsync.h>

rs_result genSig(char* filePath, int sigFd);

// generate a delta, based on the implementation of rdiff
rs_result genDelta(int sigFd, char* filePath, int deltaFd);

rs_result applyPatch(int deltaFd, char* inputPath, char* outputPath);

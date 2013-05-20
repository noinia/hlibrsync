{-# LANGUAGE ForeignFunctionInterface #-}
module Network.LibRSync.Internal where

import System.IO
import GHC.IO.Handle
import System.Posix.IO
import System.Posix.Types

import Foreign
-- import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
-- import Foreign.Storable

#include <stdio.h>
#include "librsync.h"

--------------------------------------------------------------------------------
-- | The CTypes

-- data Target = Target { t :: CUShort , i :: CInt }
-- deriving (Show, Eq)

-- {#pointer *rs_target as TargetPtr -> Target #}

-- data BlockSig
-- {#pointer *rs_block_sig_t as BlockSigPtr -> BlockSig #}


type CSignature = Ptr CFile


-- | The Signature type
{#pointer *rs_signature_t as SignaturePtr -> CSignature #}

-- | The results type
{#enum rs_result as Result {underscoreToCase} deriving (Eq, Show)#}

--------------------------------------------------------------------------------
-- | Generating Signatures


-- We first implement a function to generate the signature which is easy to
-- call
#c
rs_result genSig(char* filePath, int fd) {
    FILE* f, sigFile;
    rs_stats_t stats;
    rs_result  result;

    f       = rs_file_open(filePath, "rb");
    sigFile = fdopen(fd, "wb");

    result = rs_sig_file(f, sigFile, &stats);
    rs_file_close(f);

    // Note that we leave the sigfile open

    return result;
}
#endc

-- cSignature      :: FilePath -> IO (Either String CSignature)
-- cSignature p = cGenSig p >>= \r -> return $ case r of
--                                               RsDone -> Right $ getSig ps
--                                               _      -> Left "some error"
--                    where
--                      getSig :: SignaturePtr -> CSignature
--                      getSig = undefined



cGenSig :: FilePath -> Handle -> IO (Handle, Result)
cGenSig p h = handleToFd h >>= cGenSig' p >>= \r -> return (h,r)

{#fun unsafe genSig as cGenSig'
      { `String'
      , fromIntegral `Fd'
      } -> `Result' cIntToEnum
 #}



-- -- | Loading signatures
-- -- the c-function is:
-- -- rs_result rs_loadsig_file(FILE *, rs_signature_t **, rs_stats_t *);
-- {#fun unsafe rs_loadsig_file as cRSLoadSigFile
--       { id      `Ptr CFile'
--       , alloca- `SignaturePtr' peek*
--       , alloca- `StatsPtr'
--       } -> `Result' cIntToEnum
--  #}

--------------------------------------------------------------------------------
-- | Delta

#c
// generate a delta, based on the implementation of rdiff
rs_result genDelta(int sigFd, char* filePath, int deltaFd) {
    FILE*           sigFile, f, deltaFile;
    rs_result       result;
    rs_signature_t* sumset;
    rs_stats_t      stats;

    sigFile   = fdopen(sigFd, "rb");
    f         = rs_open_file(filePath, "rb");
    deltaFile = fdopen(deltaFd, "wb");

    result = rs_loadsig_file(sigFile, &sumset, &stats);
    if (result != RS_DONE)
       return result;

    if ((result = rs_build_hash_table(sumset)) != RS_DONE)
       return result;

    result = rs_delta_file(sumset, f, deltaFile, &stats);

    rs_free_sumset(sumset);

    // rs_file_close(deltaFile);
    rs_file_close(f);
    // rs_file_close(sigFile);

    return result;
}
#endc



--------------------------------------------------------------------------------
-- | Patch


#c
rs_result applyPatch(int deltaFd, char* inputPath, char* outputPath) {
    FILE*      deltaFile, inputFile, outputFile;
    rs_stats_t stats;
    rs_result  result;

    inputFile  = rs_file_open(inputPath, "rb");
    deltaFile  = fdopen(deltaFd, "rb");
    outputFile = rs_file_open(outputPath, "wb");

    result = rs_patch_file(inputFile, deltaFile, outputFile, &stats);

    rs_file_close(inputFile);
    // keep the delta file open
    rs_file_close(outputFile);

    return result;
}
#endc

--------------------------------------------------------------------------------
-- | Helper functions

cIntToEnum :: Enum a => CInt -> a
cIntToEnum = toEnum . fromIntegral


test = do
  h <- openBinaryFile "/tmp/signature" WriteMode
  (h',r) <- cGenSig "/Users/frank/tmp/httpd-error.log" h
  print r
  hClose h'

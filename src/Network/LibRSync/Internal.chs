{-# LANGUAGE ForeignFunctionInterface #-}
module Network.LibRSync.Internal where

import Data.ByteString

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


-- type CSignature = Ptr CFile


-- -- | The Signature type
-- {#pointer *rs_signature_t as SignaturePtr -> CSignature #}

-- | The results type
{#enum rs_result as Result {underscoreToCase} deriving (Eq, Show)#}

--------------------------------------------------------------------------------
-- | Generating Signatures

type Signature = ByteString

signature   :: FilePath -> IO (Either String Signature)
signature p = undefined



-- | Given a file, and a handle h, opened in binary Read-Write mode, indicating
-- where to write the signature to. Generate the signature. The signature is
-- written to the file corresponding to handle h. The handle is moved to the
-- beginning of the file. The function returns a Maybe String, indicating if
-- something goes wrong. If the result is Nothing, the operation succeeded.
hSignature     :: FilePath -> Handle -> IO (Maybe String)
hSignature p h = copyHandleToFd h >>= cGenSig p >>= \r -> case r of
                    RsDone -> hSeek h AbsoluteSeek 0 >> return Nothing
                    _      -> return $ Just "some error"

{#fun unsafe genSig as cGenSig
      { `String'
      , fromIntegral `Fd'
      } -> `Result' cIntToEnum
 #}

-- We first implement a function to generate the signature which is easy to
-- call
#c
rs_result genSig(char* filePath, int fd) {
    FILE*      f;
    FILE*      sigFile;
    rs_stats_t stats;
    rs_result  result;

    f       = fopen(filePath, "rb");
    sigFile = fdopen(fd, "wb");

    result = rs_sig_file(f, sigFile,
                         RS_DEFAULT_BLOCK_LEN, RS_DEFAULT_STRONG_LEN, &stats);
    rs_file_close(f);

    // Note that we leave the sigfile open

    return result;
}
#endc

--------------------------------------------------------------------------------
-- | Delta

-- | Given a handle refering to the sginature, in (at least) binary Read mode,
-- positioned at the beginning of the file, a file path to a file, and a handle
-- in binary read write mode. Compute the delta, and write the result to the
-- file indicated by the second handle. The handle is repositioned to the
-- beginning of the file. The result of this function is a Maybe String
-- indicating any error messages. Nothing means the operation succeeded.
cGenDelta :: Handle -> FilePath -> Handle -> IO (Maybe String)
cGenDelta sigHandle p deltaHandle = undefined


{#fun unsafe genDelta as genDelta'
      { fromIntegral `Fd'
      , `String'
      , fromIntegral `Fd'
      } -> `Result' cIntToEnum

 #}

#c
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

    // rs_file_close(deltaFile);
    rs_file_close(f);
    rs_file_close(sigFile);

    return result;
}
#endc

--------------------------------------------------------------------------------
-- | Patch

{#fun unsafe applyPatch as applyPatch'
      { fromIntegral `Fd'
      , `String'
      , `String'
      } -> `Result' cIntToEnum
 #}


#c
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

copyHandleToFd h = hDuplicate h >>= handleToFd

test = do
  h <- openBinaryFile "/tmp/signature" ReadWriteMode
  r <- hSignature "/Users/frank/tmp/httpd-error.log" h
  print r
  hClose h

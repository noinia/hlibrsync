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
    sigFile = fdopen(fd, "w");

    result = rs_sig_file(f, sigFile, &stats);
    rs_file_close(f);

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
-- | Helper functions

cIntToEnum :: Enum a => CInt -> a
cIntToEnum = toEnum . fromIntegral


test = do
  h <- openBinaryFile "/tmp/signature" WriteMode
  (h',r) <- cGenSig "/Users/frank/tmp/httpd-error.log" h
  print r
  hClose h'

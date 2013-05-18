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

type CRSLong = CLLong

-- data Target = Target { t :: CUShort , i :: CInt }
--               deriving (Show, Eq)

-- {#pointer *rs_target as TargetPtr -> Target #}

-- data BlockSig
-- {#pointer *rs_block_sig_t as BlockSigPtr -> BlockSig #}


-- | The Signature type
data CSignature -- = CSignature { flength :: CRSLong
               --               , count   :: CInt
               --               , remainder :: CInt
               --               , blockLenth :: CInt
               --               , strongSumLenght :: CInt
               --               , blockSigs :: BlockSigPtr
               --               , targets   :: TargetPtr
               --               }
               -- deriving (Eq, Show)

{#pointer *rs_signature_t as SignaturePtr -> CSignature #}

-- | The Stats type
data Stats
{#pointer *rs_stats as StatsPtr -> Stats #}

-- | The results type
{#enum rs_result as Result {underscoreToCase} deriving (Eq, Show)#}


--------------------------------------------------------------------------------
-- | Generating Signatures

-- crsSignature   :: Handle -> IO (Maybe CSignature, Result)
-- crsSignature h = undefined

type CFilePtr = Ptr CFile

-- | Loading signatures
-- the c-function is:
-- rs_result rs_loadsig_file(FILE *, rs_signature_t **, rs_stats_t *);
{#fun unsafe rs_loadsig_file as cRSLoadSigFile
      { id      `CFilePtr'
      , alloca- `SignaturePtr' peek*
      , alloca- `StatsPtr'
      } -> `Result' cIntToEnum
 #}


--------------------------------------------------------------------------------
-- | Helper functions

cIntToEnum :: Enum a => CInt -> a
cIntToEnum = toEnum . fromIntegral

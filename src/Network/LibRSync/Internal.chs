{-# LANGUAGE ForeignFunctionInterface #-}
module Network.LibRSync.Internal where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable

#include "librsync.h"

--------------------------------------------------------------------------------
-- | The CTypes

type CRSLong = CLLong

-- | Extract the signature type
{#pointer *rs_signature_t as Stignature #}

-- | Extract the stats type
{#pointer *rs_stats as Stats #}

-- | The results type
{#enum rs_result as Result {underscoreToCase} deriving (Eq, Show)#}


--------------------------------------------------------------------------------
-- | The functions


-- | Loading signatures
-- the c-function is:
-- rs_result rs_loadsig_file(FILE *, rs_signature_t **, rs_stats_t *);
{#fun unsafe rs_loadsig_file as cRSLoadSigFile
      { handleToCFile* `Handle'
      , alloca- `Ptr Signature' peek*
      , alloca- `Ptr Stats' peek*
      } -> `Result' cIntToEnum
 #}


--------------------------------------------------------------------------------
-- | Helper functions

cIntToEnum :: Enum a => CInt -> a
cIntToEnum = toEnum . cIntConv

-- | Convert a handle into a pointer to a CFile
-- Code taken from http://www.pwan.org/wp/?p=33
handleToCFile     :: Handle -> String -> IO (Ptr CFile)
handleToCFile h m =
 do iomode <- newCString m
    -- Duplicate the handle, so the original stays open
    -- after the handleToFd call closes the duplicate
    dup_h <- hDuplicate h
    fd <- handleToFd dup_h
    fdopen fd iomode

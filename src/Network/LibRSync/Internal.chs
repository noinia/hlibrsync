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

#include "internal.h"

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



-- | Given a file, and a handle h, opened in binary Write mode, indicating
-- where to write the signature to. Generate the signature. The signature is
-- written to the file corresponding to handle h. The handle is closed as a result.
-- The function returns a Maybe
-- String, indicating if something goes wrong. If the result is Nothing, the
-- operation succeeded.
hSignature     :: FilePath -> Handle -> IO (Maybe String)
hSignature p h = copyHandleToFd h >>= cGenSig p >>= \r -> case r of
                    RsDone -> return Nothing
                    _      -> return $ Just "some error"

{#fun unsafe genSig as cGenSig
      { `String'
      , fromIntegral `Fd'
      } -> `Result' cIntToEnum
 #}


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

--------------------------------------------------------------------------------
-- | Patch

{#fun unsafe applyPatch as applyPatch'
      { fromIntegral `Fd'
      , `String'
      , `String'
      } -> `Result' cIntToEnum
 #}

--------------------------------------------------------------------------------
-- | Helper functions

cIntToEnum :: Enum a => CInt -> a
cIntToEnum = toEnum . fromIntegral

copyHandleToFd = handleToFd
-- copyHandleToFd h = hDuplicate h >>= handleToFd

test = do
  h <- openBinaryFile "/tmp/signature" ReadWriteMode
  r <- hSignature "/Users/frank/tmp/httpd-error.log" h
  print r
  hClose h

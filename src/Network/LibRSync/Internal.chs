{-# LANGUAGE ForeignFunctionInterface,
             EmptyDataDecls
 #-}
module Network.LibRSync.Internal where

import Data.ByteString
import Data.Conduit

import System.IO
import GHC.IO.Handle
import System.Posix.IO
import System.Posix.Types

import Foreign
import Foreign.Marshal.Alloc
-- import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
-- import Foreign.Storable

#include "internal.h"

-- #include "../../../c_src/internal.h"

--------------------------------------------------------------------------------
-- | The CTypes

data CInMemoryBuffer = CInMemoryBuffer (Ptr Char) CSize CSize

{#pointer *inMemoryBuffer_t as CInMemoryBufferPtr -> CInMemoryBuffer #}

-- data CRSFileBuf = CRSFileBuf (Ptr CFile) CSize

-- {#pointer *rs_filebuf_t as CRSFileBufPtr -> CRSFileBuf #}

data CJob
data CBuffers
data CRSFileBuf

data CRSyncSourceState = CRSyncSourceState { f :: Ptr CFile
                                           , job :: Ptr CJob
                                           , buf :: Ptr CBuffers
                                           , inBuf :: Ptr CRSFileBuf
                                           , outputBuf :: CInMemoryBufferPtr
                                           }

instance Storable CRSyncSourceState where
    sizeOf _  = {#sizeof rsyncSourceState_t #}
    alignment = undefined
    peek      = undefined
    poke      = undefined


{#pointer *rsyncSourceState_t as CRSyncSourceStatePtr -> CRSyncSourceState #}


-- data BlockSig
-- {#pointer *rs_block_sig_t as BlockSigPtr -> BlockSig #}


-- type CSignature = Ptr CFile

-- -- | The Signature type
-- {#pointer *rs_signature_t as SignaturePtr -> CSignature #}

-- | The results type
{#enum rs_result as Result {underscoreToCase} deriving (Eq, Show)#}

--------------------------------------------------------------------------------
-- | Generating Signatures

type RSyncSourceState = CRSyncSourceStatePtr

type Signature = ByteString


startSignature :: FilePath -> IO RSyncSourceState
startSignature path = do
  state <- malloc :: IO (Ptr CRSyncSourceState)
  rsres  <- cStartSignature path state
  return state
  -- TODO: check what to do with rsres
  -- case rsres of
  --   RsDone ->

endSignature :: RSyncSourceState -> IO ()
endSignature state = cEndSignature state >> free state

signatureSource :: MonadResource m => RSyncSourceState -> Source m Signature
signatureSource state = undefined -- do
    -- (CInMemoryBuffer xs size inUse) <- {#get inMemorybuffer->outputBuf-> state







{#fun unsafe startSignature as cStartSignature
      { `String' -- FilePath
      , id `CRSyncSourceStatePtr'
      } -> `Result' cIntToEnum
 #}

{#fun unsafe signatureChunk as cSignatureChunk
      { id `CRSyncSourceStatePtr'
      , `Bool'
      } -> `Result' cIntToEnum
 #}

{#fun unsafe endSignature as cEndSignature
      { id `CRSyncSourceStatePtr'
      } -> `()'
 #}


-- type Signature = ByteString


--------------------------------------------------------------------------------
-- | Delta


--------------------------------------------------------------------------------
-- | Patch


--------------------------------------------------------------------------------
-- | Helper functions

cIntToEnum :: Enum a => CInt -> a
cIntToEnum = toEnum . fromIntegral

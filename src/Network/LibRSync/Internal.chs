{-# LANGUAGE ForeignFunctionInterface,
             EmptyDataDecls
 #-}
module Network.LibRSync.Internal where

import Control.Applicative((<$>),(<*>))
import Control.Monad

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

data CInMemoryBuffer = CInMemoryBuffer (Ptr CChar) CSize CSize

{#pointer *inMemoryBuffer_t as CInMemoryBufferPtr -> CInMemoryBuffer #}

instance Storable CInMemoryBuffer where
    sizeOf    = const {#sizeof inMemoryBuffer_t #}
    alignment = const 4
    peek p    = CInMemoryBuffer
                <$> {#get inMemoryBuffer_t->buffer #} p
                <*> liftM fromIntegral ({#get inMemoryBuffer_t->size #}   p)
                <*> liftM fromIntegral ({#get inMemoryBuffer_t->inUse #}  p)
    poke      = undefined

getData                          :: CInMemoryBuffer -> IO ByteString
getData (CInMemoryBuffer xs _ s) = packCStringLen (xs,fromIntegral s)






-- data CRSFileBuf = CRSFileBuf (Ptr CFile) CSize

-- {#pointer *rs_filebuf_t as CRSFileBufPtr -> CRSFileBuf #}

data CJob
data CBuffers
data CRSFileBuf

data CRSyncSourceState = CRSyncSourceState { f :: Ptr CFile
                                           , job :: Ptr CJob
                                           , buf :: Ptr CBuffers
                                           , inBuf :: Ptr CRSFileBuf
                                           , outputBuf' :: CInMemoryBufferPtr
                                           }

{#pointer *rsyncSourceState_t as CRSyncSourceStatePtr -> CRSyncSourceState #}

instance Storable CRSyncSourceState where
    sizeOf    = const {#sizeof rsyncSourceState_t #}
    alignment = const 4
               -- We can only access the output buffer and the return state
    peek p    = CRSyncSourceState undefined undefined undefined undefined
                <$> {#get rsyncSourceState_t->outputBuf #} p
    poke      = undefined

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

outputBuf   :: RSyncSourceState -> IO CInMemoryBuffer
outputBuf p = (outputBuf' <$> peek p) >>= peek

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

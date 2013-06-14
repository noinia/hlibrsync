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

-- | The results type
{#enum rs_result as RsResult {underscoreToCase} deriving (Eq, Show)#}



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


data CJob
data CBuffers
data CRSFileBuf

data CRSyncSourceState = CRSyncSourceState { f'         :: Ptr CFile
                                           , job'       :: Ptr CJob
                                           , buf'       :: Ptr CBuffers
                                           , inBuf'     :: Ptr CRSFileBuf
                                           , outputBuf' :: CInMemoryBufferPtr
                                           , status'    :: RsResult
                                           }

{#pointer *rsyncSourceState_t as CRSyncSourceStatePtr -> CRSyncSourceState #}

instance Storable CRSyncSourceState where
    sizeOf    = const {#sizeof rsyncSourceState_t #}
    alignment = const 4
               -- We can only access the output buffer and the return state
    peek p    = CRSyncSourceState undefined undefined undefined undefined
                <$> {#get rsyncSourceState_t->outputBuf #} p
                <*> liftM cIntToEnum ({#get rsyncSourceState_t->status #} p)
    poke      = undefined


--------------------------------------------------------------------------------
-- | Generating Signatures

type RSyncSourceState = CRSyncSourceStatePtr

outputBuf   :: RSyncSourceState -> IO CInMemoryBuffer
outputBuf p = (outputBuf' <$> peek p) >>= peek

type Signature = ByteString


initSignature :: FilePath -> IO RSyncSourceState
initSignature path = do
  state <- malloc :: IO (Ptr CRSyncSourceState)
  rsres  <- cInitSignature path state
  return state
  -- TODO: check what to do with rsres
  -- case rsres of
  --   RsDone ->

finalizeSignature :: RSyncSourceState -> IO ()
finalizeSignature state = cFinalizeSignature state >> free state

signatureSource :: MonadResource m => RSyncSourceState -> Source m Signature
signatureSource state = undefined -- TODO, st. like: outputBuf state >>= getData >>= yield







{#fun unsafe initSignature as cInitSignature
      { `String' -- FilePath
      , id `CRSyncSourceStatePtr'
      } -> `()'
 #}

{#fun unsafe signatureChunk as cSignatureChunk
      { id `CRSyncSourceStatePtr'
      , `Bool'
      } -> `()'
 #}

{#fun unsafe finalizeSignature as cFinalizeSignature
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

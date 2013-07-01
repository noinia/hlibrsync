{-# LANGUAGE ForeignFunctionInterface,
             EmptyDataDecls
 #-}
module Network.LibRSync.Internal where

import Control.Applicative((<$>),(<*>))

import Control.Monad
import Control.Monad.IO.Class

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

--------------------------------------------------------------------------------
-- | Generating Signatures



data CRSyncSignatureState = CRSyncSignatureState { f'         :: Ptr CFile
                                                 , job'       :: Ptr CJob
                                                 , buf'       :: Ptr CBuffers
                                                 , inBuf'     :: Ptr CRSFileBuf
                                                 , outputBuf' :: CInMemoryBufferPtr
                                                 , status'    :: RsResult
                                                 }

{#pointer *rsyncSignatureState_t as CRSyncSignatureStatePtr -> CRSyncSignatureState #}

instance Storable CRSyncSignatureState where
    sizeOf    = const {#sizeof rsyncSignatureState_t #}
    alignment = const 4
               -- We can only access the output buffer and the return state
    peek p    = CRSyncSignatureState undefined undefined undefined undefined
                <$> {#get rsyncSignatureState_t->outputBuf #} p
                <*> liftM cIntToEnum ({#get rsyncSignatureState_t->status #} p)
    poke      = undefined


type RSyncSignatureState = CRSyncSignatureStatePtr

outputBuf   :: RSyncSignatureState -> IO CInMemoryBuffer
outputBuf p = (outputBuf' <$> peek p) >>= peek

status   :: RSyncSignatureState -> IO RsResult
status p = status' <$> peek p

type Signature = ByteString


initSignature :: FilePath -> IO RSyncSignatureState
initSignature path = do
  state <- malloc :: IO (Ptr CRSyncSignatureState)
  rsres  <- cInitSignature path state
  return state
  -- TODO: check what to do with rsres: if RsError we should throw an error or so
  -- case rsres of
  --   RsDone ->

finalizeSignature       :: RSyncSignatureState -> IO ()
finalizeSignature state = cFinalizeSignature state >> free state

signatureSource       :: MonadResource m => RSyncSignatureState -> Source m Signature
signatureSource state = liftIO (status state) >>= \s -> case s of
                          RsBlocked -> do
                                         liftIO $ cSignatureChunk state True
                                         buf   <- liftIO $ outputBuf state
                                         -- TODO: verify that we really execute this.
                                         chunk <- liftIO $ getData buf
                                         yield chunk
                                         signatureSource state
                          RsDone    -> return ()
                          err       -> error "error!"


{#fun unsafe initSignature as cInitSignature
      { `String' -- FilePath
      , id `CRSyncSignatureStatePtr'
      } -> `()'
 #}

{#fun unsafe signatureChunk as cSignatureChunk
      { id `CRSyncSignatureStatePtr'
      , `Bool'
      } -> `()'
 #}

{#fun unsafe finalizeSignature as cFinalizeSignature
      { id `CRSyncSignatureStatePtr'
      } -> `()'
 #}


--------------------------------------------------------------------------------
-- | Delta

type Delta = ByteString


--------------------------------------------------------------------------------
-- | Patch


data CRSyncPatchState = CRSyncPatchState { inF'       :: Ptr CFile
                                         , outF'      :: Ptr CFile
                                         , djob'      :: Ptr CJob
                                         , dbuf'      :: Ptr CBuffers
                                         , deltaBuf'  :: CInMemoryBufferPtr
                                         , deltaEOF'  :: Bool
                                         , doutputBuf':: Ptr CRSFileBuf
                                         , dstatus'   :: RsResult
                                         }

{#pointer *rsyncPatchState_t as CRSyncPatchStatePtr -> CRSyncPatchState #}

-- instance Storable CRSyncPatchState where
--     sizeOf    = const {#sizeof rsyncPatchState_t #}
--     alignment = const 4
--                -- We can only access the output buffer and the return state
--     peek p    = CRSyncPatchState undefined undefined undefined undefined
--                 <$> {#get rsyncPatchState_t->outputBuf #} p
--                 <*> liftM cIntToEnum ({#get rsyncPatchState_t->status #} p)
--     poke      = undefined


{#fun unsafe initPatch as cInitPatch
      { `String' -- FilePath to the input file
      , `String' -- FilePath to the output file
      , id `CRSyncPatchStatePtr'
      } -> `()'
 #}

{#fun unsafe patchChunk as cPatchChunk
      { id `CRSyncPatchStatePtr'
      } -> `()'
 #}

{#fun unsafe finalizePatch as cFinalizePatch
      { id `CRSyncPatchStatePtr'
      } -> `()'
 #}

type RSyncPatchState = CRSyncPatchStatePtr

initPatch                :: FilePath -> FilePath -> IO RSyncPatchState
initPatch inPath outPath = undefined

finalizePatch       :: RSyncPatchState -> IO ()
finalizePatch state = undefined

patchSink       :: MonadResource m => RSyncPatchState -> Sink Delta m ()
patchSink state = undefined

--------------------------------------------------------------------------------
-- | Helper functions

cIntToEnum :: Enum a => CInt -> a
cIntToEnum = toEnum . fromIntegral

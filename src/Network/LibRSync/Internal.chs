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

-- | The data buffer we use to communicate data between Haskell and C
data CInMemoryBuffer = CInMemoryBuffer (Ptr CChar) CSize CSize

{#pointer *inMemoryBuffer_t as CInMemoryBufferPtr -> CInMemoryBuffer #}

instance Storable CInMemoryBuffer where
    sizeOf    = const {#sizeof inMemoryBuffer_t #}
    alignment = const 4
    peek p    = CInMemoryBuffer
                <$> {#get inMemoryBuffer_t->buffer #} p
                <*> liftM fromIntegral ({#get inMemoryBuffer_t->size  #}  p)
                <*> liftM fromIntegral ({#get inMemoryBuffer_t->inUse #}  p)
    poke p (CInMemoryBuffer xs _ l) = setData' p (xs,fromIntegral l)

-- TODO, We should not do the set->buffer but really just a memcopy
setData'            :: CInMemoryBufferPtr -> CStringLen -> IO ()
setData' buf (xs,l) = do
                        {#set inMemoryBuffer_t->buffer #} buf   xs
                        {#set inMemoryBuffer_t->inUse  #} buf $ fromIntegral l

getData                          :: CInMemoryBuffer -> IO ByteString
getData (CInMemoryBuffer xs _ s) = packCStringLen (xs,fromIntegral s)


setData     :: CInMemoryBufferPtr -> ByteString -> IO ()
setData p b = useAsCStringLen b (setData' p)

--------------------------------------------------------------------------------
-- | Generating Signatures

data CJob
data CBuffers
data CRSFileBuf

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


type Signature = ByteString

type RSyncSignatureState = CRSyncSignatureStatePtr


outputBuf   :: RSyncSignatureState -> IO CInMemoryBuffer
outputBuf p = (outputBuf' <$> peek p) >>= peek

status   :: RSyncSignatureState -> IO RsResult
status p = status' <$> peek p

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

instance Storable CRSyncPatchState where
    sizeOf    = const {#sizeof rsyncPatchState_t #}
    alignment = const 4
        -- We can read only the deltaBuf, deltaEof, and dstatus'
    peek p    = (\db deof ds -> CRSyncPatchState undefined undefined
                                                 undefined undefined
                                                 db deof undefined ds)
                <$>                   {#get rsyncPatchState_t->deltaBuf #} p
                <*> liftM (> 0)      ({#get rsyncPatchState_t->deltaEOF #} p)
                <*> liftM cIntToEnum ({#get rsyncPatchState_t->status   #} p)

        -- We (can) set only the deltaEOF fields
    poke p x  = let eof = fromIntegral $ if deltaEOF' x then 1 else 0 in
                {#set rsyncPatchState_t->deltaEOF #} p eof

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

deltaBuffer = deltaBuf' . peek

setDelta             :: RSyncPatchState -> Delta -> IO ()
setDelta state delta = do
                         buf <- deltaBuffer state
                         setData buf delta



--------------------------------------------------------------------------------
-- | Helper functions

cIntToEnum :: Enum a => CInt -> a
cIntToEnum = toEnum . fromIntegral

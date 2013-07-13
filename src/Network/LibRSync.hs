-- module Main where
module Network.LibRSync where


import Control.Applicative((<$>),(<*>),pure)
import Control.Monad
import Control.Monad.IO.Class

import Data.ByteString
import Data.Conduit

import Foreign.Marshal.Alloc

import Network.LibRSync.Internal

--------------------------------------------------------------------------------

signature      :: MonadResource m => FilePath -> Source m Signature
signature path = bracketP (initSignature path) finalizeSignature signatureSource

initSignature :: FilePath -> IO RSyncSignatureState
initSignature path = do
  state <- malloc :: IO RSyncSignatureState
  rsres <- cInitSignature path state
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

--------------------------------------------------------------------------------


delta :: FilePath -> Signature -> IO (Maybe Delta)
delta = undefined


--------------------------------------------------------------------------------

patch :: MonadResource m => FilePath -> FilePath -> Sink Delta m ()
patch inPath outPath = bracketP (initPatch inPath outPath) finalizePatch patchSink

initPatch                :: FilePath -> FilePath -> IO RSyncPatchState
initPatch inPath outPath = do
  state <- malloc :: IO RSyncPatchState
  rsres <- cInitPatch inPath outPath state
  -- TODO: checks on rsres
  return state

finalizePatch       :: RSyncPatchState -> IO ()
finalizePatch state = cFinalizePatch state >> free state

patchSink       :: MonadResource m => RSyncPatchState -> Sink Delta m ()
patchSink state = await >>= \mdelta -> case mdelta of
                    Nothing    -> patchSink' empty True
                    Just delta -> patchSink' delta False >> patchSink state
    where
      patchSink' delta eof = liftIO . useAsCInMemoryBuffer delta $ \deltaBuf ->
                               updateDeltaState state deltaBuf eof >>
                               cPatchChunk state

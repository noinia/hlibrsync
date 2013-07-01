-- module Main where
module Network.LibRSync where

import Control.Monad
import Control.Monad.IO.Class

import Network.LibRSync.Internal

import Data.ByteString

import Data.Conduit

import System.IO

import Control.Applicative

import qualified Data.ByteString as B

--------------------------------------------------------------------------------

signature      :: MonadResource m => FilePath -> Source m Signature
signature path = bracketP (initSignature path) finalizeSignature signatureSource

delta :: FilePath -> Signature -> IO (Maybe Delta)
delta = undefined

patch :: MonadResource m => FilePath -> FilePath -> Sink Delta m ()
patch inPath outPath = bracketP (initPatch inPath outPath) finalizePatch patchSink

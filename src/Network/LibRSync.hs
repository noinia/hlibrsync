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



type Delta     = ByteString

signature      :: MonadResource m => FilePath -> Source m Signature
signature path = bracketP (initSignature path) finalizeSignature signatureSource





-- writeFileSync :: FilePath -> Sink ByteString IO ()
-- writeFileSync = sinkFile

-- writeFileSync path  = do
--   mbs <- await
--   case mbs of
--     Nothing -> liftIO $ print "EOF"
--     Just bs -> do
--                  liftIO $ B.putStr bs
--                  writeFileSync path



delta :: FilePath -> Signature -> IO (Maybe Delta)
delta = undefined

patch :: FilePath -> Delta -> IO ()
patch = undefined


-- main = do
--   s <- signature "/Users/frank/tmp/httpd-error.log"
--   print s
--   case s of
--     Left  _  -> return ()
--     Right s' -> writeSignature "/tmp/signature" s'


writeSignature = B.writeFile

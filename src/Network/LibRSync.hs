-- module Main where
module Network.LibRSync where

import Control.Monad
import Network.LibRSync.Internal

import Data.ByteString

import Data.Conduit

import System.IO

import Control.Applicative

import qualified Data.ByteString as B

--------------------------------------------------------------------------------



type Delta     = ByteString

-- signature      :: FilePath -> Source IO Signature
signature path = bracketP (startSignature path) endSignature signatureSource






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

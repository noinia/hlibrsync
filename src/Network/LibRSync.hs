module Network.LibRSync where

import Data.ByteString
import System.IO

--------------------------------------------------------------------------------

type Signature = ByteString
type Delta     = ByteString

signature :: FilePath -> IO (Maybe Signature)
signature inputFile = undefined

hsignature :: Handle -> IO (Maybe Signature)
hsignature h = undefined

delta :: FilePath -> Signature -> IO (Maybe Delta)
delta = undefined

patch :: FilePath -> Delta -> IO ()
patch = undefined

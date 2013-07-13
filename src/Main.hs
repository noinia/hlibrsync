module Main where

import Network.LibRSync
import Data.Conduit

import Data.Conduit.Binary

sig :: MonadResource m => m ()
sig = signature "/Users/frank/tmp/httpd-error.log" $$ sinkFile "/tmp/sigH"

patch' :: MonadResource m => m ()
patch' = sourceFile "/Users/frank/tmp/httpd-error_delta" $$
         patch "/Users/frank/tmp/httpd-error_editted.log" "/tmp/patched.txt"

main = runResourceT $ patch'

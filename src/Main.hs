module Main where

import Network.LibRSync
import Data.Conduit

import Data.Conduit.Binary



main = runResourceT $ signature "/Users/frank/tmp/httpd-error.log" $$ sinkFile "/tmp/sigH"

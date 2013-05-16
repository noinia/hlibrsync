{-# LANGUAGE ForeignFunctionInterface #-}
module Network.LibRSync( signature
                       , delta
                       , patch
                       ) where
y
import Foreign
import Foreign.C.String
import Foreign.C.Types

--------------------------------------------------------------------------------

type Signature = ByteString
type Delta     = ByteString

signature :: FilePath -> IO (Maybe Signature)
signature inputFile = undefined

delta :: FilePath -> Signature -> IO (Maybe Delta)
delta = undefined

patch :: FilePath -> Delta -> IO ()
patch = undefined

--------------------------------------------------------------------------------

data RSResult = RSResult String

type C_Signature = Signature
type SigFile = CString

foreign import ccall unsafe "librsync.h rs_loadsig_files"
        rs_loadsig_files :: Ptr SigFile -> Ptr C_Signature -> IO RSResult

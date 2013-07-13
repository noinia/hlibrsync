import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils (rawSystemExit)

main = defaultMainWithHooks simpleUserHooks
    { preBuild = \a b -> makeLib a b >> preBuild simpleUserHooks a b }

makeLib         :: Args -> BuildFlags -> IO ()
makeLib _ flags = do
    print "Compiling C Helper functions"
    rawSystemExit (fromFlag $ buildVerbosity flags) "env"
        ["CFLAGS=-D_LIB", "make", "--directory=c_src", "all"]




-- import Distribution.Simple
-- main = defaultMain

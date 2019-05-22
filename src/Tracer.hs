module Tracer where

import GHC.Exts.Heap
import System.IO.Unsafe
import Debug.Trace

traceAny :: a -> b -> b
traceAny a = trace (show $ unsafePerformIO $ getClosureData a)

traceAnyIO :: a -> IO ()
traceAnyIO a = do
    cd <- getClosureData a
    print cd
    return ()
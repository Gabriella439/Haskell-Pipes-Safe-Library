-- | Prelude of proxies providing simple resource management features

{-# LANGUAGE Rank2Types #-}

module Control.Proxy.Safe.Prelude (
    -- * Handle allocation
    withFile,

    -- * String I/O
    -- $string
    readFileS,
    writeFileD
    ) where

import Control.Proxy (Proxy(request, respond), Producer)
import Control.Proxy.Safe.Core (SafeIO, MonadSafeIO, ExceptionP, bracket, tryIO)
import qualified System.IO as IO

-- | Safely allocate a 'IO.Handle' within a managed 'Proxy'
withFile
    :: (MonadSafeIO m, Proxy p)
    => FilePath                                   -- ^File
    -> IO.IOMode                                  -- ^IO Mode
    -> (IO.Handle -> ExceptionP p a' a b' b m r)  -- ^Continuation
    -> ExceptionP p a' a b' b m r
withFile file ioMode = bracket (IO.openFile file ioMode) IO.hClose

{- $string
    Note that 'String's are very inefficient, and I will release future separate
    packages with 'ByteString' and 'Text' operations.  I only provide these to
    allow users to test simple I/O without requiring any additional library
    dependencies.
-}
{-| Read from a file, lazily opening the 'IO.Handle' and automatically closing
    it afterwards
-}
readFileS
    :: (Proxy p) => FilePath -> () -> Producer (ExceptionP p) String SafeIO ()
readFileS file () = withFile file IO.ReadMode $ \handle -> do
    let go = do
            eof <- tryIO $ IO.hIsEOF handle
            if eof
                then return ()
                else do
                    str <- tryIO $ IO.hGetLine handle
                    respond str
                    go
    go

{-| Write to a file, lazily opening the 'IO.Handle' and automatically closing it
    afterwards
-}
writeFileD
    :: (Proxy p) => FilePath -> x -> ExceptionP p x String x String SafeIO r
writeFileD file x0 = do
    withFile file IO.WriteMode $ \handle -> do
        let go x = do
                str <- request x
                tryIO $ IO.hPutStrLn handle str
                x2 <- respond str
                go x2
        go x0

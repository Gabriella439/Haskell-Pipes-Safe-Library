-- | Simple resource management functions

{-# LANGUAGE RankNTypes, TypeFamilies #-}

module Pipes.Safe.Prelude (
    -- * Handle management
    withFile,

    -- * String I/O
    -- $strings
    readFile,
    writeFile
    ) where

import Pipes (Producer', Consumer')
import Pipes.Safe (bracket, MonadSafe, Base)
import qualified Pipes.Prelude as P
import qualified System.IO as IO
import Prelude hiding (readFile, writeFile)

-- | Acquire a 'IO.Handle' within 'MonadSafe'
withFile
    :: (MonadSafe m, Base m ~ IO)
    => FilePath -> IO.IOMode -> (IO.Handle -> m r) -> m r
withFile file ioMode = bracket (IO.openFile file ioMode) IO.hClose
{-# INLINABLE withFile #-}

{- $strings
    Note that 'String's are very inefficient, and I will release future separate
    packages with 'Data.ByteString.ByteString' and 'Data.Text.Text' operations.
    I only provide these to allow users to test simple I/O without requiring any
    additional library dependencies.
-}

{-| Read lines from a file, automatically opening and closing the file as
    necessary
-}
readFile :: (MonadSafe m, Base m ~ IO) => FilePath -> Producer' String m ()
readFile file = withFile file IO.ReadMode P.fromHandle
{-# INLINABLE readFile #-}

{-| Write lines to a file, automatically opening and closing the file as
    necessary
-}
writeFile :: (MonadSafe m, Base m ~ IO) => FilePath -> Consumer' String m r
writeFile file = withFile file IO.WriteMode P.toHandle
{-# INLINABLE writeFile #-}

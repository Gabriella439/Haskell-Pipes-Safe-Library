-- | Simple resource management functions

{-# LANGUAGE RankNTypes, Safe #-}

module Pipes.Safe.Prelude (
    -- * Handle management
    withFile,

    -- * String I/O
    -- $strings
    readFile,
    writeFile
    ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Pipes (Producer', Consumer')
import Pipes.Safe (bracket, MonadSafe)
import qualified Pipes.Prelude as P
import qualified System.IO as IO
import Prelude hiding (readFile, writeFile)

-- | Acquire a 'IO.Handle' within 'MonadSafe'
withFile :: (MonadSafe m) => FilePath -> IO.IOMode -> (IO.Handle -> m r) -> m r
withFile file ioMode = bracket (liftIO $ IO.openFile file ioMode) (liftIO . IO.hClose)
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
readFile :: (MonadSafe m) => FilePath -> Producer' String m ()
readFile file = withFile file IO.ReadMode P.fromHandle
{-# INLINABLE readFile #-}

{-| Write lines to a file, automatically opening and closing the file as
    necessary
-}
writeFile :: (MonadSafe m) => FilePath -> Consumer' String m r
writeFile file = withFile file IO.WriteMode P.toHandle
{-# INLINABLE writeFile #-}

-- | Simple resource management functions

{-# LANGUAGE RankNTypes, Safe #-}

module Pipes.Safe.Prelude (
    -- * Handle management
    withFile,
    openFile,

    -- * String I/O
    -- $strings
    readFile,
    writeFile,

    -- * Registering/releasing
    allocate,
    allocate_
    ) where

import Control.Monad.Catch (mask_)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Pipes (Producer', Consumer')
import Pipes.Safe (bracket, liftBase, register, Base, MonadSafe, ReleaseKey)
import qualified Pipes.Prelude as P
import qualified System.IO as IO
import Prelude hiding (readFile, writeFile)

-- | Acquire a 'IO.Handle' within 'MonadSafe'
withFile :: (MonadSafe m) => FilePath -> IO.IOMode -> (IO.Handle -> m r) -> m r
withFile file ioMode = bracket (liftIO $ IO.openFile file ioMode) (liftIO . IO.hClose)
{-# INLINABLE withFile #-}

{- | Acquire a 'IO.Handle' within 'MonadSafe'

     The 'ReleaseKey' can be used to close the handle with 'Pipes.Safe.release';
     otherwise the handle will be closed automatically at the conclusion of the
     'MonadSafe' block.
-}
openFile :: MonadSafe m => FilePath -> IO.IOMode -> m (ReleaseKey, IO.Handle)
openFile file ioMode = allocate (liftIO $ IO.openFile file ioMode) (liftIO . IO.hClose)
{-# INLINABLE openFile #-}

{- $strings
    Note that 'String's are very inefficient, and I will release future separate
    packages with 'Data.ByteString.ByteString' and 'Data.Text.Text' operations.
    I only provide these to allow users to test simple I/O without requiring any
    additional library dependencies.
-}

{-| Read lines from a file, automatically opening and closing the file as
    necessary
-}
readFile :: MonadSafe m => FilePath -> Producer' String m ()
readFile file = withFile file IO.ReadMode P.fromHandle
{-# INLINABLE readFile #-}

{-| Write lines to a file, automatically opening and closing the file as
    necessary
-}
writeFile :: MonadSafe m => FilePath -> Consumer' String m r
writeFile file = withFile file IO.WriteMode $ \h -> P.toHandle h
{-# INLINABLE writeFile #-}

{- | Acquire some resource with a guarantee that it will eventually be released

     The 'ReleaseKey' can be passed to 'Pipes.Safe.release' to
     release the resource manually. If this has not been done by the end
     of the 'MonadSafe' block, the resource will be released automatically.
-}
allocate :: MonadSafe m =>
    Base m a             -- ^ Acquire
    -> (a -> Base m ())  -- ^ Release
    -> m (ReleaseKey, a)
allocate acq rel = mask_ $ do
    a <- liftBase acq
    key <- register (rel a)
    return (key, a)

{- | Like 'allocate', but for when the resource itself is not needed

     The acquire  action runs immediately. The 'ReleaseKey' can be passed
     to 'Pipes.Safe.release' to run the release action. If this has not been
     done by the end of the 'MonadSafe' block, the release action will be
     run automatically.
-}
allocate_ :: MonadSafe m =>
    Base m a        -- ^ Acquire
    -> (Base m ())  -- ^ Release
    -> m ReleaseKey
allocate_ acq rel = fmap fst (allocate acq (const rel))

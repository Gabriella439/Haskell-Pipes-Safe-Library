{-# LANGUAGE Rank2Types #-}

module Control.Proxy.Safe.SafeIO (
    Status(restore, upstream, downstream),
    SafeIO(SafeIO, unSafeIO),
    runSafeIO,
    runSaferIO,
    trySafeIO,
    trySaferIO,
    MonadSafeIO(liftSafeIO),
    UninterruptedIO(UninterruptedIO, runUninterruptedIO)
    ) where

import qualified Control.Exception as Ex
import Control.Exception (SomeException)
import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad.Trans.Reader (ReaderT(ReaderT, runReaderT))
import Data.IORef (IORef, newIORef, readIORef)


data Status = Status {
    restore    :: forall a . IO a -> IO a,
    upstream   :: IORef (IO ())          ,
    downstream :: IORef (IO ())          }

{-| 'SafeIO' masks asynchronous exceptions by default, and only unmasks them
    during 'try' or 'tryIO' blocks in order to check all asynchronous
    exceptions.

    'SafeIO' also saves all finalizers dropped as a result of premature
    termination and runs them when the 'P.Session' completes.
-}
newtype SafeIO r = SafeIO { unSafeIO :: ReaderT Status IO r }

instance Functor SafeIO where
    fmap f m = SafeIO (fmap f (unSafeIO m))

instance Applicative SafeIO where
    pure r  = SafeIO (pure r)
    f <*> x = SafeIO (unSafeIO f <*> unSafeIO x)

instance Monad SafeIO where
    return r = SafeIO (return r)
    m >>= f  = SafeIO (unSafeIO m >>= \a -> unSafeIO (f a))

{-| Convert back to the 'IO' monad, running all dropped finalizers at the very
    end and rethrowing any checked exceptions

    This uses 'Ex.mask' to mask asynchronous exceptions and only unmasks them
    during 'try' or 'tryIO'.
-}
runSafeIO :: SafeIO (Either SomeException r) -> IO r
runSafeIO m =
    Ex.mask $ \restore -> do
        huRef <- newIORef (return ())
        hdRef <- newIORef (return ())
        e <- runReaderT (unSafeIO m) (Status restore huRef hdRef)
            `Ex.finally` (do
                hu <- readIORef huRef
                hu
                hd <- readIORef hdRef
                hd )
        case e of
            Left exc -> Ex.throwIO exc
            Right r  -> return r

{-| Convert back to the 'IO' monad, running all dropped finalizers at the very
    end and rethrowing any checked exceptions

    This uses 'Ex.uninterruptibleMask' to mask asynchronous exceptions and only
    unmasks them during 'try' or 'tryIO'.
-}
runSaferIO :: SafeIO (Either SomeException r) -> IO r
runSaferIO m =
    Ex.uninterruptibleMask $ \restore -> do
        huRef <- newIORef (return ())
        hdRef <- newIORef (return ())
        e <- runReaderT (unSafeIO m) (Status restore huRef hdRef)
            `Ex.finally` (do
                hu <- readIORef huRef
                hu
                hd <- readIORef hdRef
                hd )
        case e of
            Left exc -> Ex.throwIO exc
            Right r  -> return r

{-| Convert back to the 'IO' monad, running all dropped finalizers at the very
    end and preserving exceptions as 'Left's

    This uses 'Ex.mask' to mask asynchronous exceptions and only unmasks them
    during 'try' or 'tryIO'.
-}
trySafeIO :: SafeIO e -> IO e
trySafeIO m =
    Ex.mask $ \restore -> do
        huRef <- newIORef (return ())
        hdRef <- newIORef (return ())
        runReaderT (unSafeIO m) (Status restore huRef hdRef) `Ex.finally` (do
            hu <- readIORef huRef
            hu
            hd <- readIORef hdRef
            hd )

{-| Convert back to the 'IO' monad, running all dropped finalizers at the very
    end and preserving exceptions as 'Left's

    This uses 'Ex.uninterruptibleMask' to mask asynchronous exceptions and only
    unmasks them during 'try' or 'tryIO'.
-}
trySaferIO :: SafeIO e -> IO e
trySaferIO m =
    Ex.uninterruptibleMask $ \restore -> do
        huRef <- newIORef (return ())
        hdRef <- newIORef (return ())
        runReaderT (unSafeIO m) (Status restore huRef hdRef) `Ex.finally` (do
            hu <- readIORef huRef
            hu
            hd <- readIORef hdRef
            hd )

class Monad m => MonadSafeIO m where
  -- | A monad morphism
  liftSafeIO :: SafeIO x -> m x

instance MonadSafeIO SafeIO where
  liftSafeIO = id

instance MonadSafeIO IO where
  liftSafeIO = trySafeIO


-- THIS SHOULD BE USED WITH GREAT CARE <3 ghc source of uninterruptibleMask
newtype UninterruptedIO a = UninterruptedIO { runUninterruptedIO :: IO a }
instance Functor UninterruptedIO where
  fmap f (UninterruptedIO m) = UninterruptedIO (fmap f m)
instance Applicative UninterruptedIO where
  pure a = UninterruptedIO (pure a)
  (UninterruptedIO f) <*> (UninterruptedIO x) = UninterruptedIO (f <*> x)
instance Monad UninterruptedIO where
  return a = UninterruptedIO (return a)
  (UninterruptedIO m) >>= k = UninterruptedIO (m >>= runUninterruptedIO . k)
instance MonadSafeIO UninterruptedIO where
  liftSafeIO = UninterruptedIO . trySaferIO

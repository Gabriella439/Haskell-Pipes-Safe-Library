{-# LANGUAGE RankNTypes #-}

module Pipes.Safe
    ( -- * SafeT
      SafeT(..)
    , runSafeT
    , runSafeP
    , ReleaseKey
    , MonadSafe(..)

    -- * Re-exports
    , module Control.Monad.Catch
    ) where

import Control.Applicative (Applicative(pure, (<*>)))
import qualified Control.Monad.Catch as C
import Control.Monad.Catch
    ( MonadCatch(..)
    , mask_
    , uninterruptibleMask_
    , catchAll
    , catchIOError
    , catchJust
    , catchIf
    , Handler(..)
    , catches
    , handle
    , handleAll
    , handleIOError
    , handleJust
    , handleIf
    , try
    , tryJust
    , Exception(..)
    , SomeException
    )
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import qualified Control.Monad.Catch.Pure          as E
import qualified Control.Monad.Trans.Identity      as I
import qualified Control.Monad.Trans.Reader        as R
import qualified Control.Monad.Trans.RWS.Lazy      as RWS
import qualified Control.Monad.Trans.RWS.Strict    as RWS'
import qualified Control.Monad.Trans.State.Lazy    as S
import qualified Control.Monad.Trans.State.Strict  as S'
import qualified Control.Monad.Trans.Writer.Lazy   as W
import qualified Control.Monad.Trans.Writer.Strict as W'
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map as M
import Data.Monoid (Monoid)
import Pipes (Proxy, Effect)
import Pipes.Internal (unsafeHoist, Proxy(..))
import Pipes.Lift (liftCatchError, runReaderP)

-- TODO: Get bounds on dependencies correct
-- TODO: Add haddocks

data Restore m = Unmasked | Masked (forall x . m x -> m x)

liftMask
    :: (MonadIO m, MonadCatch m)
    => (forall s . ((forall x . m x -> m x) -> m s) -> m s)
    -> ((forall x . Proxy a' a b' b m x -> Proxy a' a b' b m x)
        -> Proxy a' a b' b m r)
    -> Proxy a' a b' b m r
liftMask mask_ k = do
        ioref <- liftIO (newIORef Unmasked)
        let unmask p = do
                mRestore <- liftIO (readIORef ioref)
                case mRestore of
                    Unmasked       -> p
                    Masked restore -> do
                        r <- unsafeHoist restore p
                        lift $ restore $ return ()
                        return r
            loop p = case p of
                Request a' fa  -> Request a' (loop . fa )
                Respond b  fb' -> Respond b  (loop . fb')
                M m            -> M $ mask_ $ \restore -> do
                    liftIO $ writeIORef ioref (Masked restore)
                    let loop' m = do
                            p' <- m
                            case p' of
                                M m' -> loop' m'
                                _    -> return p'
                    p' <- loop' m
                    liftIO $ writeIORef ioref  Unmasked
                    return (loop p')
                Pure r         -> Pure r
        loop (k unmask)

instance (MonadCatch m, MonadIO m) => MonadCatch (Proxy a' a b' b m) where
    throwM = lift . throwM
    catch  = liftCatchError C.catch
    mask                = liftMask mask
    uninterruptibleMask = liftMask uninterruptibleMask

data Finalizers m = Finalizers
    { nextKey    :: !Integer
    , finalizers :: !(M.Map Integer (m ()))
    }

newtype SafeT m r = SafeT { unSafeT :: R.ReaderT (IORef (Finalizers m)) m r }

-- Deriving 'Functor'
instance (Monad m) => Functor (SafeT m) where
    fmap f m = SafeT (do
        r <- unSafeT m
        return (f r) )

-- Deriving 'Applicative'
instance (Monad m) => Applicative (SafeT m) where
    pure r = SafeT (return r)
    mf <*> mx = SafeT (do
        f <- unSafeT mf
        x <- unSafeT mx
        return (f x) )

-- Deriving 'Monad'
instance (Monad m) => Monad (SafeT m) where
    return r = SafeT (return r)
    m >>= f = SafeT (do
        r <- unSafeT m
        unSafeT (f r) )

-- Deriving 'MonadIO'
instance (MonadIO m) => MonadIO (SafeT m) where
    liftIO m = SafeT (liftIO m)

-- Deriving 'MonadCatch'
instance (MonadCatch m) => MonadCatch (SafeT m) where
    throwM e = SafeT (throwM e)
    m `catch` f = SafeT (unSafeT m `C.catch` \r -> unSafeT (f r))
    mask k = SafeT (mask (\restore ->
        unSafeT (k (\ma -> SafeT (restore (unSafeT ma)))) ))
    uninterruptibleMask k = SafeT (uninterruptibleMask (\restore ->
        unSafeT (k (\ma -> SafeT (restore (unSafeT ma)))) ))

instance MonadTrans SafeT where
    lift m = SafeT (lift m)

runSafeT :: (MonadCatch m, MonadIO m) => SafeT m r -> m r
runSafeT m = C.bracket
    (liftIO $ newIORef $! Finalizers 0 M.empty)
    (\ioref -> do
        Finalizers _ fs <- liftIO (readIORef ioref)
        mapM snd (M.toDescList fs) )
    (R.runReaderT (unSafeT m))
{-# INLINABLE runSafeT #-}

runSafeP :: (MonadCatch m, MonadIO m) => Effect (SafeT m) r -> Effect m r
runSafeP m = C.bracket
    (liftIO $ newIORef $! Finalizers 0 M.empty)
    (\ioref -> do
        Finalizers _ fs <- liftIO (readIORef ioref)
        lift $ mapM snd (M.toDescList fs) )
    (\ioref -> runReaderP ioref (unsafeHoist unSafeT m))
{-# INLINABLE runSafeP #-}

newtype ReleaseKey = ReleaseKey { unlock :: Integer }

class (MonadCatch m, MonadIO m) => MonadSafe m where
    register :: IO () -> m ReleaseKey
    release  :: ReleaseKey -> m ()

instance (MonadIO m, MonadCatch m) => MonadSafe (SafeT m) where
    register io = do
        ioref <- SafeT R.ask
        liftIO $ do
            Finalizers n fs <- readIORef ioref
            writeIORef ioref $! Finalizers (n + 1) (M.insert n (liftIO io) fs)
            return (ReleaseKey n)
    release key = do
        ioref <- SafeT R.ask
        liftIO $ do
            Finalizers n fs <- readIORef ioref
            writeIORef ioref $! Finalizers n (M.delete (unlock key) fs)

instance (MonadSafe m) => MonadSafe (Proxy a' a b' b m) where
    register = lift . register
    release  = lift . release

instance (MonadSafe m) => MonadSafe (I.IdentityT m) where
    register = lift . register
    release  = lift . release

instance (MonadSafe m) => MonadSafe (E.CatchT m) where
    register = lift . register
    release  = lift . release

instance (MonadSafe m) => MonadSafe (R.ReaderT i m) where
    register = lift . register
    release  = lift . release

instance (MonadSafe m) => MonadSafe (S.StateT s m) where
    register = lift . register
    release  = lift . release

instance (MonadSafe m) => MonadSafe (S'.StateT s m) where
    register = lift . register
    release  = lift . release

instance (MonadSafe m, Monoid w) => MonadSafe (W.WriterT w m) where
    register = lift . register
    release  = lift . release

instance (MonadSafe m, Monoid w) => MonadSafe (W'.WriterT w m) where
    register = lift . register
    release  = lift . release

instance (MonadSafe m, Monoid w) => MonadSafe (RWS.RWST i w s m) where
    register = lift . register
    release  = lift . release

instance (MonadSafe m, Monoid w) => MonadSafe (RWS'.RWST i w s m) where
    register = lift . register
    release  = lift . release

onAbort :: (MonadSafe m) => m a -> IO b -> m a
m1 `onAbort` io = do
    key <- register (io >> return ())
    r   <- m1
    release key
    return r
{-# INLINABLE onAbort #-}

finally :: (MonadSafe m) => m a -> IO b -> m a
m1 `finally` after = bracket_ (return ()) after m1
{-# INLINABLE finally #-}

bracket :: (MonadSafe m) => IO a -> (a -> IO b) -> (a -> m c) -> m c
bracket before after action = mask $ \restore -> do
    h <- liftIO before
    r <- restore (action h) `onAbort` after h
    liftIO (after h)
    return r
{-# INLINABLE bracket #-}

bracket_ :: (MonadSafe m) => IO a -> IO b -> m c -> m c
bracket_ before after action = bracket before (\_ -> after) (\_ -> action)
{-# INLINABLE bracket_ #-}

bracketOnError :: (MonadSafe m) => IO a -> (a -> IO b) -> (a -> m c) -> m c
bracketOnError before after action = mask $ \restore -> do
    h <- liftIO before
    restore (action h) `onAbort` after h
{-# INLINABLE bracketOnError #-}

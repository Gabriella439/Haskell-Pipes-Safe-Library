-- | Resource acquisition integrated with proxies

{-# LANGUAGE Rank2Types #-}

module Control.Proxy.Safe (
    -- * Safe IO
    SafeIO,
    runSafeIO,

    -- * ExceptionP
    -- $exceptionp
    ExceptionP,
    runExceptionP,
    runExceptionK,

    -- * Exception handling
    try,
    bracket,
    throw,
    catch,
    handle,

    -- * Prompt Finalization
    -- $prompt
    unsafeCloseU,
    unsafeCloseD
    ) where

import qualified Control.Exception as Ex
import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad.IO.Class(MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT(runReaderT), asks)
import qualified Control.Proxy as P
import Control.Proxy ((>->))
import qualified Control.Proxy.Trans.Either as E
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Prelude hiding (catch)

-- Documentation
import qualified Prelude (catch)

registerK
 :: (Monad m, P.Proxy p)
 => (forall x . SafeIO x -> m x)
 -> IO ()
 -> (b' -> p a' a b' b m r)
 -> (b' -> p a' a b' b m r)
registerK nat h k =
    P.runIdentityK (P.hoistK nat up) >-> k >-> P.runIdentityK (P.hoistK nat dn)
  where
    dn b'0 = do
        b0 <- P.request b'0
        huRef <- lift $ SafeIO $ asks downstream
        let dn' b = do
                hu <- lift $ liftIO $ do
                    hu <- readIORef huRef
                    writeIORef huRef (hu >> h)
                    return hu
                b' <- P.respond b
                lift $ liftIO $ writeIORef huRef hu
                b2 <- P.request b'
                dn' b2
        dn' b0
    up a'0 = do
        hdRef <- lift $ SafeIO $ asks upstream
        let up' a' = do
                hd  <- lift $ liftIO $ do
                    hd <- readIORef hdRef
                    writeIORef hdRef (h >> hd)
                    return hd
                a   <- P.request a'
                lift $ liftIO $ writeIORef hdRef hd
                a'2 <- P.respond a
                up' a'2
        up' a'0

register
 :: (Monad m, P.Proxy p)
 => (forall x . SafeIO x -> m x)
 -> IO ()
 -> p a' a b' b m r
 -> p a' a b' b m r
register nat h p = registerK nat h (\_ -> p) undefined
{- This is safe and there is a way that does not use 'undefined' if I slightly
   restructure the Proxy type class, but this will do for now. -}

data Status = Status {
    restore    :: forall a . IO a -> IO a,
    upstream   :: IORef (IO ())          ,
    downstream :: IORef (IO ())          }

{-| 'SafeIO' extends the 'IO' monad with the ability to store registered
    finalizers and unmask computations

    'liftIO' converts 'IO' actions to 'SafeIO' actions, but it will mask
    asynchronous exceptions and will not be locally catchable.
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

instance MonadIO SafeIO where
    liftIO = SafeIO . lift

{-| Convert back to the 'IO' monad, running all dropped finalizers at the very
    end

    This masks asynchronous exceptions and only unmasks them in 'try' blocks. -}
runSafeIO :: SafeIO r -> IO r
runSafeIO m =
    Ex.mask $ \restore -> do
        huRef <- newIORef (return ())
        hdRef <- newIORef (return ())
        runReaderT (unSafeIO m) (Status restore huRef hdRef) `Ex.finally` (do
            hu    <- readIORef huRef
            hd    <- readIORef hdRef
            hu
            hd )

{- $exceptionp
    'ExceptionP' is a type synonym around 'E.EitherP' from
    @Control.Proxy.Trans.Either@.  This means that you can use the more general
    functions from 'E.EitherP' if you prefer.

    Similarly, use 'E.runEitherP' instead of 'runExceptionP' if you prefer not
    to rethrow any final 'Ex.Exception'.
-}

-- | A proxy transformer that stores exceptions using 'E.EitherP'
type ExceptionP = E.EitherP Ex.SomeException

{-| Run an 'ExceptionP' proxy transformer, converting all 'Left's back to
    'E.Exception's -}
runExceptionP
 :: (MonadIO m, P.Proxy p) => ExceptionP p a' a b' b m r -> p a' a b' b m r
runExceptionP p = P.runIdentityP $ do
    e <- P.IdentityP $ E.runEitherP p
    case e of
       Left someExc -> lift $ liftIO $ Ex.throwIO someExc
       Right r      -> return r

{-| Run an 'ExceptionP' \'@K@\'leisli arrow, converting all 'Left's back to
    'E.Exception's -}
runExceptionK
 :: (MonadIO m, P.Proxy p)
 => (q -> ExceptionP p a' a b' b m r) -> (q -> p a' a b' b m r)
runExceptionK k q = runExceptionP (k q)

{-| 'try' converts all synchronous and asynchronous exceptions to locally
    'catch'able exceptions -}
try :: (P.Proxy p) => IO r -> ExceptionP p a' a b' b SafeIO r
try io = E.EitherP $ P.runIdentityP $ lift $ SafeIO $ do
    _restore <- asks restore
    lift $ Ex.try $ _restore io

{-| Analogous to the Prelude 'bracket'

    The first argument lifts 'bracket' to work with any base monad.  Use 'id' if
    your base monad is already 'SafeIO'.

    'bracket' guarantees that the acquired resource is finalized, even if:

    * Another 'P.Proxy' terminates before the computation completes

    * An exception is thrown

    'bracket' guards against both synchronous and asynchronous exceptions thrown
    anywhere within the 'P.Session', even if they thrown from other proxies or
    not wrapped in 'try'. -}
bracket
 :: (Monad m, P.Proxy p)
 => (forall x . SafeIO x -> m x)       -- ^ Monad morphism
 -> IO h                               -- ^ Acquire resource
 -> (h -> IO r')                       -- ^ Release resource
 -> (h -> ExceptionP p a' a b' b m r)  -- ^ Use resource
 -> ExceptionP p a' a b' b m r
bracket nat before after p = do
    h <- P.hoist nat $ try before
    let finalizer = after h
    r <- register nat (finalizer >> return ()) (p h)
        `E.catch` (\e -> do
            P.hoist nat $ try finalizer
            E.throw e )
    P.hoist nat $ try finalizer
    return r

-- | Analogous to 'Ex.throwIO'
throw :: (Monad m, P.Proxy p, Ex.Exception e) => e -> ExceptionP p a' a b' b m r
throw = E.throw . Ex.toException

{-| Analogous to the Prelude 'Prelude.catch'

    This only catches exceptions from code wrapped in 'try'.  Any other
    exceptions bring down the 'P.Session' and propagate directly outward to
    'runSafeIO'.  You can only catch these using -}
catch
 :: (Ex.Exception e, Monad m, P.Proxy p)
 => ExceptionP p a' a b' b m r         -- ^ Original computation
 -> (e -> ExceptionP p a' a b' b m r)  -- ^ Handler
 -> ExceptionP p a' a b' b m r
catch p f = p `E.catch` (\someExc ->
    case Ex.fromException someExc of
        Nothing -> E.throw someExc
        Just e  -> f e )

{-| Analogous to the Prelude 'Prelude.handle'

    Can only catch exceptions from code wrapped in 'try' -}
handle
 :: (Ex.Exception e, Monad m, P.Proxy p)
 => (e -> ExceptionP p a' a b' b m r)  -- ^ Handler
 -> ExceptionP p a' a b' b m r         -- ^ Original computation
 -> ExceptionP p a' a b' b m r
handle = flip catch

{- $prompt
    This implementation does not /prompt/ly finalize a 'Proxy' when another
    'P.Proxy' composed with it terminates first.  In those cases, finalization
    only occurs when 'runSafeIO' completes.  For example, given the following
    'P.Session':

> runSafeIO $ runProxy $ runExceptionK $ (p1 >-> p2) >=> p3

    ... when @(p1 >-> p2)@ terminates, 'runSafeIO' will not run any dropped
    finalizers until @p3@ completes.

    In practice, this is usually not a problem since most users will run
    linear 'P.Session's that look like this:

> runSafeIO $ runProxy $ runExceptionK $ p1 >-> p2 >-> ...

    Here, the end of composition coincides with the end of 'runSafeIO', so the
    implementation finalizes everything promptly.

    However, sometimes you know for certain that upstream or downstream
    'P.Proxy's are unreachable and would like to finalize them immediately to
    conserve resources.  The following functions let you promptly finalize all
    resources in either direction, but they are unsafe because you most prove
    that those resources are unreachable.
-}

{-| 'unsafeCloseU' calls all finalizers registered upstream of the current
    'P.Proxy'.  You can only safely call this function if the surrounding monad
    never 'P.request's again and never terminates. -}
unsafeCloseU :: (P.Proxy p) => ExceptionP p a' a b' b SafeIO ()
unsafeCloseU = do
    (huRef, hu) <- lift $ SafeIO $ do
        huRef <- asks upstream
        hu    <- lift $ readIORef huRef
        return (huRef, hu)
    try hu
    lift $ liftIO $ writeIORef huRef (return ())

{-| 'unsafeCloseD' calls all finalizers registered downstream of the current
    'P.Proxy'.  You can only safely call this function if the surrounding monad
    never 'P.respond's again and never terminates. -}
unsafeCloseD :: (P.Proxy p) => ExceptionP p a' a b' b SafeIO ()
unsafeCloseD = do
    (hdRef, hd) <- lift $ SafeIO $ do
        hdRef <- asks upstream
        hd    <- lift $ readIORef hdRef
        return (hdRef, hd)
    try hd
    lift $ liftIO $ writeIORef hdRef (return ())

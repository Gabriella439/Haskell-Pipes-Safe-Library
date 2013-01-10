-- | Resource acquisition integrated with proxies

{-# LANGUAGE Rank2Types, CPP, KindSignatures #-}

module Control.Proxy.Safe (
    -- * Safe IO
    SafeIO,
    runSafeIO,

    -- * ExceptionP
    -- $exceptionp
    ExceptionP,
    module Control.Proxy.Trans.Either,

    -- * Checked Exceptions
    CheckP(..),
    tryK,
    tryIO,
    tryIOK,

    -- * Checked Exception handling
    throw,
    catch,
    handle,

    -- * Finalization
    onException,
    bracket,

    -- * Prompt Finalization
    -- $prompt
    unsafeCloseU,
    unsafeCloseD
    ) where

import qualified Control.Exception as Ex
import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT(ReaderT, runReaderT), asks)
import qualified Control.Proxy as P
import qualified Control.Proxy.Core.Fast as PF
import qualified Control.Proxy.Core.Correct as PC
import Control.Proxy ((>->))
import qualified Control.Proxy.Trans.Identity as I
import qualified Control.Proxy.Trans.Either as E
import Control.Proxy.Trans.Either hiding (throw, catch, handle)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
#if MIN_VERSION_base(4,6,0)
#else
import Prelude hiding (catch)
#endif

data Status = Status {
    restore    :: forall a . IO a -> IO a,
    upstream   :: IORef (IO ())          ,
    downstream :: IORef (IO ())          }

{-| 'SafeIO' extends the 'IO' monad with the ability to selectively unmask
    computations and store registered finalizers
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
    end

    This masks asynchronous exceptions and only unmasks them during a 'try'.
-}
runSafeIO :: SafeIO r -> IO r
runSafeIO m =
    Ex.mask $ \restore -> do
        huRef <- newIORef (return ())
        hdRef <- newIORef (return ())
        runReaderT (unSafeIO m) (Status restore huRef hdRef) `Ex.finally` (do
            hu <- readIORef huRef
            hu
            hd <- readIORef hdRef
            hd )

{- 'registerK' should satisfy the following laws:

* 'registerK' defines a functor from finalizers to functions

> registerK nat m1 . registerK nat m2 = registerK nat (m1 >> m2)
> 
> registerK nat (return ()) = id

* 'registerK' is a functor between Kleisli categories

> registerK nat m (p1 >=> p2) = registerK nat m p1 >=> registerK nat m p2
>
> registerK nat m return = return

    These laws are not provable using the current set of proxy laws, mainly
    because the proxy laws do not yet specify how proxies interact with the
    'Arrow' instance for the Kleisli category.  However, I'm reasonably sure
    that when I do specify this interaction that the above laws will hold.

    For now, just consider the above laws the contract for 'registerK' and
    consider any violations of the above laws as bugs.
-}
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
                hu <- lift $ SafeIO $ lift $ do
                    hu <- readIORef huRef
                    writeIORef huRef (hu >> h)
                    return hu
                b' <- P.respond b
                lift $ SafeIO $ lift $ writeIORef huRef hu
                b2 <- P.request b'
                dn' b2
        dn' b0
    up a'0 = do
        hdRef <- lift $ SafeIO $ asks upstream
        let up' a' = do
                hd  <- lift $ SafeIO $ lift $ do
                    hd <- readIORef hdRef
                    writeIORef hdRef (h >> hd)
                    return hd
                a   <- P.request a'
                lift $ SafeIO $ lift $ writeIORef hdRef hd
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
   restructure the Proxy type class, but this will work for now. -}

{- $exceptionp
    'ExceptionP' is a type synonym around 'EitherP' from
    @Control.Proxy.Trans.Either@.  This means that you can use the more general
    'throw' and 'catch' functions for 'EitherP' if you prefer.

    Also, use 'runEitherP' or 'runEitherK' to convert it back to the base proxy.
-}

-- | A proxy transformer that stores exceptions using 'EitherP'
type ExceptionP = EitherP Ex.SomeException

{-| Proxies that support checked exceptions

    'try' is a monad morphism:

> try $ do x <- m
>          f x
> = do x <- try m
>      try (f x)
>
> try (return x) = return x
-}
class CheckP p where
    try :: p a' a b' b IO r -> ExceptionP p a' a b' b SafeIO r

{-| Check all exceptions for a 'P.Proxy' \'@K@\'leisli arrow

    @tryK = (try .)@, which defines a functor between Kleisli categories

> tryK (f >=> g) = tryK f >=> tryK g
>
> tryK return = return
-}
tryK
 :: (CheckP p) => (q -> p a' a b' b IO r)
 -> (q -> ExceptionP p a' a b' b SafeIO r)
tryK = (try .)

instance CheckP PF.ProxyFast where
    try p0 = EitherP (go p0) where
        go p = case p of
            PF.Request a' fa  -> PF.Request a' (\a  -> go (fa  a ))
            PF.Respond b  fb' -> PF.Respond b  (\b' -> go (fb' b'))
            PF.M m -> PF.M (SafeIO (ReaderT (\s -> do
                e <- Ex.try (restore s m)
                case e of
                    Left exc -> return (PF.Pure (Left exc))
                    Right p' -> return (go p') )))
            PF.Pure r -> PF.Pure (Right r)

instance CheckP PC.ProxyCorrect where
    try p0 = EitherP (go p0) where
        go p = PC.Proxy (SafeIO (ReaderT (\s -> do
            e <- Ex.try (restore s (PC.unProxy p))
            case e of
                Left exc -> return (PC.Pure (Left exc))
                Right fp -> case fp of
                    PC.Request a' fa  ->
                        return (PC.Request a' (\a  -> go (fa  a )))
                    PC.Respond b  fb' ->
                        return (PC.Respond b  (\b' -> go (fb' b')))
                    PC.Pure r -> return (PC.Pure (Right r)) )))

{-| Check all exceptions for an 'IO' action

> try $ do x <- m
>          f x
> = do x <- try m
>      try (f x)
>
> try (return x) = return x
-}
tryIO :: (P.Proxy p) => IO r -> ExceptionP p a' a b' b SafeIO r
tryIO io = EitherP $ P.runIdentityP $ lift $ SafeIO $ ReaderT $ \s ->
    Ex.try $ restore s io

{-| Check all exceptions for an 'IO' \'@K@\'leisli arrow

> tryIOK (f >=> g) = tryIOK f >=> tryIOK g
>
> tryIOK return = return
-}
tryIOK :: (P.Proxy p) => (q -> IO r) -> (q -> ExceptionP p a' a b' b SafeIO r)
tryIOK = (tryIO .)

-- | Analogous to 'Ex.throwIO' from @Control.Exception@
throw :: (Monad m, P.Proxy p, Ex.Exception e) => e -> ExceptionP p a' a b' b m r
throw = E.throw . Ex.toException

-- | Analogous to 'Ex.catch' from @Control.Exception@
catch
 :: (Ex.Exception e, Monad m, P.Proxy p)
 => ExceptionP p a' a b' b m r         -- ^ Original computation
 -> (e -> ExceptionP p a' a b' b m r)  -- ^ Handler
 -> ExceptionP p a' a b' b m r
catch p f = p `E.catch` (\someExc ->
    case Ex.fromException someExc of
        Nothing -> E.throw someExc
        Just e  -> f e )

-- | Analogous to 'Ex.handle' from @Control.Exception@
handle
 :: (Ex.Exception e, Monad m, P.Proxy p)
 => (e -> ExceptionP p a' a b' b m r)  -- ^ Handler
 -> ExceptionP p a' a b' b m r         -- ^ Original computation
 -> ExceptionP p a' a b' b m r
handle = flip catch

{-| Analogous to 'Ex.onException' from @Control.Exception@, except this also
    protects against premature termination.

    The first argument lifts 'onException' to work with any base monad.  Use
    'id' if your base monad is already 'SafeIO'.
-}
onException
 :: (Monad m, P.Proxy p)
 => (forall x . SafeIO x -> m x)  -- ^ Monad morphism
 -> IO r'                         -- ^ Action run on exception
 -> ExceptionP p a' a b' b m r    -- ^ Guarded computation
 -> ExceptionP p a' a b' b m r
onException nat after p =
    register nat (after >> return ()) p
        `E.catch` (\e -> do
            P.hoist nat $ tryIO after
            E.throw e )

{-| Analogous to 'Ex.bracket' from @Control.Exception@

    The first argument lifts 'bracket' to work with any base monad.  Use 'id' if
    your base monad is already 'SafeIO'.

    'bracket' guarantees that the acquired resource is finalized, even if:

    * Another 'P.Proxy' terminates before the computation completes

    * An exception is thrown anywhere, even if uncaught
-}
bracket
 :: (Monad m, P.Proxy p)
 => (forall x . SafeIO x -> m x)       -- ^ Monad morphism
 -> IO h                               -- ^ Acquire resource
 -> (h -> IO r')                       -- ^ Release resource
 -> (h -> ExceptionP p a' a b' b m r)  -- ^ Use resource
 -> ExceptionP p a' a b' b m r
bracket nat before after p = do
    h <- P.hoist nat $ tryIO before
    let finalizer = after h
    r <- register nat (finalizer >> return ()) (p h)
        `E.catch` (\e -> do
            P.hoist nat $ tryIO finalizer
            E.throw e )
    P.hoist nat $ tryIO finalizer
    return r

{- $prompt
    This implementation does not /promptly/ finalize a 'Proxy' when another
    'P.Proxy' composed with it terminates first.  In those cases, finalization
    only occurs when 'runSafeIO' completes.  For example, given the following
    'P.Session':

> runSafeIO $ runProxy $ runEitherK $ (p1 >-> p2) >=> p3

    ... when @(p1 >-> p2)@ terminates, 'runSafeIO' will not run any dropped
    finalizers until @p3@ completes.

    In practice, this is usually not a problem since most users will run
    linear 'P.Session's that look like this:

> runSafeIO $ runProxy $ runEitherK $ p1 >-> p2 >-> ...

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
    tryIO hu
    lift $ SafeIO $ lift $ writeIORef huRef (return ())

{-| 'unsafeCloseD' calls all finalizers registered downstream of the current
    'P.Proxy'.  You can only safely call this function if the surrounding monad
    never 'P.respond's again and never terminates. -}
unsafeCloseD :: (P.Proxy p) => ExceptionP p a' a b' b SafeIO ()
unsafeCloseD = do
    (hdRef, hd) <- lift $ SafeIO $ do
        hdRef <- asks upstream
        hd    <- lift $ readIORef hdRef
        return (hdRef, hd)
    tryIO hd
    lift $ SafeIO $ lift $ writeIORef hdRef (return ())

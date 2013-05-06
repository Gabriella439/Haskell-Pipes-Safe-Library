-- | Exception handling and resource management integrated with proxies

{-# LANGUAGE Rank2Types, CPP #-}

module Control.Proxy.Safe.Core (
    -- * Exception Handling
    -- $exceptionp
    module Control.Proxy.Trans.Either,
    module Control.Exception,
    ExceptionP,
    throw,
    catch,
    handle,

    -- * Safe IO
    SafeIO,
    runSafeIO,
    runSaferIO,
    trySafeIO,
    trySaferIO,

    -- * Checked Exceptions
    -- $check
    CheckP(..),
    tryK,
    tryIO,

    -- * Finalization
    onAbort,
    finally,
    bracket,
    bracket_,
    bracketOnAbort,

    -- * Prompt Finalization
    -- $prompt
    unsafeCloseU,
    unsafeCloseD,
    unsafeClose
    ) where

import qualified Control.Exception as Ex
import Control.Exception (SomeException, Exception)
import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT(ReaderT, runReaderT), asks)
import qualified Control.Proxy as P
import qualified Control.Proxy.Core.Fast as PF
import qualified Control.Proxy.Core.Correct as PC
import Control.Proxy ((->>), (>>~))
import qualified Control.Proxy.Trans.Either as E
import Control.Proxy.Trans.Either hiding (throw, catch, handle)
import qualified Control.Proxy.Trans.Reader as R
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
#if MIN_VERSION_base(4,6,0)
#else
import Prelude hiding (catch)
#endif
import System.IO.Error (userError)

{- $exceptionp
    This library checks and stores all exceptions using the 'EitherP' proxy
    transformer.  The 'ExceptionP' type synonym simplifies type signatures.

    Use 'runEitherP' / 'runEitherK' from the re-exported
    @Control.Proxy.Trans.Either@ to convert 'ExceptionP' back to the base monad.

    This module does not re-export 'E.throw', 'E.catch', and 'E.handle' from
    @Control.Proxy.Trans.Either@ and instead defines new versions similar to the
    API from @Control.Exception@.  If you want the old versions you will need to
    import them qualified.

    This module only re-exports 'SomeException' and 'Exception' from
    @Control.Exception@.
-}

-- | A proxy transformer that stores exceptions using 'EitherP'
type ExceptionP = EitherP SomeException

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

{- I don't export 'register' only because people rarely want to guard solely
   against premature termination.  Usually they also want to guard against
   exceptions, too.

    @registerK = (register .)@ should satisfy the following laws:

* 'registerK' defines a functor from finalizers to functions:

> registerK morph m1 . registerK morph m2 = registerK morph (m2 >> m1)
> 
> registerK morph (return ()) = id

* 'registerK' is a functor between Kleisli categories:

> registerK morph m (p1 >=> p2) = registerK morph m p1 >=> registerK morph m p2
>
> registerK morph m return = return

    These laws are not provable using the current set of proxy laws, mainly
    because the proxy laws do not yet specify how proxies interact with the
    'Arrow' instance for the Kleisli category.  However, I'm reasonably sure
    that when I do specify this interaction that the above laws will hold.

    For now, just consider the above laws the contract for 'register' and
    consider any violations of the above laws as bugs.
-}
register
    :: (Monad m, P.Proxy p)
    => (forall x . SafeIO x -> m x)
    -> IO ()
    -> p a' a b' b m r
    -> p a' a b' b m r
register morph h k =
    P.runIdentityP . P.hoist morph . up
    ->> k
    >>~ P.runIdentityP . P.hoist morph . dn
  where
    dn b0 = do
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
                    writeIORef hdRef (hd >> h)
                    return hd
                a   <- P.request a'
                lift $ SafeIO $ lift $ writeIORef hdRef hd
                a'2 <- P.respond a
                up' a'2
        up' a'0

{- $check
    The following @try@ functions are the only way to convert 'IO' actions to
    'SafeIO'.  These functions check all exceptions, including asynchronous
    exceptions, and store them in the 'ExceptionP' proxy transformer.
-}

{-| Use 'try' to retroactively check all exceptions for proxies that implement
    'CheckP'.

    'try' is /almost/ a proxy morphism (See @Control.Proxy.Morph@ from @pipes@
    for the full list of laws).  The only exception is the following law:

> try (return x) = return x

    The left-hand side unmasks asynchronous exceptions and checks them
    immediately, whereas the right-hand side delays asynchronous exceptions
    until the next 'try' or 'tryIO' block.
-}
class (P.Proxy p) => CheckP p where
    try :: p a' a b' b IO r -> ExceptionP p a' a b' b SafeIO r

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

instance (CheckP p) => CheckP (P.IdentityP p) where
    try = EitherP . P.IdentityP . runEitherP . try . P.runIdentityP

instance (CheckP p) => CheckP (R.ReaderP i p) where
    try p = EitherP $ R.ReaderP $ \i -> runEitherP $ try (R.unReaderP p i)

-- | Check all exceptions for a 'P.Proxy' \'@K@\'leisli arrow
tryK
    :: (CheckP p)
    => (q -> p a' a b' b IO r) -> (q -> ExceptionP p a' a b' b SafeIO r)
tryK = (try .)

{-| Check all exceptions for an 'IO' action

    'tryIO' is a monad morphism, with the same caveat as 'try'.
-}
tryIO :: (P.Proxy p) => IO r -> ExceptionP p a' a b' b SafeIO r
tryIO io = EitherP $ P.runIdentityP $ lift $ SafeIO $ ReaderT $ \s ->
    Ex.try $ restore s io

{-| Similar to 'Ex.onException' from @Control.Exception@, except this also
    protects against:

    * premature termination, and

    * exceptions in other proxy stages.

    The first argument lifts 'onAbort' to work with other base monads.  Use
    'id' if your base monad is already 'SafeIO'.

    @(onAbort morph fin)@ is a monad morphism:

> onAbort morph fin $ do x <- m  =  do x <- onAbort morph fin m
>                        f x           onAbort morph fin (f x)
>
> onAbort morph fin (return x) = return x

    'onAbort' ensures finalizers are called from inside to out:

> onAbort morph fin1 . onAbort morph fin2 = onAbort morph (fin2 >> fin1)
>
> onAbort morph (return ()) = id
-}
onAbort
    :: (Monad m, P.Proxy p)
    => (forall x . SafeIO x -> m x)  -- ^ Monad morphism
    -> IO r'                         -- ^ Action to run on abort
    -> ExceptionP p a' a b' b m r    -- ^ Guarded computation
    -> ExceptionP p a' a b' b m r
onAbort morph after p =
    register morph (after >> return ()) p
        `E.catch` (\e -> do
            P.hoist morph $ tryIO after
            E.throw e )

{-| Analogous to 'Ex.finally' from @Control.Exception@

    The first argument lifts 'finally' to work with other base monads.  Use 'id'
    if your base monad is already 'SafeIO'.

> finally morph after p = do
>     r <- onAbort morph after p
>     hoist morph $ tryIO after
>     return r
-}
finally
    :: (Monad m, P.Proxy p)
    => (forall x . SafeIO x -> m x) -- ^ Monad morphism
    -> IO r'                        -- ^ Guaranteed final action
    -> ExceptionP p a' a b' b m r   -- ^ Guarded computation
    -> ExceptionP p a' a b' b m r
finally morph after p = do
    r <- onAbort morph after p
    P.hoist morph $ tryIO after
    return r

{-| Analogous to 'Ex.bracket' from @Control.Exception@

    The first argument lifts 'bracket' to work with other base monads.  Use 'id'
    if your base monad is already 'SafeIO'.

    'bracket' guarantees that if the resource acquisition completes, then the
    resource will be released.

> bracket morph before after p = do
>     h <- hoist morph $ tryIO before
>     finally morph (after h) (p h)
-}
bracket
    :: (Monad m, P.Proxy p)
    => (forall x . SafeIO x -> m x)       -- ^ Monad morphism
    -> IO h                               -- ^ Acquire resource
    -> (h -> IO r')                       -- ^ Release resource
    -> (h -> ExceptionP p a' a b' b m r)  -- ^ Use resource
    -> ExceptionP p a' a b' b m r
bracket morph before after p = do
    h <- P.hoist morph $ tryIO before
    finally morph (after h) (p h)

{-| Analogous to 'Ex.bracket_' from @Control.Exception@

    The first argument lifts 'bracket_' to work with any base monad.  Use 'id'
    if your base monad is already 'SafeIO'.

> bracket_ morph before after p = do
>     hoist morph $ tryIO before
>     finally morph after p
-}
bracket_
    :: (Monad m, P.Proxy p)
    => (forall x . SafeIO x -> m x)  -- ^ Monad morphism
    -> IO r1                         -- ^ Acquire resource
    -> IO r2                         -- ^ Release resource
    -> ExceptionP p a' a b' b m r    -- ^ Use resource
    -> ExceptionP p a' a b' b m r
bracket_ morph before after p = do
    P.hoist morph $ tryIO before
    finally morph after p

{-| Analogous to 'Ex.bracketOnError' from @Control.Exception@

    The first argument lifts 'bracketOnAbort' to work with any base monad.  Use
    'id' if your base monad is already 'SafeIO'.

> bracketOnAbort morph before after p = do
>     h <- hoist morph $ tryIO before
>     onAbort morph (after h) (p h)
-}
bracketOnAbort
    :: (Monad m, P.Proxy p)
    => (forall x . SafeIO x -> m x)       -- ^ Monad morphism
    -> IO h                               -- ^ Acquire resource
    -> (h -> IO r')                       -- ^ Release resource
    -> (h -> ExceptionP p a' a b' b m r)  -- ^ Use resource
    -> ExceptionP p a' a b' b m r
bracketOnAbort morph before after p = do
    h <- P.hoist morph $ tryIO before
    onAbort morph (after h) (p h)

{- $prompt
    This implementation will not /promptly/ finalize a 'P.Proxy' if another
    composed 'P.Proxy' prematurely terminates.  However, the implementation will
    still save the dropped finalizer and run it when the 'P.Session' completes
    in order to guarantee deterministic finalization.

    To see why, consider the following 'P.Proxy' assembly:

> p1 >-> ((p2 >-> p3) >=> p4)

    Now ask yourself the question, \"If @p3@ prematurely terminates, should it
    promptly finalize @p1@?\"

    If you answered \"yes\", then you would have a bug if @p4@ were to
    'request', which would restore control to @p1@ after we already finalized
    it.

    If you answered \"no\", then consider the case where @p2 = idT@ and
    @p4 = return@:

> p1 >-> ((idT >-> p3) >=> return)
> p1 >-> (idT >-> p3)               -- f   >=> return = f
> p1 >-> p3                         -- idT >-> p      = p

    Answering \"no\" means that @p3@ would be unable to promptly finalize a
    'P.Proxy' immediately upstream of it.

    There is a solution that permits perfectly prompt finalization, but it
    requires indexed monads to guarantee the necessary safety through the type
    system.  In the absence of indexed monads, the next best solution is to let
    you promptly finalize things yourself, but then /you/ must prove that this
    finalization is safe and that all upstream pipes are unreachable.

    The following two unsafe operations allow you to trade safety for prompt
    finalization.  Use them if you desire prompter finalization guarantees and
    if you can prove their usage is safe.  However, this proof is not trivial.

    For example, you might suppose that the following usage of 'unsafeCloseU' is
    safe because it never 'request's after closing upstream, nor does it
    terminate:

> falseSenseOfSecurity () = do
>     x <- request ()
>     unsafeCloseU ()
>     forever $ respond x

    However, this is not safe, as the following counter-example demonstrates:

> p1 >-> ((falseSenseOfSecurity >-> request) >=> request)

    @falseSenseOfSecurity@ will finalize the upstream @p1@, but then will
    abort when the downstream 'request' terminates, and then the second
    'request' will illegally access @p1@ after we already finalized it.

    In other words, you cannot prove any prompt finalization is safe unless you
    control the entire 'P.Session'.  Therefore, do not use the following unsafe
    operations in 'P.Proxy' libraries.  Only the end user assembling the
    final 'P.Session' may safely insert these calls.
-}

{-| 'unsafeCloseU' calls all finalizers registered upstream of the current
    'P.Proxy'.
-}
unsafeCloseU :: (P.Proxy p) => r -> ExceptionP p a' a b' b SafeIO r
unsafeCloseU r = do
    (huRef, hu) <- lift $ SafeIO $ do
        huRef <- asks upstream
        hu    <- lift $ readIORef huRef
        return (huRef, hu)
    tryIO hu
    lift $ SafeIO $ lift $ writeIORef huRef (return ())
    return r

{-| 'unsafeCloseD' calls all finalizers registered downstream of the current
    'P.Proxy'.
-}
unsafeCloseD :: (P.Proxy p) => r -> ExceptionP p a' a b' b SafeIO r
unsafeCloseD r = do
    (hdRef, hd) <- lift $ SafeIO $ do
        hdRef <- asks downstream
        hd    <- lift $ readIORef hdRef
        return (hdRef, hd)
    tryIO hd
    lift $ SafeIO $ lift $ writeIORef hdRef (return ())
    return r

{-| 'unsafeClose' calls all registered finalizers

    'unsafeClose' is a Kleisli arrow so that you can easily seal terminating
    proxies if there is a risk of delayed finalization:

> (producer >-> (takeB_ 10 >=> unsafeClose) >-> consumer) >=> later
-}
unsafeClose :: (P.Proxy p) => r -> ExceptionP p a' a b' b SafeIO r
unsafeClose = unsafeCloseU P.>=> unsafeCloseD

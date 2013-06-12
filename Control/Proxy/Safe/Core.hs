-- | Exception handling and resource management integrated with proxies

{-# LANGUAGE CPP #-}

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
    MonadSafeIO,
    SafeIO,
    runSafeIO,
    runSaferIO,
    trySafeIO,
    trySaferIO,

    -- * Checked Exceptions
    -- $check
    CheckP(..),
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
    unsafeClose,

    -- * Deprecated
    -- $deprecated
    tryK
    ) where

import qualified Control.Exception as Ex
import Control.Exception (SomeException)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT(ReaderT), asks)
import qualified Control.Proxy as P
import qualified Control.Proxy.Core.Fast as PF
import qualified Control.Proxy.Core.Correct as PC
import Control.Proxy ((->>), (>>~))
import Control.Proxy.Safe.SafeIO
import qualified Control.Proxy.Trans.Either as E
import Control.Proxy.Trans.Either hiding (throw, catch, handle)
import qualified Control.Proxy.Trans.Reader as R
import Data.IORef (IORef, readIORef, writeIORef)
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



{- I don't export 'register' only because people rarely want to guard solely
   against premature termination.  Usually they also want to guard against
   exceptions, too.

    @registerK = (register .)@ should satisfy the following laws:

* 'registerK' defines a functor from finalizers to functions:

> registerK m1 . registerK m2 = registerK (m2 >> m1)
> 
> registerK (return ()) = id

* 'registerK' is a functor between Kleisli categories:

> registerK m (p1 >=> p2) = registerK m p1 >=> registerK m p2
>
> registerK m return = return

    These laws are not provable using the current set of proxy laws, mainly
    because the proxy laws do not yet specify how proxies interact with the
    'Arrow' instance for the Kleisli category.  However, I'm reasonably sure
    that when I do specify this interaction that the above laws will hold.

    For now, just consider the above laws the contract for 'register' and
    consider any violations of the above laws as bugs.
-}
register
    :: (MonadSafeIO m, P.Proxy p)
    => IO ()
    -> p a' a b' b m r
    -> p a' a b' b m r
register h k =
    P.runIdentityP . P.hoist liftSafeIO . up
    ->> k
    >>~ P.runIdentityP . P.hoist liftSafeIO . dn
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

{-| Check all exceptions for an 'IO' action

    'tryIO' is a monad morphism, with the same caveat as 'try'.
-}
tryIO :: (P.Proxy p) => IO r -> ExceptionP p a' a b' b SafeIO r
tryIO io = EitherP $ P.runIdentityP $ lift $ SafeIO $ ReaderT $ \s ->
    Ex.try $ restore s io


-- This idiom popped up a few times, so I named it
safely :: (MonadSafeIO m, P.Proxy p) => IO r -> ExceptionP p a' a b' b m r
safely m = P.hoist liftSafeIO $ tryIO m
{-# INLINE safely #-}

{-| Similar to 'Ex.onException' from @Control.Exception@, except this also
    protects against:

    * premature termination, and

    * exceptions in other proxy stages.

    The first argument lifts 'onAbort' to work with other base monads.  Use
    'id' if your base monad is already 'SafeIO'.

    @(onAbort fin)@ is a monad morphism:

> onAbort fin $ do x <- m  =  do x <- onAbort fin m
>                  f x           onAbort fin (f x)
>
> onAbort fin (return x) = return x

    'onAbort' ensures finalizers are called from inside to out:

> onAbort fin1 . onAbort fin2 = onAbort (fin2 >> fin1)
>
> onAbort (return ()) = id
-}
onAbort
    :: (MonadSafeIO m, P.Proxy p)
    => IO r'                         -- ^ Action to run on abort
    -> ExceptionP p a' a b' b m r    -- ^ Guarded computation
    -> ExceptionP p a' a b' b m r
onAbort after p =
    register (after >> return ()) p
        `E.catch` (\e -> do
            safely after
            E.throw e )

{-| Analogous to 'Ex.finally' from @Control.Exception@

    The first argument lifts 'finally' to work with other base monads.  Use 'id'
    if your base monad is already 'SafeIO'.

> finally after p = do
>     r <- onAbort after p
>     hoist liftSafeIO $ tryIO after
>     return r
-}
finally
    :: (MonadSafeIO m, P.Proxy p)
    => IO r'                        -- ^ Guaranteed final action
    -> ExceptionP p a' a b' b m r   -- ^ Guarded computation
    -> ExceptionP p a' a b' b m r
finally after p = do
    r <- onAbort after p
    safely after
    return r

{-| Analogous to 'Ex.bracket' from @Control.Exception@

    The first argument lifts 'bracket' to work with other base monads.  Use 'id'
    if your base monad is already 'SafeIO'.

    'bracket' guarantees that if the resource acquisition completes, then the
    resource will be released.

> bracket before after p = do
>     h <- hoist liftSafeIO $ tryIO before
>     finally (after h) (p h)
-}
bracket
    :: (MonadSafeIO m, P.Proxy p)
    => IO h                               -- ^ Acquire resource
    -> (h -> IO r')                       -- ^ Release resource
    -> (h -> ExceptionP p a' a b' b m r)  -- ^ Use resource
    -> ExceptionP p a' a b' b m r
bracket before after p = do
    h <- safely before
    finally (after h) (p h)

{-| Analogous to 'Ex.bracket_' from @Control.Exception@

    The first argument lifts 'bracket_' to work with any base monad.  Use 'id'
    if your base monad is already 'SafeIO'.

> bracket_ before after p = do
>     hoist liftSafeIO $ tryIO before
>     finally after p
-}
bracket_
    :: (MonadSafeIO m, P.Proxy p)
    => IO r1                         -- ^ Acquire resource
    -> IO r2                         -- ^ Release resource
    -> ExceptionP p a' a b' b m r    -- ^ Use resource
    -> ExceptionP p a' a b' b m r
bracket_ before after p = do
    safely before
    finally after p

{-| Analogous to 'Ex.bracketOnError' from @Control.Exception@

    The first argument lifts 'bracketOnAbort' to work with any base monad.  Use
    'id' if your base monad is already 'SafeIO'.

> bracketOnAbort before after p = do
>     h <- hoist liftSafeIO $ tryIO before
>     onAbort (after h) (p h)
-}
bracketOnAbort
    :: (MonadSafeIO m, P.Proxy p)
    => IO h                               -- ^ Acquire resource
    -> (h -> IO r')                       -- ^ Release resource
    -> (h -> ExceptionP p a' a b' b m r)  -- ^ Use resource
    -> ExceptionP p a' a b' b m r
bracketOnAbort before after p = do
    h <- safely before
    onAbort (after h) (p h)

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

{- $deprecated
    To be removed in version @2.0.0@
-}

tryK
    :: (CheckP p)
    => (q -> p a' a b' b IO r) -> (q -> ExceptionP p a' a b' b SafeIO r)
tryK = (try .)
{-# DEPRECATED tryK "Use '(try .)' instead" #-}

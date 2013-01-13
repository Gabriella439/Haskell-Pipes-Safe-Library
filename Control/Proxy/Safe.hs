-- | Exception handling and resource management integrated with proxies

{-# LANGUAGE Rank2Types, CPP, KindSignatures #-}

module Control.Proxy.Safe (
    -- * Safe IO
    SafeIO,
    runSafeIO,
    runSaferIO,

    -- * ExceptionP
    -- $exceptionp
    ExceptionP,
    module Control.Proxy.Trans.Either,

    -- * Checked Exceptions
    CheckP(..),
    tryK,
    tryIO,

    -- * Exception handling
    throw,
    catch,
    handle,

    -- * Finalization
    onAbort,
    finally,
    bracket,
    bracket_,
    bracketOnAbort,

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

    'SafeIO' purposefully does not implement 'MonadIO'.  Use 'tryIO' to safely
    introduce 'IO' actions.
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

    This uses 'Ex.mask' to mask asynchronous exceptions and only unmasks them
    during 'try' or 'tryIO'.
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

{-| Convert back to the 'IO' monad, running all dropped finalizers at the very
    end

    This uses 'Ex.uninterruptibleMask' to mask asynchronous exceptions and only
    unmasks them during 'try' or 'tryIO'.
-}
runSaferIO :: SafeIO r -> IO r
runSaferIO m =
    Ex.uninterruptibleMask $ \restore -> do
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

{-| You can retroactively check all exceptions for proxies that implement
    'CheckP'.

    'try' is a both a monad morphism and a proxy morphism, which means that
    @tryK = (try .)@ defines a functor that preserves five categories.

    Functor between \'@K@\'leisli categories:

> tryK f >=> tryK g = tryK (f >=> g)
>
> tryK return = return

    Functor between 'P.Proxy' categories:

> tryK f >-> tryK g = tryK (f >-> g)
>
> tryK idT = idT

> tryK f >~> tryK g = tryK (f >~> g)
> 
> tryK coidT = coidT

    Functor between \"request\" categories:

> tryK f \>\ tryK g = tryK (f \>\ g)
>
> tryK request = request

    Functor between \"respond\" categories:

> tryK f />/ tryK g = tryK (f />/ g)
>
> tryK respond = respond
-}
class CheckP p where
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

-- | Check all exceptions for a 'P.Proxy' \'@K@\'leisli arrow
tryK
 :: (CheckP p)
 => (q -> p a' a b' b IO r) -> (q -> ExceptionP p a' a b' b SafeIO r)
tryK = (try .)

{-| Check all exceptions for an 'IO' action

> (tryIO .) f >=> (tryIO .) g = (tryIO .) (f >=> g)
>
> (tryIO .) return = return
-}
tryIO :: (P.Proxy p) => IO r -> ExceptionP p a' a b' b SafeIO r
tryIO io = EitherP $ P.runIdentityP $ lift $ SafeIO $ ReaderT $ \s ->
    Ex.try $ restore s io

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

{-| Similar to 'Ex.onException' from @Control.Exception@, except this also
    protects against:

    * premature termination, and

    * exceptions in other proxy stages.

    The first argument lifts 'onAbort' to work with other base monads.  Use
    'id' if your base monad is already 'SafeIO'.

> do x <- onAbort nat fin m
>    onAbort nat fin (f x)
> = onAbort nat fin $ do x <- m
>                            f x
>
> onAbort (return x) = return x

> onAbort nat fin1 . onAbort nat fin2 = onAbort nat (fin2 >> fin1)
>
> onAbort nat (return ()) = id
-}
onAbort
 :: (Monad m, P.Proxy p)
 => (forall x . SafeIO x -> m x)  -- ^ Monad morphism
 -> IO r'                         -- ^ Action to run on abort
 -> ExceptionP p a' a b' b m r    -- ^ Guarded computation
 -> ExceptionP p a' a b' b m r
onAbort nat after p =
    register nat (after >> return ()) p
        `E.catch` (\e -> do
            P.hoist nat $ tryIO after
            E.throw e )

{-| Analogous to 'Ex.finally' from @Control.Exception@

    The first argument lifts 'finally' to work with other base monads.  Use 'id'
    if your base monad is already 'SafeIO'.

> finally nat after p = do
>     r <- onAbort nat after p
>     P.hoist nat $ tryIO after
>     return r
-}
finally
 :: (Monad m, P.Proxy p)
 => (forall x . SafeIO x -> m x) -- ^ Monad morphism
 -> IO r'                        -- ^ Guaranteed final action
 -> ExceptionP p a' a b' b m r   -- ^ Guarded computation
 -> ExceptionP p a' a b' b m r
finally nat after p = do
    r <- onAbort nat after p
    P.hoist nat $ tryIO after
    return r

{-| Analogous to 'Ex.bracket' from @Control.Exception@

    The first argument lifts 'bracket' to work with other base monads.  Use 'id'
    if your base monad is already 'SafeIO'.

    'bracket' guarantees that if the resource acquisition completes, then the
    resource will be released.

> bracket nat before after p = do
>     h <- P.hoist nat $ tryIO before
>     let finalizer = after h
>     finally nat finalizer (p h)
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
    finally nat finalizer (p h)

{-| Analogous to 'Ex.bracket_' from @Control.Exception@

    The first argument lifts 'bracket_' to work with any base monad.  Use 'id'
    if your base monad is already 'SafeIO'.

> bracket_ nat before after p = do
>     P.hoist nat $ tryIO before
>     finally nat after p
-}
bracket_
 :: (Monad m, P.Proxy p)
 => (forall x . SafeIO x -> m x)  -- ^ Monad morphism
 -> IO r1                         -- ^ Acquire resource
 -> IO r2                         -- ^ Release resource
 -> ExceptionP p a' a b' b m r    -- ^ Use resource
 -> ExceptionP p a' a b' b m r
bracket_ nat before after p = do
    P.hoist nat $ tryIO before
    finally nat after p

{-| Analogous to 'Ex.bracketOnAbort' from @Control.Exception@

    The first argument lifts 'bracketOnAbort' to work with any base monad.  Use
    'id' if your base monad is already 'SafeIO'.

> bracketOnAbort nat before after p = do
>     h <- P.hoist nat $ tryIO before
>     let finalizer = after h
>     onAbort nat finalizer (p h)
-}
bracketOnAbort
 :: (Monad m, P.Proxy p)
 => (forall x . SafeIO x -> m x)       -- ^ Monad morphism
 -> IO h                               -- ^ Acquire resource
 -> (h -> IO r')                       -- ^ Release resource
 -> (h -> ExceptionP p a' a b' b m r)  -- ^ Use resource
 -> ExceptionP p a' a b' b m r
bracketOnAbort nat before after p = do
    h <- P.hoist nat $ tryIO before
    let finalizer = after h
    onAbort nat finalizer (p h)


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
    'P.Proxy' immediately upstream of it.  More generally, the 'P.Proxy' laws
    and the 'Monad' laws say that we cannot distinguish between proxies that are
    \"directly\" or \"indirectly\" composed with a given 'P.Proxy'.

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
>     unsafeCloseU
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
    'P.Proxy'. -}
unsafeCloseU :: (P.Proxy p) => ExceptionP p a' a b' b SafeIO ()
unsafeCloseU = do
    (huRef, hu) <- lift $ SafeIO $ do
        huRef <- asks upstream
        hu    <- lift $ readIORef huRef
        return (huRef, hu)
    tryIO hu
    lift $ SafeIO $ lift $ writeIORef huRef (return ())

{-| 'unsafeCloseD' calls all finalizers registered downstream of the current
    'P.Proxy'. -}
unsafeCloseD :: (P.Proxy p) => ExceptionP p a' a b' b SafeIO ()
unsafeCloseD = do
    (hdRef, hd) <- lift $ SafeIO $ do
        hdRef <- asks upstream
        hd    <- lift $ readIORef hdRef
        return (hdRef, hd)
    tryIO hd
    lift $ SafeIO $ lift $ writeIORef hdRef (return ())

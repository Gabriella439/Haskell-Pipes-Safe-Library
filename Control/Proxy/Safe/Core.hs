-- | Exception handling and resource management integrated with proxies

{-# LANGUAGE Rank2Types, CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Proxy.Safe.Core (
    -- * Exception Handling
    -- $exceptionp
    module Control.Proxy.Trans.Either,
    module Control.Exception,
    SafeP,
    throw,
    catch,
    handle,

    -- * Safe IO
    SafeIO,
    runSafeIO,

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
    ) where

import qualified Control.Exception as Ex
import Control.Exception (SomeException, Exception)
import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT(ReaderT, runReaderT), asks)
import qualified Control.Proxy as P
import qualified Control.Proxy.Core.Fast as PF
import qualified Control.Proxy.Core.Correct as PC
import Control.Proxy ((->>), (>>~), (?>=))
import qualified Control.Proxy.Trans.Either as E
import Control.Proxy.Trans.Either hiding (throw, catch, handle)
import qualified Control.Proxy.Trans.Maybe  as M
import qualified Control.Proxy.Trans.Reader as R
import qualified Control.Proxy.Trans.State  as S
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
#if MIN_VERSION_base(4,6,0)
#else
import Prelude hiding (catch)
#endif
import System.IO.Error (userError)

{- $exceptionp
    This module re-exports 'runEitherP' / 'runEitherK' from
    @Control.Proxy.Trans.Either@ so that you can further unwrap the result of
    'runSafeP'.

    This module does not re-export 'E.throw', 'E.catch', and 'E.handle' from
    @Control.Proxy.Trans.Either@, which would clash with the functions provided
    here.

    @Control.Exception@ re-exports 'SomeException' and 'Exception'.
-}

-- | Analogous to 'Ex.throwIO' from @Control.Exception@
throw :: (Monad m, P.Proxy p, Ex.Exception e) => e -> SafeP p a' a b' b m r
throw e = SafeP (E.throw (Ex.toException e))
{-# INLINABLE throw #-}

-- | Analogous to 'Ex.catch' from @Control.Exception@
catch
    :: (Ex.Exception e, Monad m, P.Proxy p)
    => SafeP p a' a b' b m r         -- ^ Original computation
    -> (e -> SafeP p a' a b' b m r)  -- ^ Handler
    -> SafeP p a' a b' b m r
catch p f = SafeP (unSafeP p `E.catch` (\someExc ->
    case Ex.fromException someExc of
        Nothing -> E.throw someExc
        Just e  -> unSafeP (f e) ))
{-# INLINABLE catch #-}

-- | Analogous to 'Ex.handle' from @Control.Exception@
handle
    :: (Ex.Exception e, Monad m, P.Proxy p)
    => (e -> SafeP p a' a b' b m r)  -- ^ Handler
    -> SafeP p a' a b' b m r         -- ^ Original computation
    -> SafeP p a' a b' b m r
handle = flip catch
{-# INLINABLE handle #-}

data Finalizers = Finalizers { upstream :: !(IO ()), downstream :: !(IO ()) }

newtype Mask = Mask { unMask :: forall a . IO a -> IO a }

{-| 'SafeIO' masks asynchronous exceptions by default and only unmasks them
    during 'try' or 'tryIO' blocks.  This ensures that all exceptions are
    checked.
-}
newtype SafeIO r = SafeIO { unSafeIO :: ReaderT Mask IO r }

instance Functor SafeIO where
    fmap f m = SafeIO (fmap f (unSafeIO m))

instance Applicative SafeIO where
    pure r  = SafeIO (pure r)
    f <*> x = SafeIO (unSafeIO f <*> unSafeIO x)

instance Monad SafeIO where
    return r = SafeIO (return r)
    m >>= f  = SafeIO (unSafeIO m >>= \a -> unSafeIO (f a))

{-| Runs a 'SafeIO' computation in a masked background.

    This uses 'Ex.mask' to mask asynchronous exceptions and only unmasks them
    during 'try' or 'tryIO'.

    'runSafeIO' is NOT a monad morphism.
-}
runSafeIO :: SafeIO r -> IO r
runSafeIO m = Ex.mask $ \unmask ->
    runReaderT (unSafeIO m) (Mask unmask)
{-# INLINABLE runSafeIO #-}

{-| Runs a 'SafeIO' computation in an uninterruptible masked background.

    This uses 'Ex.uninterruptibleMask' to mask asynchronous exceptions and only
    unmasks them during 'try' or 'tryIO'.

    'runSaferIO' is NOT a monad morphism.
-}
runSaferIO :: SafeIO r -> IO r
runSaferIO m = Ex.uninterruptibleMask $ \unmask ->
    runReaderT (unSafeIO m) (Mask unmask)
{-# INLINABLE runSaferIO #-}

newtype SafeP p a' a b' b m r
    = SafeP { unSafeP :: EitherP SomeException (S.StateP Finalizers p) a' a b' b m r }
    deriving (Functor, Applicative, Monad, P.ProxyInternal, P.Proxy, P.MFunctor)

runSafeP
    :: (Monad m, P.Proxy p)
    => (forall x . SafeIO x -> m x)
    -> SafeP p a' a b' b m r -> p a' a b' b m (Either SomeException r)
runSafeP morph p = P.runIdentityP $ do
    let s0 = Finalizers (return ()) (return ())
    (e, _) <- P.IdentityP $ S.runStateP s0 $ E.runEitherP $ unSafeP $ do
        r <- p
        s1 <- SafeP $ P.liftP S.get
        hoist morph $ tryIO $ do
            upstream   s1
            downstream s1
        return r
    return e

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
    :: (Monad m, P.Proxy p)
    => IO ()
    -> SafeP p a' a b' b m r
    -> SafeP p a' a b' b m r
register h k = up ->> k >>~ dn
  where
    dn b = do
        old <- SafeP $ P.liftP $ do
            old <- S.get
            S.put $! old { upstream = (upstream old >> h) }
            return old
        b' <- P.respond b
        SafeP $ P.liftP $ S.put $! old
        b2 <- P.request b'
        dn b2
    up a' = do
        old <- SafeP $ P.liftP $ do
            old <- S.get
            S.put $! old { downstream = (downstream old >> h) }
            return old
        a   <- P.request a'
        SafeP $ P.liftP $ S.put $! old
        a'2 <- P.respond a
        up a'2

{- $check
    The following @try@ functions are the only way to convert 'IO' actions to
    'SafeIO'.  These functions check all exceptions, including asynchronous
    exceptions, and store them in the 'SafeP' proxy transformer.
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
    try :: p a' a b' b IO r -> SafeP p a' a b' b SafeIO r

instance CheckP PF.ProxyFast where
    try p0 = SafeP (EitherP (S.StateP (P.thread_P (go p0)))) where
        go p = case p of
            PF.Request a' fa  -> PF.Request a' (\a  -> go (fa  a ))
            PF.Respond b  fb' -> PF.Respond b  (\b' -> go (fb' b'))
            PF.M m -> PF.M (SafeIO (ReaderT (\(Mask restore) -> do
                e <- Ex.try (restore m)
                case e of
                    Left exc -> return (PF.Pure (Left exc))
                    Right p' -> return (go p') )))
            PF.Pure r -> PF.Pure (Right r)

instance CheckP PC.ProxyCorrect where
    try p0 = SafeP (EitherP (S.StateP (P.thread_P (go p0)))) where
        go p = PC.Proxy (SafeIO (ReaderT (\(Mask restore) -> do
            e <- Ex.try (restore (PC.unProxy p))
            case e of
                Left exc -> return (PC.Pure (Left exc))
                Right fp -> case fp of
                    PC.Request a' fa  ->
                        return (PC.Request a' (\a  -> go (fa  a )))
                    PC.Respond b  fb' ->
                        return (PC.Respond b  (\b' -> go (fb' b')))
                    PC.Pure r -> return (PC.Pure (Right r)) )))

instance (CheckP p) => CheckP (P.IdentityP p) where
    try p = SafeP (E.EitherP (S.StateP (\s -> P.IdentityP (
        S.unStateP (E.runEitherP (unSafeP (try (P.runIdentityP p)))) s ))))

instance (CheckP p) => CheckP (R.ReaderP i p) where
    try p = SafeP (E.EitherP (S.StateP (\s -> R.ReaderP (\i ->
        S.unStateP (E.runEitherP (unSafeP (try (R.unReaderP p i)))) s ))))

instance (CheckP p) => CheckP (E.EitherP e p) where
    try p = SafeP (E.EitherP (S.StateP (\s -> E.EitherP (
        S.unStateP (E.runEitherP (unSafeP (try (E.runEitherP p)))) s ?>= \r ->
        P.return_P (munge r) ))))
      where
        munge :: (Either a (Either b r), s) -> Either b (Either a r, s)
        munge (e, s) = case e of
            Left  a  -> Right (Left a, s)
            Right e' -> case e' of
                Left  b -> Left b
                Right r -> Right (Right r, s)

instance (CheckP p) => CheckP (M.MaybeP p) where
    try p = SafeP (E.EitherP (S.StateP (\s -> M.MaybeP (
        S.unStateP (E.runEitherP (unSafeP (try (M.runMaybeP p)))) s ?>= \r ->
        P.return_P (munge r) ))))
      where
        munge :: (Either a (Maybe r), s) -> Maybe (Either a r, s)
        munge (e, s) = case e of
            Left  a -> Just (Left a, s)
            Right m -> case m of
                Nothing -> Nothing
                Just r  -> Just (Right r, s)

{-| Check all exceptions for an 'IO' action

    'tryIO' is a monad morphism, with the same caveat as 'try'.
-}
tryIO :: (P.Proxy p) => IO r -> SafeP p a' a b' b SafeIO r
tryIO io = SafeP $ EitherP $ lift $ SafeIO $ ReaderT $ \(Mask restore) ->
    Ex.try $ restore io
{-# INLINABLE tryIO #-}

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
    -> SafeP p a' a b' b m r         -- ^ Guarded computation
    -> SafeP p a' a b' b m r
onAbort morph after p =
    register (after >> return ()) p
        `catch` (\e -> do
            hoist morph $ tryIO after
            throw (e :: SomeException) )
{-# INLINABLE onAbort #-}

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
    -> SafeP p a' a b' b m r        -- ^ Guarded computation
    -> SafeP p a' a b' b m r
finally morph after p = do
    r <- onAbort morph after p
    hoist morph $ tryIO after
    return r
{-# INLINABLE finally #-}

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
    => (forall x . SafeIO x -> m x)  -- ^ Monad morphism
    -> IO h                          -- ^ Acquire resource
    -> (h -> IO r')                  -- ^ Release resource
    -> (h -> SafeP p a' a b' b m r)  -- ^ Use resource
    -> SafeP p a' a b' b m r
bracket morph before after p = do
    h <- hoist morph $ tryIO before
    finally morph (after h) (p h)
{-# INLINABLE bracket #-}

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
    -> SafeP p a' a b' b m r         -- ^ Use resource
    -> SafeP p a' a b' b m r
bracket_ morph before after p = do
    hoist morph $ tryIO before
    finally morph after p
{-# INLINABLE bracket_ #-}

{-| Analogous to 'Ex.bracketOnError' from @Control.Exception@

    The first argument lifts 'bracketOnAbort' to work with any base monad.  Use
    'id' if your base monad is already 'SafeIO'.

> bracketOnAbort morph before after p = do
>     h <- hoist morph $ tryIO before
>     onAbort morph (after h) (p h)
-}
bracketOnAbort
    :: (Monad m, P.Proxy p)
    => (forall x . SafeIO x -> m x)  -- ^ Monad morphism
    -> IO h                          -- ^ Acquire resource
    -> (h -> IO r')                  -- ^ Release resource
    -> (h -> SafeP p a' a b' b m r)  -- ^ Use resource
    -> SafeP p a' a b' b m r
bracketOnAbort morph before after p = do
    h <- hoist morph $ tryIO before
    onAbort morph (after h) (p h)
{-# INLINABLE bracketOnAbort #-}

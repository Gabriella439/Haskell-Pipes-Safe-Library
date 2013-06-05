-- | Exception handling and resource management integrated with proxies

{-# LANGUAGE RankNTypes, CPP #-}

module Control.Proxy.Safe (
    -- * SafeP
    SafeP,
    runSafeP,
    runSafeK,

    -- * SafeIO
    SafeIO,
    runSafeIO,
    runSaferIO,

    -- * Checking Exceptions
    -- $check
    CheckP(..),
    tryIO,
    maskIO,

    -- * Exception Handling
    throw,
    catch,
    handle,

    -- * Finalization
    onAbort,
    finally,
    bracket,
    bracket_,
    bracketOnAbort,

    -- * Utilities

    -- ** Handle allocation
    withFile,

    -- ** String I/O
    -- $string
    readFileS,
    writeFileD,

    -- * Re-exports
    -- $reexports
    module Control.Exception,
    module Control.Proxy.Trans.Either
    ) where

import qualified Control.Exception as Ex
import Control.Exception (SomeException, Exception)
import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.Morph (MFunctor(hoist))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Reader (ReaderT(ReaderT, runReaderT), asks)
import qualified Control.Proxy as P
import qualified Control.Proxy.Core.Fast as PF
import qualified Control.Proxy.Core.Correct as PC
import Control.Proxy ((->>), (>>~), (>\\), (//>), (?>=))
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
import qualified System.IO as IO
import System.IO.Error (userError)

data Finalizers = Finalizers { upstream :: !(IO ()), downstream :: !(IO ()) }

-- | 'SafeP' stores all checked 'Exception's and all registered finalizers
newtype SafeP p a' a b' b m r = SafeP
    { unSafeP :: EitherP SomeException (S.StateP Finalizers p) a' a b' b m r }

-- Deriving 'Functor'
instance (Monad m, P.Proxy p) => Functor (SafeP p a' a b' b m) where
    fmap f p = SafeP (fmap f (unSafeP p))

-- Deriving 'Applicative'
instance (Monad m, P.Proxy p) => Applicative (SafeP p a' a b' b m) where
    pure r    = SafeP (pure r)
    mf <*> mx = SafeP (unSafeP mf <*> unSafeP mx)

-- Deriving 'Monad'
instance (Monad m, P.Proxy p) => Monad (SafeP p a' a b' b m) where
    return r = SafeP (return r)
    m >>= f  = SafeP (unSafeP m >>= \r -> unSafeP (f r))

-- Deriving 'MonadTrans'
instance (P.Proxy p) => MonadTrans (SafeP p a' a b' b) where
    lift = P.lift_P

-- Deriving 'MFunctor'
instance (P.Proxy p) => MFunctor (SafeP p a' a b' b) where
    hoist = P.hoist_P

-- Deriving 'ProxyInternal'
instance (P.Proxy p) => P.ProxyInternal (SafeP p) where
    return_P = \r -> SafeP (P.return_P r)
    m ?>= f  = SafeP (unSafeP m ?>= \r -> unSafeP (f r))

    lift_P m = SafeP (P.lift_P m)

    hoist_P nat p = SafeP (P.hoist_P nat (unSafeP p))

    liftIO_P m = SafeP (P.liftIO_P m)

    thread_P p s = SafeP (P.thread_P (unSafeP p) s)

-- Deriving 'Proxy'
instance (P.Proxy p) => P.Proxy (SafeP p) where
    request = \a' -> SafeP (P.request a')
    respond = \b  -> SafeP (P.respond b )

    fb' ->> p = SafeP ((\b' -> unSafeP (fb' b')) ->> unSafeP p)
    fb' >\\ p = SafeP ((\b' -> unSafeP (fb' b')) >\\ unSafeP p)

    p >>~ fb  = SafeP (unSafeP p >>~ (\b -> unSafeP (fb b)))
    p //> fb  = SafeP (unSafeP p //> (\b -> unSafeP (fb b)))

    turn p = SafeP (P.turn (unSafeP p))

instance P.ProxyTrans SafeP where
    liftP p = SafeP (P.liftP (P.liftP p))

instance P.PFunctor SafeP where
    hoistP nat p = SafeP (P.hoistP (P.hoistP nat) (unSafeP p))

{-| Unwrap a self-contained 'SafeP' session, running all dropped finalizers at
    the end of the computation (ordering finalizers from upstream to downstream)

    Note that all outbound values will be dropped.
-}
runSafeP
    :: (Monad m, P.Proxy p)
    => (forall x . SafeIO x -> m x)
    -- ^ Monad morphism
    -> SafeP p _a' () () _b m r
    -- ^ Self-contained 'SafeP' session
    -> EitherP SomeException p a' a b' b m r
    -- ^ Unwrapped 'Session'
runSafeP morph p = E.EitherP $ P.runIdentityP $ up >\\ (do
    let s0 = Finalizers (return ()) (return ())
    (e, _) <- P.IdentityP $ S.runStateP s0 $ E.runEitherP $ unSafeP $ do
        r <- p `catch` (\e -> do
            fin
            throw (e :: SomeException) )
        fin
        return r
    return e ) //> dn
  where
    up _ = return ()
    dn _ = return ()
    fin = do
        s1 <- SafeP $ P.liftP S.get
        hoist morph $ maskIO $ do
            upstream   s1
            downstream s1
{-# INLINABLE runSafeP #-}

-- | Run a 'SafeP' \'@K@\'leisli arrow
runSafeK
    :: (Monad m, P.Proxy p)
    => (forall x . SafeIO x -> m x)
    -- ^ Monad morphism
    -> (q -> SafeP p _a' () () _b m r)
    -- ^ Self-contained 'SafeP' session
    -> (q -> EitherP SomeException p a' a b' b m r)
    -- ^ Unwrapped 'Session'
runSafeK morph k q = runSafeP morph (k q)
{-# INLINABLE runSafeK #-}

-- | Analogous to 'Ex.throwIO' from @Control.Exception@
throw :: (Monad m, P.Proxy p, Ex.Exception e) => e -> SafeP p a' a b' b m r
throw e = SafeP (E.throw (Ex.toException e))
{-# INLINABLE throw #-}

-- | Analogous to 'Ex.catch' from @Control.Exception@
catch
    :: (Ex.Exception e, Monad m, P.Proxy p)
    => SafeP p a' a b' b m r         -- ^ Original computation
    -> (e -> SafeP p a' a b' b m r)  -- ^ Handler
    -> SafeP p a' a b' b m r         -- ^ Handled computation
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
    -> SafeP p a' a b' b m r         -- ^ Handled computation
handle = flip catch
{-# INLINABLE handle #-}

newtype Mask = Mask { unMask :: forall a . IO a -> IO a }

{-| 'SafeIO' masks asynchronous exceptions by default and only unmasks them
    during 'try' or 'tryIO' blocks.  This ensures that all asynchronous
    exceptions are checked, too.
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

{-| 'runSafeIO' masks asynchronous exceptions using 'Ex.mask' and only unmasks
    them during 'try' or 'tryIO'.

    'runSafeIO' is NOT a monad morphism.
-}
runSafeIO :: SafeIO e -> IO e
runSafeIO m = Ex.mask $ \unmask ->
    runReaderT (unSafeIO m) (Mask unmask)
{-# INLINABLE runSafeIO #-}

{-| 'runSaferIO' masks asynchronous exceptions using 'Ex.uninterruptibleMask'
    and only unmasks them during 'try' or 'tryIO'.

    'runSaferIO' is NOT a monad morphism.
-}
runSaferIO :: SafeIO e -> IO e
runSaferIO m = Ex.uninterruptibleMask $ \unmask ->
    runReaderT (unSafeIO m) (Mask unmask)
{-# INLINABLE runSaferIO #-}

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

{-| Check all exceptions for an 'IO' action, unmasking asynchronous exceptions

    'tryIO' is a monad morphism, with the same caveat as 'try'.
-}
tryIO :: (P.Proxy p) => IO r -> SafeP p a' a b' b SafeIO r
tryIO io = SafeP $ EitherP $ lift $ SafeIO $ ReaderT $ \(Mask restore) ->
    Ex.try $ restore io
{-# INLINABLE tryIO #-}

{-| Like 'tryIO', but does not mask asynchronous exceptions

    'maskIO' is a monad morphism.
-}
maskIO :: (P.Proxy p) => IO r -> SafeP p a' a b' b SafeIO r
maskIO io = SafeP $ EitherP $ lift $ SafeIO $ lift $ Ex.try io

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
            hoist morph $ maskIO after
            throw (e :: SomeException) )
{-# INLINABLE onAbort #-}

{-| Analogous to 'Ex.finally' from @Control.Exception@

    The first argument lifts 'finally' to work with other base monads.  Use 'id'
    if your base monad is already 'SafeIO'.

> finally morph after p = do
>     r <- onAbort morph after p
>     hoist morph $ maskIO after
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
    hoist morph $ maskIO after
    return r
{-# INLINABLE finally #-}

{-| Analogous to 'Ex.bracket' from @Control.Exception@

    The first argument lifts 'bracket' to work with other base monads.  Use 'id'
    if your base monad is already 'SafeIO'.

    'bracket' guarantees that if the resource acquisition completes, then the
    resource will be released.

> bracket morph before after p = do
>     h <- hoist morph $ maskIO before
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
    h <- hoist morph $ maskIO before
    finally morph (after h) (p h)
{-# INLINABLE bracket #-}

{-| Analogous to 'Ex.bracket_' from @Control.Exception@

    The first argument lifts 'bracket_' to work with any base monad.  Use 'id'
    if your base monad is already 'SafeIO'.

> bracket_ morph before after p = do
>     hoist morph $ maskIO before
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
    hoist morph $ maskIO before
    finally morph after p
{-# INLINABLE bracket_ #-}

{-| Analogous to 'Ex.bracketOnError' from @Control.Exception@

    The first argument lifts 'bracketOnAbort' to work with any base monad.  Use
    'id' if your base monad is already 'SafeIO'.

> bracketOnAbort morph before after p = do
>     h <- hoist morph $ maskIO before
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
    h <- hoist morph $ maskIO before
    onAbort morph (after h) (p h)
{-# INLINABLE bracketOnAbort #-}

-- | Safely allocate a 'IO.Handle' within a managed 'Proxy'
withFile
    :: (Monad m, P.Proxy p)
    => (forall x . SafeIO x -> m x)          -- ^Monad morphism
    -> FilePath                              -- ^File
    -> IO.IOMode                             -- ^IO Mode
    -> (IO.Handle -> SafeP p a' a b' b m r)  -- ^Continuation
    -> SafeP p a' a b' b m r
withFile morph file ioMode = bracket morph (IO.openFile file ioMode) IO.hClose
{-# INLINABLE withFile #-}

{- $string
    Note that 'String's are very inefficient, and I will release future separate
    packages with 'ByteString' and 'Text' operations.  I only provide these to
    allow users to test simple I/O without requiring any additional library
    dependencies.
-}

{-| Read from a file, lazily opening the 'IO.Handle' and automatically closing
    it afterwards
-}
readFileS
    :: (P.Proxy p) => FilePath -> () -> P.Producer (SafeP p) String SafeIO ()
readFileS file () = withFile id file IO.ReadMode $ \handle -> do
    let go = do
            eof <- tryIO $ IO.hIsEOF handle
            if eof
                then return ()
                else do
                    str <- tryIO $ IO.hGetLine handle
                    P.respond str
                    go
    go
{-# INLINABLE readFileS #-}

{-| Write to a file, lazily opening the 'IO.Handle' and automatically closing it
    afterwards
-}
writeFileD
    :: (P.Proxy p) => FilePath -> x -> SafeP p x String x String SafeIO r
writeFileD file x0 = do
    withFile id file IO.WriteMode $ \handle -> do
        let go x = do
                str <- P.request x
                tryIO $ IO.hPutStrLn handle str
                x2 <- P.respond str
                go x2
        go x0
{-# INLINABLE writeFileD #-}

{- $reexports
    @Control.Proxy.Trans.Either@ only re-exports 'runEitherP' and 'runEitherK'.

    @Control.Exception@ only re-exports 'SomeException' and 'Exception'.
-}

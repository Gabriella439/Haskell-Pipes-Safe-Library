-- | Exception handling and resource management integrated with proxies

{-# LANGUAGE RankNTypes, CPP #-}

module Pipes.Safe (
    -- * MonadSafe
    MonadSafe(throw, catch, tryIO),
    handle,

    -- * SafeIO
    SafeIO,
    checkSafeIO,
    checkSaferIO,
    runSafeIO,
    runSaferIO,

    -- * Finalization
    promptly,
    onAbort,
    finally,
    bracket,
    bracket_,
    bracketOnAbort,
    ) where

import Prelude hiding (
#if MIN_VERSION_base(4,6,0)
#else
    catch,
#endif
    readFile, writeFile
    )
import qualified System.IO as IO

import Control.Applicative (Applicative(pure, (<*>)), (<*))
import qualified Control.Exception as Ex
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Morph (MFunctor(hoist))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Error (
    ErrorT(ErrorT, runErrorT), Error(noMsg, strMsg), throwError, catchError )
import Control.Monad.Trans.Reader (ReaderT(ReaderT, runReaderT), asks)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
import Pipes
import qualified Pipes.Lift as PL
import System.IO.Error (userError)

newtype Mask = Mask { unMask :: forall a . IO a -> IO a }

data Finalizers = Finalizers
    { upstream   :: [Maybe (IO ())]
    , downstream :: [Maybe (IO ())]
    }

{-| 'SafeIO' masks asynchronous exceptions by default and only unmasks them
    during 'tryIO' blocks.  This ensures that all asynchronous exceptions are
    checked.
-}
newtype SafeIO r = SafeIO { unSafeIO
    :: ErrorT Ex.SomeException (StateT Finalizers (ReaderT Mask IO)) r }

instance Functor SafeIO where
    fmap f m = SafeIO (fmap f (unSafeIO m))

instance Applicative SafeIO where
    pure r  = SafeIO (pure r)
    f <*> x = SafeIO (unSafeIO f <*> unSafeIO x)

instance Monad SafeIO where
    return r = SafeIO (return r)
    m >>= f  = SafeIO (unSafeIO m >>= \a -> unSafeIO (f a))

instance Error Ex.SomeException where
    strMsg str = Ex.toException (userError str)

{-| 'MonadSafe' supports exception handling and runs in a default background of
    masked asynchronous exceptions.

    'liftIO' runs an action with asynchronous exceptions masked.

    'tryIO' runs an action with asynchronous exceptions unmasked.
-}
class (MonadIO m) => MonadSafe m where
    -- | Analogous to 'Ex.throwIO' from @Control.Exception@
    throw :: (Ex.Exception e) => e -> m r
    -- | Analogous to 'Ex.catch' from @Control.Exception@
    catch :: (Ex.Exception e) => m r -> (e -> m r) -> m r
    {-| Check all exceptions for an 'IO' action, unmasking asynchronous
        exceptions
    -}
    tryIO :: IO r -> m r
    getFinalizers :: m Finalizers
    putFinalizers :: Finalizers -> m ()

-- | Analogous to 'Ex.handle' from @Control.Exception@
handle :: (Ex.Exception e, MonadSafe m) => (e -> m r) -> m r -> m r
handle = flip catch
{-# INLINABLE handle #-}

instance MonadIO SafeIO where
    liftIO io = SafeIO $ ErrorT $ lift $ lift $ Ex.try io

instance MonadSafe SafeIO where
    throw e = SafeIO $ throwError (Ex.toException e)
    catch m f = SafeIO $ unSafeIO m `catchError` (\someExc ->
        case Ex.fromException someExc of
            Nothing -> throwError someExc
            Just e  -> unSafeIO (f e) )
    tryIO  io = SafeIO $ ErrorT $ lift $ do
        restore <- asks unMask
        lift $ Ex.try (restore io)
    getFinalizers = SafeIO $ lift get
    putFinalizers finalizers = SafeIO $ lift $ put finalizers

instance (MonadSafe m) => MonadSafe (Proxy a' a b' b m) where
    throw = lift . throw
    catch = PL.liftCatchError catch
    tryIO = lift . tryIO
    getFinalizers = lift getFinalizers
    putFinalizers = lift . putFinalizers

markStartingPoint :: (MonadSafe m) => m ()
markStartingPoint = do
    Finalizers up dn <- getFinalizers
    putFinalizers (Finalizers (Nothing:up) (Nothing:dn))

newFinalizers :: (MonadSafe m) => m (IO (), IO ())
newFinalizers = do
    Finalizers ups dns <- getFinalizers
    let (newUps, ups') = new ups
        (newDns, dns') = new dns
    putFinalizers (Finalizers ups' dns')
    return (newUps, newDns)
  where
    new = go []
    go as mas = case mas of
        []           -> (sequence_ (reverse as), []  )
        Nothing:mas' -> (sequence_ (reverse as), mas')
        Just a :mas' -> go (a:as) mas'

_promptly :: (MonadSafe m) => m r -> m r
_promptly m = do
    markStartingPoint
    (m     >>= (\r -> cleanup >> return  r                     ))
       `catch` (\e -> cleanup >> throw  (e :: Ex.SomeException))
  where
    cleanup = do
        (up, dn) <- newFinalizers
        liftIO up `catch` (\e -> liftIO dn >> throw (e :: Ex.SomeException))
        liftIO dn

_tryWith
    :: (((forall a . IO a -> IO a) -> IO (Either Ex.SomeException r))
        -> IO (Either Ex.SomeException r) )
    -> SafeIO r
    -> IO (Either Ex.SomeException r)
_tryWith mask sio = mask $ \restore ->
    runReaderT (evalStateT (runErrorT (unSafeIO sio0)) s0) (Mask restore)
  where
    sio0 = _promptly sio
    s0   = Finalizers [] []

_rethrow :: IO (Either Ex.SomeException r) -> IO r
_rethrow io = do
    x <- io
    case x of
        Left  e -> Ex.throw e
        Right r -> return r

{-| 'checkSafeIO' masks asynchronous exceptions using 'Ex.mask'.

    Returns caught exceptions in a 'Left'
-}
checkSafeIO :: SafeIO r -> IO (Either Ex.SomeException r)
checkSafeIO = _tryWith Ex.mask
{-# INLINABLE checkSafeIO #-}

-- | Like 'checkSafeIO', except using 'Ex.uninterruptibleMask'
checkSaferIO :: SafeIO r -> IO (Either Ex.SomeException r)
checkSaferIO = _tryWith Ex.uninterruptibleMask
{-# INLINABLE checkSaferIO #-}

-- | Like 'checkSafeIO', except rethrows any caught exceptions
runSafeIO :: SafeIO r -> IO r
runSafeIO sio = _rethrow (checkSafeIO sio)
{-# INLINABLE runSafeIO #-}

-- | Like 'checkSaferIO' except rethrows any caught exceptions
runSaferIO :: SafeIO r -> IO r
runSaferIO sio = _rethrow (checkSaferIO sio)
{-# INLINABLE runSaferIO #-}

{-| @promptly p@ runs all dropped finalizers that @p@ registered when @p@
    completes.
-}
promptly :: (MonadSafe m) => Effect' m r -> Effect' m r
promptly = _promptly

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
    :: (MonadSafe m)
    => IO ()
    -> Proxy a' a b' b m r
    -> Proxy a' a b' b m r
register h p = up >\\ p //> dn
  where
    dn b = do
        old <- getFinalizers
        putFinalizers $ old { upstream = Just h:upstream old }
	b' <- respond b
	putFinalizers old
	return b'
    up a' = do
        old <- getFinalizers
        putFinalizers $ old { downstream = Just h:downstream old }
        a  <- request a'
        putFinalizers old
        return a

{-| Similar to 'Ex.onException' from @Control.Exception@, except this also
    protects against:

    * premature termination, and

    * exceptions in other pipe stages.

    @(`onAbort` fin)@ is a monad morphism:

> (`onAbort` fin) $ do x <- m  =  do x <- m `onAbort` fin
>                      f x           f x `onAbort` fin
>
> return x `onAbort` fin = return x

    'onAbort' ensures finalizers are called from inside to out:

> (`onAbort` fin1) . (`onAbort` fin2) = (`onAbort` (fin2 >> fin1))
>
> (`onAbort` return ()) = id
-}
onAbort
    :: (MonadSafe m)
    => Proxy a' a b' b m r -- ^ Guarded computation
    -> IO s                -- ^ Action to run on abort
    -> Proxy a' a b' b m r
onAbort p after =
    register (after >> return ()) p
        `catch` (\e -> do
            liftIO after
            throw (e :: Ex.SomeException) )
{-# INLINABLE onAbort #-}

{-| Analogous to 'Ex.finally' from @Control.Exception@

> finally p after = do
>     r <- p `onAbort` after
>     liftIO after
>     return r
-}
finally
    :: (MonadSafe m)
    => Proxy a' a b' b m r -- ^ Guarded computation
    -> IO s                -- ^ Guaranteed final action
    -> Proxy a' a b' b m r
finally p after = do
    r <- p `onAbort` after
    liftIO after
    return r
{-# INLINABLE finally #-}

{-| Analogous to 'Ex.bracket' from @Control.Exception@

    'bracket' guarantees that if the resource acquisition completes, then the
    resource will be released.

> bracket before after p = do
>     h <- liftIO before
>     p h `finally` after h
-}
bracket
    :: (MonadSafe m)
    => IO h                        -- ^ Acquire resource
    -> (h -> IO r')                -- ^ Release resource
    -> (h -> Proxy a' a b' b m r)  -- ^ Use resource
    -> Proxy a' a b' b m r
bracket before after p = do
    h <- liftIO before
    p h `finally` after h
{-# INLINABLE bracket #-}

{-| Analogous to 'Ex.bracket_' from @Control.Exception@

> bracket_ before after p = do
>     liftIO before
>     p `finally` after
-}
bracket_
    :: (MonadSafe m)
    => IO s                 -- ^ Acquire resource
    -> IO t                 -- ^ Release resource
    -> Proxy a' a b' b m r  -- ^ Use resource
    -> Proxy a' a b' b m r
bracket_ before after p = do
    liftIO before
    p `finally` after
{-# INLINABLE bracket_ #-}

{-| Analogous to 'Ex.bracketOnError' from @Control.Exception@

> bracketOnAbort before after p = do
>     h <- liftIO before
>     p h `onAbort` after h
-}
bracketOnAbort
    :: (MonadSafe m)
    => IO h                        -- ^ Acquire resource
    -> (h -> IO s)                 -- ^ Release resource
    -> (h -> Proxy a' a b' b m r)  -- ^ Use resource
    -> Proxy a' a b' b m r
bracketOnAbort before after p = do
    h <- liftIO before
    p h `onAbort` after h
{-# INLINABLE bracketOnAbort #-}

{-| Exception handling and resource management integrated with @pipes@

    If you are using @base-4.5.1.0@ or older then the @catch@ from this module
    will conflict with @catch@ from the Prelude.  You will need to either import
    this module qualified:

> import qualified Pipes.Safe as PS

    ... or hide @catch@ from the @Prelude@:

> import Prelude hiding (catch)
-}

{-# LANGUAGE RankNTypes, CPP #-}

module Pipes.Safe (
    -- * MonadSafe
    -- $monadsafe
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

import qualified System.IO as IO

import Control.Applicative (Applicative(pure, (<*>)), (<*))
import qualified Control.Exception as Ex
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Morph (MFunctor(hoist))
import Control.Monad.Trans.Class (MonadTrans(lift))
import qualified Control.Monad.Trans.Error         as E
import qualified Control.Monad.Trans.Identity      as I
import qualified Control.Monad.Trans.Maybe         as M
import qualified Control.Monad.Trans.RWS.Lazy      as RWS
import qualified Control.Monad.Trans.RWS.Strict    as RWS'
import qualified Control.Monad.Trans.Reader        as R
import qualified Control.Monad.Trans.State.Lazy    as S 
import qualified Control.Monad.Trans.State.Strict  as S'
import qualified Control.Monad.Trans.Writer.Lazy   as W 
import qualified Control.Monad.Trans.Writer.Strict as W'
import Data.Monoid (Monoid)
import Pipes
import Pipes.Safe.Internal
import qualified Pipes.Lift as PL
#if MIN_VERSION_base(4,6,0)
#else
import Prelude hiding (catch)
#endif
import System.IO.Error (userError)

{- $monadsafe
    This module does not export the entire 'MonadSafe' type class to protect
    internal invariants.  If you want to implement your own 'MonadSafe'
    instances then you must import 'MonadSafe' from "Pipes.Safe.Internal".
-}

newtype Mask = Mask { unMask :: forall a . IO a -> IO a }

{-| 'SafeIO' masks asynchronous exceptions by default and only unmasks them
    during 'tryIO' blocks.  This ensures that all asynchronous exceptions are
    checked.
-}
newtype SafeIO r = SafeIO
    { unSafeIO
        :: E.ErrorT Ex.SomeException (
           S'.StateT Finalizers (
           R.ReaderT Mask IO )) r
    }

instance Functor SafeIO where
    fmap f m = SafeIO (fmap f (unSafeIO m))

instance Applicative SafeIO where
    pure r  = SafeIO (pure r)
    f <*> x = SafeIO (unSafeIO f <*> unSafeIO x)

instance Monad SafeIO where
    return r = SafeIO (return r)
    m >>= f  = SafeIO (unSafeIO m >>= \a -> unSafeIO (f a))

instance E.Error Ex.SomeException where
    strMsg str = Ex.toException (userError str)

instance MonadIO SafeIO where
    liftIO io = SafeIO $ E.ErrorT $ lift $ lift $ Ex.try io

instance MonadSafe SafeIO where
    throw e = SafeIO $ E.throwError (Ex.toException e)
    catch m f = SafeIO $ unSafeIO m `E.catchError` (\someExc ->
        case Ex.fromException someExc of
            Nothing -> E.throwError someExc
            Just e  -> unSafeIO (f e) )
    tryIO  io = SafeIO $ E.ErrorT $ lift $ do
        restore <- R.asks unMask
        lift $ Ex.try (restore io)
    getFinalizers = SafeIO $ lift S'.get
    putFinalizers finalizers = SafeIO $ lift $ S'.put finalizers

instance (MonadSafe m, E.Error e) => MonadSafe (E.ErrorT e m) where
    throw = lift . throw
    catch m f = E.ErrorT (catch (E.runErrorT m) (\e -> E.runErrorT (f e)))
    tryIO = lift . tryIO
    getFinalizers = lift getFinalizers
    putFinalizers = lift . putFinalizers

instance (MonadSafe m) => MonadSafe (I.IdentityT m) where
    throw = lift . throw
    catch = I.liftCatch catch
    tryIO = lift . tryIO
    getFinalizers = lift getFinalizers
    putFinalizers = lift . putFinalizers

instance (MonadSafe m) => MonadSafe (M.MaybeT m) where
    throw = lift . throw
    catch = M.liftCatch catch
    tryIO = lift . tryIO
    getFinalizers = lift getFinalizers
    putFinalizers = lift . putFinalizers

instance (MonadSafe m, Monoid w) => MonadSafe (RWS.RWST r w s m) where
    throw = lift . throw
    catch = RWS.liftCatch catch
    tryIO = lift . tryIO
    getFinalizers = lift getFinalizers
    putFinalizers = lift . putFinalizers

instance (MonadSafe m, Monoid w) => MonadSafe (RWS'.RWST r w s m) where
    throw = lift . throw
    catch = RWS'.liftCatch catch
    tryIO = lift . tryIO
    getFinalizers = lift getFinalizers
    putFinalizers = lift . putFinalizers

instance (MonadSafe m) => MonadSafe (R.ReaderT i m) where
    throw = lift . throw
    catch = R.liftCatch catch
    tryIO = lift . tryIO
    getFinalizers = lift getFinalizers
    putFinalizers = lift . putFinalizers

instance (MonadSafe m) => MonadSafe (S.StateT s m) where
    throw = lift . throw
    catch = S.liftCatch catch
    tryIO = lift . tryIO
    getFinalizers = lift getFinalizers
    putFinalizers = lift . putFinalizers

instance (MonadSafe m) => MonadSafe (S'.StateT s m) where
    throw = lift . throw
    catch = S'.liftCatch catch
    tryIO = lift . tryIO
    getFinalizers = lift getFinalizers
    putFinalizers = lift . putFinalizers

instance (MonadSafe m, Monoid w) => MonadSafe (W.WriterT w m) where
    throw = lift . throw
    catch = W.liftCatch catch
    tryIO = lift . tryIO
    getFinalizers = lift getFinalizers
    putFinalizers = lift . putFinalizers

instance (MonadSafe m, Monoid w) => MonadSafe (W'.WriterT w m) where
    throw = lift . throw
    catch = W'.liftCatch catch
    tryIO = lift . tryIO
    getFinalizers = lift getFinalizers
    putFinalizers = lift . putFinalizers

instance (MonadSafe m) => MonadSafe (Proxy a' a b' b m) where
    throw = lift . throw
    catch = PL.liftCatchError catch
    tryIO = lift . tryIO
    getFinalizers = lift getFinalizers
    putFinalizers = lift . putFinalizers

-- | Analogous to 'Ex.handle' from @Control.Exception@
handle :: (Ex.Exception e, MonadSafe m) => (e -> m r) -> m r -> m r
handle = flip catch
{-# INLINABLE handle #-}

markStartingPoint :: (MonadSafe m) => m ()
markStartingPoint = do
    Finalizers up dn <- getFinalizers
    putFinalizers (Finalizers (return ():up) (return ():dn))

newFinalizers :: (MonadSafe m) => m (IO (), IO ())
newFinalizers = do
    Finalizers ups dns <- getFinalizers
    let (newUps, ups') = new ups
        (newDns, dns') = new dns
    putFinalizers (Finalizers ups' dns')
    return (newUps, newDns)
  where
    new fins = case fins of
        []   -> (return (), [])
        a:as -> (a        , as)

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
    R.runReaderT (S'.evalStateT (E.runErrorT (unSafeIO sio0)) s0) (Mask restore)
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

{-| @(promptly p)@ runs all dropped finalizers that @p@ registered when @p@
    completes.
-}
promptly :: (MonadSafe m) => Effect' m r -> Effect' m r
promptly = _promptly

{- I don't export 'register' only because people rarely want to guard solely
   against premature termination.  Usually they also want to guard against
   exceptions, too.

    'register' should satisfy the following laws:

* (register m) defines a functor from finalizers to functions:

> register m1 . register m2 = register (m2 >> m1)
> 
> register (return ()) = id

* 'register' defines a functor between Kleisli categories:

> register m . (p1 >=> p2) = register m . p1 >=> register m . p2
>
> register m . return = return
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
        putFinalizers $ old { upstream = add h (upstream old) }
	b' <- respond b
	putFinalizers old
	return b'
    up a' = do
        old <- getFinalizers
        putFinalizers $ old { downstream = add h (downstream old) }
        a  <- request a'
        putFinalizers old
        return a
    add h old = case old of
        []       -> []
        fin:fins -> (fin >> h):fins

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

> (`onAbort` (fin2 >> fin1)) = (`onAbort` fin1) . (`onAbort` fin2)
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

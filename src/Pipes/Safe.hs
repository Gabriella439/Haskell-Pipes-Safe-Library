{-# LANGUAGE RankNTypes, TypeFamilies, FlexibleContexts, FlexibleInstances,
      MultiParamTypeClasses, UndecidableInstances, ScopedTypeVariables,
      GeneralizedNewtypeDeriving, CPP, Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| This module provides an orphan 'MonadCatch' instance for 'Proxy' of the
    form:

> instance (MonadCatch m, MonadIO m) => MonadCatch (Proxy a' a b' b m) where

    ... so you can throw and catch exceptions within pipes using all
    'MonadCatch' operations.

    This module also provides generalized versions of some 'MonadCatch'
    operations so that you can also protect against premature termination of
    connected components.  For example, if you protect a 'readFile' computation
    using 'bracket' from this module:

> -- readFile.hs
> import Pipes
> import qualified Pipes.Prelude as P
> import Pipes.Safe
> import qualified System.IO as IO
> import Prelude hiding (readFile)
>
> readFile :: FilePath -> Producer' String (SafeT IO) ()
> readFile file = bracket
>     (do h <- IO.openFile file IO.ReadMode
>         putStrLn $ "{" ++ file ++ " open}"
>         return h )
>     (\h -> do
>         IO.hClose h
>         putStrLn $ "{" ++ file ++ " closed}" )
>     P.fromHandle

    ... then this generalized 'bracket' will guard against both exceptions and
    premature termination of other pipes:

>>> runSafeT $ runEffect $ readFile "readFile.hs" >-> P.take 4 >-> P.stdoutLn
{readFile.hs open}
-- readFile.hs
import Pipes
import qualified Pipes.Prelude as P
import Pipes.Safe
{readFile.hs closed}

    Note that the 'MonadCatch' instance for 'Proxy' provides weaker versions of
    'mask' and 'uninterruptibleMask' that do not completely prevent asynchronous
    exceptions.  Instead, they provide a weaker guarantee that asynchronous
    exceptions will only occur during 'Pipes.await's or 'Pipes.yield's and
    nowhere else.  For example, if you write:

> mask_ $ do
>     x <- await
>     lift $ print x
>     lift $ print x

    ... then you may receive an asynchronous exception during the 'Pipes.await',
    but you will not receive an asynchronous exception during or in between the
    two 'print' statements.  This weaker guarantee suffices to provide
    asynchronous exception safety.
-}

module Pipes.Safe
    ( -- * SafeT
      SafeT
    , runSafeT
    , runSafeP

     -- * MonadSafe
    , ReleaseKey
    , MonadSafe(..)

      -- * Utilities
      -- $utilities
    , onException
    , tryP
    , catchP
    , finally
    , bracket
    , bracket_
    , bracketOnError

    -- * Re-exports
    -- $reexports
    , module Control.Monad.Catch
    , module Control.Exception
    ) where

import Control.Applicative (Applicative, Alternative)
import Control.Exception(Exception(..), SomeException(..))
import qualified Control.Monad.Catch as C
import Control.Monad.Catch
    ( MonadCatch(..)
    , MonadThrow(..)
    , MonadMask(..)
#if MIN_VERSION_exceptions(0,10,0)
    , ExitCase(..)
#endif
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
    , tryJust
    , Exception(..)
    , SomeException
    )
import Control.Monad (MonadPlus, liftM)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import qualified Control.Monad.Base                as B
import qualified Control.Monad.Catch.Pure          as E
import qualified Control.Monad.Trans.Identity      as I
import qualified Control.Monad.Cont.Class          as CC
import qualified Control.Monad.Error.Class         as EC
import qualified Control.Monad.Primitive           as Prim
import qualified Control.Monad.Trans.Reader        as R
import qualified Control.Monad.Trans.RWS.Lazy      as RWS
import qualified Control.Monad.Trans.RWS.Strict    as RWS'
import qualified Control.Monad.Trans.State.Lazy    as S
import qualified Control.Monad.Trans.State.Strict  as S'
import qualified Control.Monad.State.Class         as SC
import qualified Control.Monad.Trans.Writer.Lazy   as W
import qualified Control.Monad.Trans.Writer.Strict as W'
import qualified Control.Monad.Writer.Class        as WC
#if MIN_VERSION_base(4,6,0)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
#else
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef)
#endif
import qualified Data.Map as M
import Data.Monoid (Monoid)
import Pipes (Proxy, Effect, Effect', runEffect)
import Pipes.Internal (Proxy(..))

data Restore m = Unmasked | Masked (forall x . m x -> m x)

liftMask
    :: forall m a' a b' b r . (MonadIO m, MonadCatch m, MonadFail m)
    => (forall s . ((forall x . m x -> m x) -> m s) -> m s)
    -> ((forall x . Proxy a' a b' b m x -> Proxy a' a b' b m x)
        -> Proxy a' a b' b m r)
    -> Proxy a' a b' b m r
liftMask maskVariant k = do
    ioref <- liftIO $ newIORef Unmasked

    let -- mask adjacent actions in base monad
        loop :: Proxy a' a b' b m r -> Proxy a' a b' b m r
        loop (Request a' fa ) = Request a' (loop . fa )
        loop (Respond b  fb') = Respond b  (loop . fb')
        loop (M m)            = M $ maskVariant $ \unmaskVariant -> do
            -- stash base's unmask and merge action
            liftIO $ writeIORef ioref $ Masked unmaskVariant
            m >>= chunk >>= return . loop
        loop (Pure r)         = Pure r

        -- unmask adjacent actions in base monad
        unmask :: forall q. Proxy a' a b' b m q -> Proxy a' a b' b m q
        unmask (Request a' fa ) = Request a' (unmask . fa )
        unmask (Respond b  fb') = Respond b  (unmask . fb')
        unmask (M m)            = M $ do
            -- retrieve base's unmask and apply to merged action
            Masked unmaskVariant <- liftIO $ readIORef ioref
            unmaskVariant (m >>= chunk >>= return . unmask)
        unmask (Pure q)         = Pure q

        -- merge adjacent actions in base monad
        chunk :: forall s. Proxy a' a b' b m s -> m (Proxy a' a b' b m s)
        chunk (M m) = m >>= chunk
        chunk s     = return s

    loop $ k unmask

instance (MonadMask m, MonadIO m, MonadFail m) => MonadMask (Proxy a' a b' b m) where
    mask = liftMask mask

    uninterruptibleMask = liftMask uninterruptibleMask

#if MIN_VERSION_exceptions(0,10,0)
    generalBracket acquire release_ use = mask $ \unmasked -> do
      a <- acquire
      let action = do
              b <- use a
              return (ExitCaseSuccess b, ExitCaseSuccess_ b)
      let handler e = return (ExitCaseException e, ExitCaseException_ e)
      (exitCase, exitCase_) <- unmasked action `catch` handler
      c <- release_ a exitCase
      case exitCase_ of
          ExitCaseException_ e -> throwM e
          ExitCaseSuccess_ b   -> return (b, c)

-- | This is to avoid an unnecessary partial pattern match in `generalBracket`
data ExitCase_ a = ExitCaseSuccess_ a | ExitCaseException_ SomeException
#endif

data Finalizers m = Finalizers
    { _nextKey    :: !Integer
    , _finalizers :: !(M.Map Integer (m ()))
    }

{-| 'SafeT' is a monad transformer that extends the base monad with the ability
    to 'register' and 'release' finalizers.

    All unreleased finalizers are called at the end of the 'SafeT' block, even
    in the event of exceptions.
-}
newtype SafeT m r = SafeT { unSafeT :: R.ReaderT (IORef (Maybe (Finalizers m))) m r }
    deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
-- The derived instance for `MonadFail` requires a `MonadFail` instance for
-- `ReaderT` which is first available in `transformers-0.5.0.0`
#if MIN_VERSION_transformers(0,5,0)
    , MonadFail
#endif
    , MonadPlus
    , MonadFix
    , EC.MonadError e
    , SC.MonadState s
    , WC.MonadWriter w
    , CC.MonadCont
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadIO
    , B.MonadBase b
    )

instance MonadTrans SafeT where
    lift m = SafeT (lift m)

instance MonadBaseControl b m => MonadBaseControl b (SafeT m) where
#if MIN_VERSION_monad_control(1,0,0)
     type StM (SafeT m) a = StM m a
     liftBaseWith f = SafeT $ R.ReaderT $ \reader' ->
         liftBaseWith $ \runInBase ->
             f $ runInBase . (\(SafeT r) -> R.runReaderT r reader'  )
     restoreM = SafeT . R.ReaderT . const . restoreM
#else
     newtype StM (SafeT m) a = StMT (StM m a)
     liftBaseWith f = SafeT $ R.ReaderT $ \reader' ->
         liftBaseWith $ \runInBase ->
             f $ liftM StMT . runInBase . \(SafeT r) -> R.runReaderT r reader'
     restoreM (StMT base) = SafeT $ R.ReaderT $ const $ restoreM base
#endif

instance Prim.PrimMonad m => Prim.PrimMonad (SafeT m) where
  type PrimState (SafeT m) = Prim.PrimState m
  primitive = lift . Prim.primitive
  {-# INLINE primitive #-}

{-| Run the 'SafeT' monad transformer, executing all unreleased finalizers at
    the end of the computation
-}
runSafeT :: (MonadMask m, MonadIO m) => SafeT m r -> m r
runSafeT m = C.bracket
    (liftIO $ newIORef $! Just $! Finalizers 0 M.empty)
    (\ioref -> do
#if MIN_VERSION_base(4,6,0)
        mres <- liftIO $ atomicModifyIORef' ioref $ \val ->
#else
        mres <- liftIO $ atomicModifyIORef ioref $ \val ->
#endif
            (Nothing, val)
        case mres of
            Nothing -> error "runSafeT's resources were freed by another"
            Just (Finalizers _ fs) -> mapM snd (M.toDescList fs) )
    (R.runReaderT (unSafeT m))
{-# INLINABLE runSafeT #-}

{-| Run 'SafeT' in the base monad, executing all unreleased finalizers at the
    end of the computation

    Use 'runSafeP' to safely flush all unreleased finalizers and ensure prompt
    finalization without exiting the 'Proxy' monad.
-}
runSafeP :: (MonadMask m, MonadIO m) => Effect (SafeT m) r -> Effect' m r
runSafeP = lift . runSafeT . runEffect
{-# INLINABLE runSafeP #-}

-- | Token used to 'release' a previously 'register'ed finalizer
newtype ReleaseKey = ReleaseKey { unlock :: Integer }

{-| 'MonadSafe' lets you 'register' and 'release' finalizers that execute in a
    'Base' monad
-}
class (MonadCatch m, MonadMask m, MonadIO m, MonadIO (Base m)) => MonadSafe m where
    {-| The monad used to run resource management actions, corresponding to the
        monad directly beneath 'SafeT'
    -}
    type Base (m :: * -> *) :: * -> *

    -- | Lift an action from the 'Base' monad
    liftBase :: Base m r -> m r

    {-| 'register' a finalizer, ensuring that the finalizer gets called if the
        finalizer is not 'release'd before the end of the surrounding 'SafeT'
        block.
    -}
    register :: Base m () -> m ReleaseKey

    {-| 'release' a registered finalizer

        You can safely call 'release' more than once on the same 'ReleaseKey'.
        Every 'release' after the first one does nothing.
    -}
    release  :: ReleaseKey -> m ()

instance (MonadIO m, MonadCatch m, MonadMask m) => MonadSafe (SafeT m) where
    type Base (SafeT m) = m

    liftBase = lift

    register io = do
        ioref <- SafeT R.ask
        liftIO $ do
#if MIN_VERSION_base(4,6,0)
            n <- atomicModifyIORef' ioref $ \val ->
#else
            n <- atomicModifyIORef ioref $ \val ->
#endif
                case val of
                    Nothing -> error "register: SafeT block is closed"
                    Just (Finalizers n fs) ->
                        (Just $! Finalizers (n + 1) (M.insert n io fs), n)
            return (ReleaseKey n)

    release key = do
        ioref <- SafeT R.ask
#if MIN_VERSION_base(4,6,0)
        liftIO $ atomicModifyIORef' ioref $ \val ->
#else
        liftIO $ atomicModifyIORef ioref $ \val ->
#endif
            case val of
                Nothing -> error "release: SafeT block is closed"
                Just (Finalizers n fs) ->
                    (Just $! Finalizers n (M.delete (unlock key) fs), ())

instance (MonadSafe m, MonadFail m) => MonadSafe (Proxy a' a b' b m) where
    type Base (Proxy a' a b' b m) = Base m
    liftBase = lift . liftBase
    register = lift . register
    release  = lift . release

instance (MonadSafe m) => MonadSafe (I.IdentityT m) where
    type Base (I.IdentityT m) = Base m
    liftBase = lift . liftBase
    register = lift . register
    release  = lift . release

instance (MonadSafe m) => MonadSafe (E.CatchT m) where
    type Base (E.CatchT m) = Base m
    liftBase = lift . liftBase
    register = lift . register
    release  = lift . release

instance (MonadSafe m) => MonadSafe (R.ReaderT i m) where
    type Base (R.ReaderT i m) = Base m
    liftBase = lift . liftBase
    register = lift . register
    release  = lift . release

instance (MonadSafe m) => MonadSafe (S.StateT s m) where
    type Base (S.StateT s m) = Base m
    liftBase = lift . liftBase
    register = lift . register
    release  = lift . release

instance (MonadSafe m) => MonadSafe (S'.StateT s m) where
    type Base (S'.StateT s m) = Base m
    liftBase = lift . liftBase
    register = lift . register
    release  = lift . release

instance (MonadSafe m, Monoid w) => MonadSafe (W.WriterT w m) where
    type Base (W.WriterT w m) = Base m
    liftBase = lift . liftBase
    register = lift . register
    release  = lift . release

instance (MonadSafe m, Monoid w) => MonadSafe (W'.WriterT w m) where
    type Base (W'.WriterT w m) = Base m
    liftBase = lift . liftBase
    register = lift . register
    release  = lift . release

instance (MonadSafe m, Monoid w) => MonadSafe (RWS.RWST i w s m) where
    type Base (RWS.RWST i w s m) = Base m
    liftBase = lift . liftBase
    register = lift . register
    release  = lift . release

instance (MonadSafe m, Monoid w) => MonadSafe (RWS'.RWST i w s m) where
    type Base (RWS'.RWST i w s m) = Base m
    liftBase = lift . liftBase
    register = lift . register
    release  = lift . release

{-| Analogous to 'C.onException' from @Control.Monad.Catch@, except this also
    protects against premature termination

    @(\`onException\` io)@ is a monad morphism.
-}
onException :: (MonadSafe m) => m a -> Base m b -> m a
m1 `onException` io = do
    key <- register (io >> return ())
    r   <- m1
    release key
    return r
{-# INLINABLE onException #-}

{- $utilities
    These utilities let you supply a finalizer that runs in the 'Base' monad
    (i.e. the monad directly beneath 'SafeT').  If you don't need to use the
    full power of the 'Base' monad and you only need to use to use 'IO', then
    just wrap the finalizer in 'liftIO', like this:

> myAction `finally` (liftIO myFinalizer)

    This will lead to a simple inferred type with a single 'MonadSafe'
    constraint:

> (MonadSafe m) => ...

    For examples of this, see the utilities in "Pipes.Safe.Prelude".

    If you omit the 'liftIO', the compiler will infer the following constraint
    instead:

> (MonadSafe m, Base m ~ IO) => ...

    This means that this function would require 'IO' directly beneath the
    'SafeT' monad transformer, which might not be what you want.
-}

{-| Analogous to 'C.finally' from @Control.Monad.Catch@, except this also
    protects against premature termination
-}
finally :: (MonadSafe m) => m a -> Base m b -> m a
m1 `finally` after = bracket_ (return ()) after m1
{-# INLINABLE finally #-}

{-| Analogous to 'C.bracket' from @Control.Monad.Catch@, except this also
    protects against premature termination
-}
bracket :: (MonadSafe m) => Base m a -> (a -> Base m b) -> (a -> m c) -> m c
bracket before after action = mask $ \restore -> do
    h <- liftBase before
    r <- restore (action h) `onException` after h
    _ <- liftBase (after h)
    return r
{-# INLINABLE bracket #-}

{-| Analogous to 'C.bracket_' from @Control.Monad.Catch@, except this also
    protects against premature termination
-}
bracket_ :: (MonadSafe m) => Base m a -> Base m b -> m c -> m c
bracket_ before after action = bracket before (\_ -> after) (\_ -> action)
{-# INLINABLE bracket_ #-}

{-| Analogous to 'C.bracketOnError' from @Control.Monad.Catch@, except this also
    protects against premature termination
-}
bracketOnError
    :: (MonadSafe m) => Base m a -> (a -> Base m b) -> (a -> m c) -> m c
bracketOnError before after action = mask $ \restore -> do
    h <- liftBase before
    restore (action h) `onException` after h
{-# INLINABLE bracketOnError #-}

{- $reexports
    @Control.Monad.Catch@ re-exports all functions except for the ones that
    conflict with the generalized versions provided here (i.e. 'bracket',
    'finally', etc.).

    @Control.Exception@ re-exports 'Exception' and 'SomeException'.
-}

{- | Transform a 'Proxy' into one that catches any exceptions caused by its
     effects, and returns the resulting exception.
-}
tryP :: (MonadSafe m, Exception e)
     => Proxy a' a b' b m r -> Proxy a' a b' b m (Either e r)
tryP p = case p of
    Request  a' fa  -> Request a' (\a  -> tryP (fa  a))
    Respond  b  fb' -> Respond b  (\b' -> tryP (fb' b'))
    M        m      -> M $ C.try m >>= \eres -> return $ case eres of
        Left  e -> Pure (Left e)
        Right a -> tryP a
    Pure     r      -> Pure (Right r)

{- | Allows direct handling of exceptions raised by the effects in a 'Proxy'.
-}
catchP :: (MonadSafe m, Exception e)
       => Proxy a' a b' b m r -> (e -> Proxy a' a b' b m r)
       -> Proxy a' a b' b m r
catchP p0 f = go p0
  where
    go p = case p of
        Request  a' fa  -> Request a' (\a  -> go (fa  a))
        Respond  b  fb' -> Respond b  (\b' -> go (fb' b'))
        M        m      -> M $ C.catch (liftM go m) (return . f)
        Pure     r      -> Pure r

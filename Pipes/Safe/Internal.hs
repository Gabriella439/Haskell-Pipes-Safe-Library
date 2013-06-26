{-| The full 'MonadSafe' is internal to avoid unsafe use of 'getFinalizers'
    and 'putFinalizers' to tamper with the registered finalizers.  You should
    only import this module if you want to define your own 'MonadSafe'
    instances.
-}

{-# LANGUAGE CPP #-}

module Pipes.Safe.Internal (
    MonadSafe(..),
    Finalizers(..)
    ) where

import qualified Control.Exception as Ex
import Control.Monad.IO.Class (MonadIO(liftIO))
#if MIN_VERSION_base(4,6,0)
#else
import Prelude hiding (catch)
#endif

{-| Finalizers that are upstream and downstream of the current proxy

    'promptly' adds new elements to the lists of stored finalizers to
    distinguish new finalizers from old finalizers.
-}
data Finalizers = Finalizers
    { upstream   :: [IO ()]
    , downstream :: [IO ()]
    }

{-| 'MonadSafe' supports exception handling and runs in a default background of
    masked asynchronous exceptions.

    'liftIO' runs an 'IO' action with asynchronous exceptions masked.

    'tryIO' runs an 'IO' action with asynchronous exceptions unmasked.
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

{-# LANGUAGE CPP #-}

module Pipes.Safe.Internal (
    MonadSafe(..),
    Finalizers(..)
    ) where

import qualified Control.Exception as Ex
import Control.Monad.IO.Class (MonadIO(liftIO))
import Pipes (Proxy, lift)
#if MIN_VERSION_base(4,6,0)
#else
import Prelude hiding (catch)
#endif

data Finalizers = Finalizers
    { upstream   :: [Maybe (IO ())]
    , downstream :: [Maybe (IO ())]
    }

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

{-| I recommend you use this module as the default entry point.

    This module exports the entire library for convenience:

    * "Control.Proxy.Safe.Core": All safety primitives

    * "Control.Proxy.Safe.Prelude": Prelude of managed proxies
-}

module Control.Proxy.Safe (
    -- * Modules
    module Control.Proxy.Safe.Core,
    module Control.Proxy.Safe.Prelude,
    ) where

import Control.Proxy.Safe.Core
import Control.Proxy.Safe.Prelude

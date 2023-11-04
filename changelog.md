# Version

* Export `SafeT` constructor
* Export `MonadReader` instance for `SafeT`

# Version 2.3.1

* Remove `MonadFail` constraints introduced in version 2.3.0
* Implement `MonadFail` for `SafeT`

# Version 2.3.0

* BREAKING CHANGE: Support GHC 8.6.1
    * This requires adding a `MonadFail` constraints to certain utilities

# Version 2.2.9

* Fix build against older versions of `exceptions`

# Version 2.2.8

* Increase upper bound on `exceptions`

# Version 2.2.7

* Increase upper bound on `exceptions`

# Version 2.2.6

* Add `PrimMonad` instance for `SafeT`

# Version 2.2.5

* Add `tryP` and `catchP`
* `MonadThrow` and `MonadCatch` instances for `Proxy` upstreamed to `pipes`

# Version 2.2.4

* Increase upper bound on `pipes`

# Version 2.2.3

* Add several new instances to `SafeT`
* Add `tryP` and `catchP`

# Version 2.2.2

* Raise upper-bound on `exceptions` dependency

# Version 2.2.1

* Raise upper-bound on `exceptions` dependency.

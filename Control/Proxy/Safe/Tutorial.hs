{-| This module provides the tutorial for the @pipes-safe@ library

    This tutorial assumes that you have already read the main @pipes@ tutorial
    in @Control.Proxy.Tutorial@.
-}

module Control.Proxy.Safe.Tutorial (
    -- * Introduction
    -- $intro

    -- * Resource Safety
    -- $safety

    -- * Checked Exceptions
    -- $checked

    -- * Prompt Finalization
    -- $prompt

    -- * Upgrade Proxy Transformers
    -- $trytransformer

    -- * Upgrade Monad Transformers
    -- $trybase

    -- * Backwards Compatible
    -- $backwards

    -- * Conclusion
    -- $conclusion
    ) where

import Control.Proxy
import Control.Proxy.Safe

{- $intro
    @pipes-safe@ adds exception-safe resource management to the @pipes@
    ecosystem.  Use this library if you want to:

    * Safely acquire and release resources within proxies

    * Natively catch and handle exceptions, including asynchronous exceptions

    The following example shows how to use 'P.Proxy' resource management to
    safely open and close a file:

> import Control.Monad (unless)
> import Control.Proxy
> import Control.Proxy.Safe
> import System.IO
> 
> readFileS
>  :: (Proxy p) => FilePath -> () -> Producer (ExceptionP p) String SafeIO ()
> readFileS file () = bracket id
>     (do h <- openFile file ReadMode
>         putStrLn "{File Open}"
>         return h )
>     (\h -> do putStrLn "{Closing File}"
>               hClose h )
>     (\h -> let loop = do
>                    eof <- tryIO $ hIsEOF h
>                    unless eof $ do
>                        str <- tryIO $ hGetLine h
>                        respond str
>                        loop
>             in loop )

    @readFileS@ uses 'bracket' from "Control.Proxy.Safe" to guard the file
    handle, which imposes two constraints on the type:

    * 'bracket' requires the 'ExceptionP' proxy transformer in order to handle
      exceptions

    * 'bracket' requires 'SafeIO' as the base monad, which checks all
      asynchronous exceptions and stores registered finalizers

    But what if I already wrote a 'Consumer' that doesn't use 'ExceptionP' or
    'SafeIO'?

> printer :: (Proxy p, Show a) => () -> Consumer p a IO r
> printer () = runIdentityP $ forever $ do
>     a <- request ()
>     lift $ print a

    Do I need to rewrite it to use resource management abstractions?  Not at
    all!  You can use 'try' / 'tryK' to automatically promote any \"unmanaged\"
    proxy to a \"managed\" proxy:

> tryK printer :: (CheckP p, Show a) => () -> Consumer (Exception p) a SafeIO r
>
> session :: (CheckP p) => () -> Session (Exception p) SafeIO ()
> session = readFileS "test.txt" >-> tryK printer

    The 'CheckP' constraint indicates that the base 'Proxy' type must be
    promotable using 'try'.

    To run this 'Session', we unwrap each layer:

>>> runSafeIO $ runProxy $ runEitherK session :: IO ()
{File Open}
"Line 1"
"Line 2"
"Line 3"
"Line 4"
{Closing File}

-}

{- $safety
    'bracket' guarantees that every successful resource acquisition is paired
    with finalization, even in the face of exceptions or premature 'Session'
    termination.

    For example, if we only draw two lines of input, 'bracket' will still safely
    finalize the handle:

> main = runSafeIO $ runProxy $ runEitherK $
>     readFileS "test.txt" >-> takeB_ 2 >-> tryK printD

>>> main
{File Open}
"Line 1"
"Line 2"
{Closing File}

    We can even sabotage ourselves by killing our own thread after a delay:

> import Control.Concurrent
>
> main = do
>     tID <- myThreadId
>     forkIO $ do
>         threadDelay 1000
>         killThread tID
>     runSafeIO $ runProxy $ runEitherK $
>         foreverK (readFileS "test.txt") >-> tryK printD

>>> main
...
"Line 2"
"Line 3"
"Line 4"
{Closing File}
{File Open}
"Line 1"
"Line 2"
{Closing File}
*** Exception: thread killed

    ... yet 'bracket' still ensures deterministic resource finalization.
-}

{- $checked
    Let's study the types a bit to understand what is going on:

> type ExceptionP = EitherP SomeException

    'ExceptionP' is just a type synonym around 'EitherP'.  @pipes-safe@ uses
    'EitherP' to check all exceptions in order to take advantage of the ability
    to 'catch' and 'throw' exceptions locally.  In fact, "Control.Proxy.Safe"
    defines specialized versions of 'throw' and 'catch' that mirror their
    equivalents in @Control.Exception@:

> throw :: (Monad m, Proxy p, Exception e) => e -> ExceptionP p a' a b' b m r
>
> catch
>  :: (Monad m, Proxy p, Exception e)
>  => ExceptionP p a' a b' b m r
>  -> (e -> ExceptionP p a' a b' b m r)
>  -> ExceptionP p a' a b' b m r

    These let you embed native exception handling into proxies.

    These work because 'SafeIO' checks all exceptions and stores them using the
    'ExceptionP' proxy transformer.  'SafeIO' masks all asynchronous exceptions     by default and only unmasks them in the middle of a 'try' or 'tryIO' block.
    This prevents asynchronous exceptions from leaking between the cracks.

    'runSafeIO' reraises the stored exception when the 'Session' completes, but
    you can also choose to preserve the exception as a 'Left' by using
    'trySafeIO' instead:

> main = do
>     tID <- myThreadId
>     forkIO $ do
>         threadDelay 1000
>         killThread tID
>     trySafeIO $ runProxy $ runEitherK $
>         foreverK (readFileS "test.txt") >-> tryK printD

>>> main
...
"Line 2"
"Line 3"
"Line 4"
{Closing File}
{File Open}
"Line 1"
"Line 2"
{Closing File}
Left thread killed

    You can even choose whether to use 'mask' or 'uninterruptibleMask':

    * 'runSafeIO' \/ 'trySafeIO' use 'mask'

    * 'runSaferIO' \/ 'trySaferIO' use 'uninterruptibleMask'.
-}

{- $prompt
    Resource management primitives like 'bracket' only guarantee prompt
    finalization in the face of exceptions.  Premature termination of
    composition will delay the finalizer until the end of the 'Session'.

    For example, consider the following 'Session':

> session () = do
>    (readFileS "test.hs" >-> takeB_ 2 >-> tryK printD) ()
>    tryIO $ putStrLn "Look busy"

>>> runSafeIO $ runProxy $ runEitherK session
{File Open}
"Line 1"
"Line 2"
Look busy
{Closing File}

    @readFileS@ is interrupted when @takeB_@ terminates, so it does not get
    finalized until the very end of the 'Session'.

    The \"Prompt Finalization\" section of "Control.Proxy.Safe" documents why
    this behavior is the only safe default.  However, often we can prove that
    prompter finalization is safe, so we take matters into our own hands and
    finalize things as promptly as we choose:

session () = do
    (readFileS "test.hs" >-> (takeB_ 2 >=> unsafeClose) >-> tryK printD) ()
    tryIO $ putStrLn "Look busy"

>>> runSafeIO $ runProxy $ runEitherK session
{File Open}
"import Control.Concurrent"
"import Control.Monad (unless)"
{Closing File}
Look busy

    The documentation explains when you can safely use these prompt finalization
    primitives.

    Fortunately, most of the time you will just assemble linear composition
    chains that look like this:

> runSafeIO $ runProxy $ runEitherK $ p1 >-> p2 >-> p3 >-> p4

    ... in which case the end of composition coincides with the end of the
    'Session' and there is no delay in finalization.  You only need to manually
    manage prompt finalization if you sequence anything after composition.
-}

{- $trytransformer
    Not all proxy transformers implement 'try'.  You can look at the instance
    list for 'CheckP' and you will see that it mainly covers the base
    proxy implementations:

> instance CheckP ProxyFast
> instance CheckP ProxyCorrect
> instance (CheckP p) => CheckP (IdentityP p)
> instance (CheckP p) => CheckP (ReaderP   p)

    However, you actually can upgrade more sophisticated stacks.  I've relaxed
    the type signature of the 'PFunctor' type class to accept proxy morphisms
    that change the base monad, so now it accepts 'try' as a valid argument.
    This means that you can use 'hoistP' as many times as necessary to map 'try'
    over the base proxy:

> p :: (CheckP p) => Producer (StateP s (MaybeP p)) IO r
>
> hoistP (hoistP try) p
>   :: (CheckP p) => Producer (StateP s (MaybeP (ExceptionP p))) SafeIO r

    'hoistP' expeccts a proxy morphism for its argument, but is 'try' a proxy
    morphism?  Yes!  'try' satisfies the proxy morphism laws, which are the same
    as the proxy transformer laws.  The documentation for 'try' lists the full
    set of equations, but the ones you should remember are:

> tryK (f >-> g) = tryK f >-> tryK g
>
> tryK idT = idT

> try (request a') = request a'

> try (respond b ) = respond b

>   do x <- try m
>      try (f x)
> = try $ do x <- m
>            f x
>
> try (return x) = return x -- Almost true!

    The last equation is slightly incorrect.  The left hand-side may throw an
    asynchronous exception, but the right-hand side will not.  This does not
    compromise the safety of this library.  At worst, it will just overzealously
    mask pure segments of code if you don't wrap them in 'try', which just
    delays the asynchronous exception until the next 'IO' action.
-}

{- $trybase
    There is one upgrade scenario that this library doesn't cover yet, not
    because it is hard, but rather because I'm still deciding on a name for the
    following type class (suggestions welcome):

> class ??? t where
>     embed?  -- Not sure what to name this either
>      :: (forall r' . p1 a' a b' b    m1  r' -> p2 a' a b' b    m2  r')
>      -> (            p1 a' a b' b (t m1) r  -> p2 a' a b' b (t m2) r )
>
> -- There would be instances for most monad transformers
> instance ??? (EitherT e)
> instance ??? (ReaderT i)
> ...

    @embed?@ would define a functor in the category of proxies, mapping proxy
    morphisms to proxy morphisms and satisfying the functor laws:

> embed? f . embed? g = embed? (f . g)
>
> embed? id = id

    ... and you would use it to lift 'try' to work with arbitrary monad
    transformer stacks in the base monad:

> p :: (CheckP p) => p a' a b' b (EitherT e (ReaderT i IO) r
> embed? (embed? try) p
>   :: (CheckP p) => ExceptionP p a' a b' b (EitherT e (ReaderT i SafeIO)) r

    I will include this type class in a future release once I work out a proper
    name for it.  Then you will be able to upgrade arbitrary proxies.
-}

{- $backwards
    
-}

{- $conclusion
    
    The biggest strength
    * Add resource management to proxies

    * 

    * Extend proxies with asynchronous 
-}

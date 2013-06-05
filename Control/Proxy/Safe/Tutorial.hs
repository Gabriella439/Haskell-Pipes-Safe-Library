{-| This module provides the tutorial for the @pipes-safe@ library

    This tutorial assumes that you have already read the main @pipes@ tutorial
    in @Control.Proxy.Tutorial@.
-}

module Control.Proxy.Safe.Tutorial (
    -- * Introduction
    -- $intro

    -- * Resource Safety
    -- $safety

    -- * Native Exception Handling
    -- $native

    -- * Checked Exceptions
    -- $checked

    -- * Prompt Finalization
    -- $prompt

    -- * Upgrade Proxy Transformers
    -- $trytransformer

    -- * Backwards Compatibility
    -- $backwards

    -- * Laziness
    -- $laziness

    -- * Conclusion
    -- $conclusion
    ) where

import Control.Proxy
import Control.Proxy.Safe
import System.IO (withFile)

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
>     :: (Proxy p) => FilePath -> () -> Producer (SafeP p) String SafeIO ()
> readFileS file () = bracket id
>     (do h <- openFile file ReadMode
>         putStrLn $ "{File Open}"
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

    * 'bracket' requires the 'SafeP' proxy transformer, which stores registered
      finalizers and checked exceptions

    * 'bracket' requires 'SafeIO' as the base monad, which masks all
      asynchronous exceptions

    But what if we already wrote a 'Consumer' that doesn't use 'ExceptionP' or
    'SafeIO'?

> printer :: (Proxy p, Show a) => () -> Consumer p a IO r
> printer () = runIdentityP $ forever $ do
>     a <- request ()
>     lift $ print a

    Do we need to rewrite it to use resource management abstractions?  Not at
    all!  We can use 'try' to automatically promote any \"unmanaged\" proxy to a
    \"managed\" proxy:

> try :: (CheckP p) => p a' a b' b IO r -> SafeP p a' a b' b SafeIO r 
>
> try . printer :: (CheckP p, Show a) => () -> Consumer (Exception p) a SafeIO r
>
> session :: (CheckP p) => () -> Session (SafeP p) SafeIO ()
> session = readFileS "test.txt" >-> try . printer

    The 'CheckP' constraint indicates that the base 'Proxy' type must be
    promotable using 'try'.

    To run this 'Session', we unwrap each layer:

> session
>     :: () -> Session (SafeP p) SafeIO ()
>
> -- Run 'SafeP', executing all dropped finalizers at the end
> runStateK id session
>     :: () -> Session (EitherP SomeException p) SafeIO ()
>
> -- RUn 'EitherP', returning any checked exceptions
> runEitherK $ runStateK id session
>     :: () -> Session p SafeIO (Either SomeException ())
>
> -- Run the 'Session', compiling the pipeline to a 'SafeIO' effect
> runProxy $ runEitherK $ runStateK id session
>     :: SafeIO (Either SomeException ())
>
> -- Run 'SafeIO' executing the computation in a masked background
> runSafeIO $ runProxy $ runEitherK $ runStateK id session
>     :: IO (Either SomeException ())

    This traverses the file and prints one line at a time, with messages marking
    when the file opens and closes:

>>> runSafeIO $ runProxy $ runEitherK $ runSafeK id session
{File Open}
"Line 1"
"Line 2"
"Line 3"
"Line 4"
{Closing File}
Right ()

-}

{- $safety
    'bracket' guarantees that every successful resource acquisition is paired
    with finalization, even in the face of exceptions or premature 'Session'
    termination.

    For example, if we only draw two lines of input, 'bracket' will still safely
    finalize the handle:

> main = runSafeIO $ runProxy $ runEitherK $ runSafeK id $
>     readFileS "test.txt" >-> takeB_ 2 >-> try . printD

>>> main
{File Open}
"Line 1"
"Line 2"
{Closing File}
Right ()

    We can even sabotage ourselves by killing our own thread after a delay:

> import Control.Concurrent
>
> main = do
>     tID <- myThreadId
>     forkIO $ do
>         threadDelay 1000
>         killThread tID
>     runSafeIO $ runProxy $ runEitherK $ runSafeK id $
>         foreverK (readFileS "test.txt") >-> try . printD

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

    ... yet 'bracket' still ensures deterministic resource finalization in the
    face of asynchronous exceptions.
-}

{- $native
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
>     :: (Monad m, Proxy p, Exception e)
>     => ExceptionP p a' a b' b m r
>     -> (e -> ExceptionP p a' a b' b m r)
>     -> ExceptionP p a' a b' b m r

    These let you embed native exception handling into proxies.  For example,
    we could use exception handling to recover from a file opening error:

> import Prelude hiding (catch) -- if using base <= 4.5
>
> openFileS :: (CheckP p) => () -> Producer (ExceptionP p) String SafeIO ()
> openFileS () = (do
>     tryIO $ putStrLn "Select a file:"
>     file <- tryIO getLine
>     readFileS file () )
>   `catch` (\e -> do
>       tryIO $ print (e :: IOException)
>       openFileS () )

>>> runSafeIO $ runProxy $ runEitherK $ openFileS >-> tryK printD
Select a file:
oops
oops: openFile: does not exist (No such file or directory)
Select a file:
test.txt
{File Open}
"Line 1"
"Line 2"
"Line 3"
"Line 4"
{Closing File}

    You can even catch and resume from asynchronous exceptions:

> heartbeat
>      :: Proxy p
>      => ExceptionP p a' a b' b SafeIO r -> ExceptionP p a' a b' b SafeIO r
> heartbeat p = p `catch` (\e -> do
>            let _ = e :: SomeException
>            tryIO $ putStrLn "<Nice try!>"
>            heartbeat p )
>
> main = do
>     tid <- myThreadId
>     forkIO $ forever $ do
>         threadDelay 5000000  -- Every 5 seconds
>         killThread tid
>     trySafeIO $ runProxy $ runEitherK $
>         heartbeat . (openFileS >-> tryK printD)

>>> main
Select a file:
te<Nice Try!>
Select a file:
st.txt
{File Open}
"Line 1"
"Line 2"
"Line 3"
"Line 4"
{Closing File}

-}

{- $checked
    Exception handling works because 'SafeIO' checks all exceptions and stores
    them using the 'ExceptionP' proxy transformer.  'SafeIO' masks all
    asynchronous exceptions by default and only unmasks them in the middle of a
    'try' or 'tryIO' block.  This prevents asynchronous exceptions from leaking
    between the cracks.

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

    * 'runSafeIO' and 'trySafeIO' both use 'mask'

    * 'runSaferIO' and 'trySaferIO' both use 'uninterruptibleMask'.
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
    prompter finalization is safe, in which case we can take matters into our
    own hands and manually finalize things even more promptly:

> session () = do
>     (readFileS "test.hs" >-> (takeB_ 2 >=> unsafeClose) >-> tryK printD) ()
>     tryIO $ putStrLn "Look busy"

>>> runSafeIO $ runProxy $ runEitherK session
{File Open}
"import Control.Concurrent"
"import Control.Monad (unless)"
{Closing File}
Look busy

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
    proxy implementations and trivial proxy transformers:

> instance CheckP ProxyFast
> instance CheckP ProxyCorrect
> instance (CheckP p) => CheckP (IdentityP p)
> instance (CheckP p) => CheckP (ReaderP   p)

    This means that we can usually only 'try' the base proxy.  However, this is
    not a problem because we can just 'hoistP' 'try' over the outer proxy
    transformers to target it to the base proxy.

    For example, if we have a 'Proxy' with two proxy transformer layers:

> p :: (CheckP p) => Producer (StateP s (MaybeP p)) IO r

    ... we just 'hoistP' the 'try' over the two outer layers to target it to the
    base 'Proxy':

> hoistP (hoistP try) p
>     :: (CheckP p) => Producer (StateP s (MaybeP (ExceptionP p))) SafeIO r

    'hoistP' expects a proxy morphism for its argument, but is 'try' a proxy
    morphism?  Yes!  'try' satisfies the proxy morphism laws and the
    documentation in the @Control.Proxy.Morph@ module (from the @pipes@ package)
    lists the full set of laws.  The important laws you should remember are:

> tryK (f >-> g) = tryK f >-> tryK g

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
    delays the asynchronous exception until the next 'try' or 'tryIO' block.

    There is one upgrade scenario that this library does not yet cover, which is
    'try'ing proxies that have base monads other than 'IO'.  For now, you will
    have to rewrite the proxy if that happens.
-}

{- $backwards
    The biggest strength of @pipes-safe@ is that it requires no buy-in from the
    rest of the @pipes@ ecosystem.  Many proxies require no resource-management
    at all, so why should they clutter their implementation with such concerns?

    @pipes-safe@ lets you write these proxies using the simpler \"unmanaged\"
    types, and then transparently promote them with 'try' later on if you need
    to use them within a resource-managed 'Session'.

    For example, the main body of @readFileS@ is identical to the implementation
    of 'hGetLineS' from the proxy prelude:

> hGetLineS :: (Proxy p) => Handle -> () -> Producer p String IO ()
> hGetLineS h () = runIdentityP loop where
>     loop = do
>         eof <- lift $ hIsEOF h
>         if eof
>             then return ()
>             else do
>                 str <- lift $ hGetLine h
>                 respond str
>                 loop

    We can reuse 'hGetLineS' by define a 'withFileS' that abstracts away the
    handle management:

> withFileS
>     :: (Proxy p)
>     => FilePath
>     -> (Handle -> b' -> ExceptionP p a' a b' b SafeIO r)
>     -> b' -> ExceptionP p a' a b' b SafeIO r
> withFileS file p b' = bracket id
>     (do h <- openFile file ReadMode
>         putStrLn "{File Open}"
>         return h )
>     (\h -> do putStrLn "{Closing File}"
>               hClose h )
>     (\h -> p h b')

    ... and now we can 'readFileS' in terms of 'withFileS' and 'hGetLineS':

> readFileS file = withFileS file (\h -> tryK (hGetLineS h))

    If 'hGetLineS' throws an error within its own code, 'withFileS' will still
    properly finalize the handle.  This works in spite of 'hGetLineS' never
    having been written to be resource safe.
-}

{- $laziness
    Now you no longer need to open all resources before a 'Session' and close
    them afterwards.  Instead, you can lazily open resources in response to
    demand and trust that they finalize safely upon termination:

> files () = do
>     readFileS "file1.txt" ()  -- 3 lines long
>     readFileS "file2.txt" ()  -- 4 lines long
> -- or: files = readFileS "file1.txt" >=> readFileS "file2.txt"

>>> runSafeIO $ runProxy $ runEitherK $ files >-> takeB_ 2 >-> printD
{File Open}
"Line 1 of file1.txt"
"Line 2 of file1.txt"
{Closing File}

    @\"file2.txt\"@ never opens because we only demand two lines.

    Even if we use both files, we never keep more than one handle open at a
    time:
 
>>> runSafeIO $ runProxy $ runEitherK $ files >-> takeB_ 5 >-> printD
{File Open}
"Line 1 of file1.txt"
"Line 2 of file1.txt"
"Line 3 of file1.txt"
{Closing File}
{File Open}
"Line 1 of file2.txt"
"Line 2 of file2.txt"
{Closing File}

-}

{- $conclusion
    @pipes-safe@ lets you package streaming resources into self-contained units
    that include:

    * their allocation/deallocation code, and

    * their exception-handling strategies.

    @pipes-safe@ reuses 'EitherP' to let you easily reason about how local
    exception handling behaves.  More importantly, multiple resources can
    concurrently coexist with each other and not interfere with each other's
    exception-handling logic.  @pipes-safe@ isolates each streaming component's
    behavior so that you reason about it how it deals with failure independently
    of other components.

    I hope this inspires people to package up more powerful streaming
    abstractions into indivisible units.  Also, don't limit yourself to simple
    file or network resources.  You will find that @pipes-safe@ can also
    simplify and package up:

    * progress meters,

    * input devices (i.e. mice and keyboards), and

    * user interfaces.

    I encourage you to be creative!
-}

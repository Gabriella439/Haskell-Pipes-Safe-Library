# Pipes-Safe v2.0.1

`pipes-safe` builds upon
[the `pipes` library](https://github.com/Gabriel439/Haskell-Pipes-Library) to
provide exception safety and resource management.

## Quick start

* Install the [Haskell Platform](http://www.haskell.org/platform/)
* `cabal install pipes-safe`

The official tutorial is on
[Hackage](http://hackage.haskell.org/package/pipes-safe).

## Features

* *Resource Safety*: Guarantee finalization using `finally`, `bracket`, and more

* *Exception Safety*: Even against asynchronous exceptions!

* *Laziness*: Only acquire resources when you need them

* *Promptness*: Finalize resources early when you are done with them

* *Native Exception Handling*: Catch and resume from exceptions inside pipes

* *No Buy-in*: Mix resource-safe pipes with unmanaged pipes using `hoist`

## Outline

Use `pipes-safe` for production code where you need deterministic and prompt
release of resources in the fact of exceptions or premature pipe termination.
`pipes-safe` lets you safely acquire resources and handle exceptions within
pipelines.

## Development Status

`pipes-safe` is complete and if the API does not change by the end of 2013 then
the library will be officially stabilized.

## Community Resources

Use the same resources as the core `pipes` library to learn more, contribute, or
request help:

* [Haskell wiki page](http://www.haskell.org/haskellwiki/Pipes)

* [Mailing list](mailto:haskell-pipes@googlegroups.com) ([Google Group](https://groups.google.com/forum/?fromgroups#!forum/haskell-pipes))

## How to contribute

* Build derived libraries

* Write `pipes-safe` tutorials

## License (BSD 3-clause)

Copyright (c) 2013 Gabriel Gonzalez
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice, this
  list of conditions and the following disclaimer in the documentation and/or
  other materials provided with the distribution.

* Neither the name of Gabriel Gonzalez nor the names of other contributors may
  be used to endorse or promote products derived from this software without
  specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

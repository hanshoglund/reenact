
# Reenact

Reenact is a reimplementation of the Reactive library by Conal Elliot.

It preserves semantics and most operators of the original library. In
particular the `Monoid`, `Applicative` and `Monad` instances for 
`Events`, `Reactives` and `Behaviours` are available and have the original semantics.
    
The implementation however is completely different, based on asynchronous
channels instead of the `unamb` operator.

(c) Hans Höglund 2013

## Requirements

* [Haskell Platform](http://www.haskell.org/platform)

## Installation

    cabal configure
    cabal install

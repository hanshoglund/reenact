
{-
    Based on communication with Conal:
    
    I recommend instead looking for a subset of imperative computation that
    suffices to implement the denotation you want, but is well-defined
    denotationally and tractable for reasoning. IO (general imperative
    computation) is neither, which is why we have denotative/functional
    programming in the first place.
-}

-- For each incoming value, possibly transform the FRP internals
type Handler a = (a -> FRP -> FRP)
-- Output is 
type Dispatcher a = (FRP -> a)


-- (Note: The above is exactly the Getter/Setter definition of the lens library.)

-- An FRP is an "event loop", i.e. a state monad over an untyped state
-- Whenever an input occurs, zero or more subscribers are fired. Each
-- subscriber may trigger a new input and so forth.
--
-- The main "challenge" is concurrent input: how to distribute FRP
-- transformations over multiple threads. This can probably be done
-- with diff-based merges (similar to GIT commits).
--
type FRP



subscribe   :: Handler      -> FRP -> FRP
cancel      :: Subscription -> FRP -> FRP

-- Run all the actions
runFRP :: FRP -> IO ()
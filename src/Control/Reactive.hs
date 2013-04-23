
{-# LANGUAGE GADTs, TypeFamilies, ScopedTypeVariables, OverloadedStrings, BangPatterns #-}

-- |
--
-- Primitives:
-- 
-- * 'Event':    'mempty', 'mappend', 'fmap'
--
-- * 'Reactive': 'fmap', 'return', 'join', '<'
--
-- * 'scatterE'
--
-- * 'accumE' or 'accumR'
--
-- * 'stepper', 'apply' (or 'sample', or 'snapshotWith')
--
-- * 'readChanE', 'writeChanE', 'getE', 'pollE', 'putE', 'runLoopUntil'
--

module Control.Reactive (

        -- * Types
        Event,
        Reactive,

        -- * Basic combinators
        -- ** Event to reactive
        stepper,
        -- switcher,
        maybeStepper,
        -- maybeSwitcher,
        -- sampleAndHold,
        sampleAndHold2,
        
        -- ** Reactive to event
        apply,
        filter',
        gate,
        sample,
        snapshot,
        snapshotWith,

        -- * Merging and splitting values
        justE,
        splitE,
        eitherE,
        -- filterE,
        -- retainE,
        -- partitionE,
        -- zipR,
        -- unzipR,

        -- * Past-dependent values
        -- ** Buffering events
        lastE,
        delayE,
        -- recallE,
        recallEWith,
        diffE,
        bufferE,
        gatherE,
        scatterE,

        -- ** Accumulating values
        accumE,
        accumR,
        foldpE,
        foldpR,
        scanlE,
        scanlR,
        mapAccum,

        -- ** Special accumulators
        firstE,
        restE,
        countE,
        countR,
        monoidE,
        monoidR,

        -- ** Lifted monoids
        sumE,
        productE,
        allE,
        anyE,
        sumR,
        productR,
        allR,
        anyR,    
        
        -- * Toggles and switches
        tickE,
        onR,
        offR,
        toggleR, 

        -- * Time
        -- Time,
        pulse,
        time,
        integral,
                   
        -- * Record and playback
        TransportControl(..),
        transport,
        record,
        playback,
        playback',

        -- * Special functions
        seqE,
        oftenE,

        -- * Creating events and reactives
        -- ** From standard library
        getCharE,
        putCharE,
        getLineE,
        putLineE,
        
        systemTimeR,
        systemTimeSecondsR, 
        systemTimeDayR, 


        -- ** From channels
        readChanE,
        writeChanE,

        -- ** From IO
        getE,
        pollE,
        putE,
        -- modifyE,
        
        -- * Run events
        run,
        runLoop,
        runLoopUntil,        

        -- * Utility
        Source,
        Sink,
        newSource,
        newSink,        
        notify,
        showing,
        runEvent,
        runReactive,
        unsafeGetReactive,
  ) where

import Prelude hiding (mapM)

import Data.Time
import Data.Monoid  
import Data.Maybe
import Data.Either
import Data.String
import Data.VectorSpace hiding (Sum, getSum)
import Control.Monad
import Control.Applicative
-- import Control.Newtype

import Control.Concurrent (forkIO, forkOS, threadDelay)
import System.IO.Unsafe

import Control.Reactive.Chan
import Control.Reactive.Var


-------------------------------------------------------------------------------------
-- Primitives
-------------------------------------------------------------------------------------

-- | 
-- A stream of values.
--
-- > type Event a = [(Time, a)]
-- 
data Event a where
    ENever  ::                             Event a
    
    EMerge  :: Event a     -> Event a   -> Event a
    ESeq    :: Event a     -> Event b   -> Event b
    EMap    :: (a -> b)    -> Event a   -> Event b
    EPred   :: (a -> Bool) -> Event a   -> Event a
    EConcat ::                Event [a] -> Event a

    EChan   :: Chan a                   -> Event a
    ESource :: IO [a]                   -> Event a
    ESink   :: (a -> IO b) -> Event a   -> Event b

    ESamp  :: Reactive a   -> Event b   -> Event a

-- | 
-- A time-varying value.
--
-- > type Reactive a = Time -> a
-- 
data Reactive a where
    RConst  :: a                                -> Reactive a

    RStep   :: Var a -> Event a                 -> Reactive a
    RAccum  :: Var a -> Event (a -> a)          -> Reactive a
    
    RApply  :: Reactive (a -> b) -> Reactive a  -> Reactive b
    -- RJoin   :: Reactive (Reactive a)            -> Reactive a
    

prepE :: Event a -> IO (Event a)
prepE (EMerge a b)     = do
    a' <- prepE a
    b' <- prepE b
    return $ EMerge a' b'
prepE (ESeq a b)     = do
    a' <- prepE a
    b' <- prepE b
    return $ ESeq a' b'
prepE (EMap f x)    = do
    x' <- prepE x
    return $ EMap f x'
prepE (EPred p x)    = do
    x' <- prepE x
    return $ EPred p x'
prepE (EConcat x)    = do
    x' <- prepE x
    return $ EConcat x'
prepE (ESink k a)     = do
    a' <- prepE a
    return $ ESink k a'
prepE (ESamp r x)    = do
    r' <- prepR r
    x' <- prepE x
    return $ ESamp r' x'
prepE (EChan ch)      = do
    ch' <- prepC ch
    return $ ESource ch' 
prepE x               = return x

prepR :: Reactive a -> IO (Reactive a)
prepR (RConst v)  = do
    return $ RConst v
prepR (RStep v x) = do
    x' <- prepE x
    v' <- prepV v
    return $ RStep v' x'
prepR (RAccum v x) = do
    x' <- prepE x
    v' <- prepV v
    return $ RAccum v' x'
prepR (RApply f x) = do
    f' <- prepR f
    x' <- prepR x
    return $ RApply f' x'
-- prepR (RJoin r) = do
--     r' <- prepR r
--     return $ RJoin r'

    -- r'' <- prepR r'
    -- return $ RJoin r''

-- prepR x = return x


prepC :: Chan a -> IO (IO [a])
prepC ch = do
    ch' <- dupChan ch
    return $ fmap maybeToList $ tryReadChan ch'

prepV :: Var a -> IO (Var a)
prepV v = dupVar v

runE :: Event a -> IO [a]
runE ENever          = return []
runE (EMap f x)      = fmap (fmap f) (runE x)
runE (EPred p x)     = fmap (filter p) (runE x)
runE (EConcat x)     = fmap concat (runE x)
runE (EMerge a b)    = liftM2 (++) (runE a) (runE b)
runE (ESource i)     = i
runE (ESink o x)     = runE x >>= mapM o
runE (ESeq a b)      = runE a >> runE b
runE (ESamp r x)    = do
    r' <- runRS r
    x' <- runE x
    return $ fmap (const r') x'
    -- case x' of
        -- [] -> return []
        -- _  -> return [r']

runRS :: Reactive a -> IO a
runRS = fmap last . runR
-- Note: last is safe as reactives (per definition) always have at least one value

runR :: Reactive a -> IO [a]
runR (RConst v)      = return [v]
runR (RStep v x)     = do
    v' <- readVar v
    x' <- runE x       
    let !ys = (v':x')
    writeVar v (last ys)
    -- putStrLn $ "RStep, size is " ++ show (length x')
    return ys
runR (RAccum v x)   = do
    v' <- readVar v
    x' <- runE x
    let !w = (foldr (.) id x') v'
    writeVar v w
    -- putStrLn $ "RAccum, size is " ++ show (length x')
    return [w]    
runR (RApply f x)   = do
    f' <- runR f
    x' <- runR x
    return $ f' <*> x'

-- FIXME leaks here?
-- FIXME we need an extra prepare here is the subnetwork is switched in
-- runR (RJoin r)   = do
    -- r' <- runRS r
    -- r_ <- prepR r'
    -- runR r_

    -- r' <- runR r
    -- r_ <- mapM prepR r'
    -- r_' <- mapM runR r_
    -- return $ concat r_'

{-# INLINE runR #-}
{-# INLINE runE #-}
{-# INLINE runRS #-}

-------------------------------------------------------------------------------------
-- Event API
-------------------------------------------------------------------------------------

-- |
-- Event is a functor: 'fmap' transforms each value.
--
instance Functor (Event) where
    fmap    = EMap

-- |
-- Event is a monoid: 'mempty' is the event that never occurs, 'mappend' interleaves values.
--
instance Monoid (Event a) where
    mempty  = ENever
    mappend = EMerge


-- |
-- The empty event.
--
never :: Event a
never = mempty

-- |
-- Interleave values.
--
mergeE :: Event a -> Event a -> Event a
mergeE = mappend

-- |
-- Interleave values of different types.
--
eitherE :: Event a -> Event b -> Event (Either a b)
a `eitherE` b = fmap Left a `mergeE` fmap Right b

-- |
-- Run both and behave as the second event.
--
seqE :: Event a -> Event b -> Event b
seqE = ESeq

oftenE :: Event ()
oftenE = pollE $ return $ Just ()

-- |
-- Map over values (synonym for @f \<$> xs@).
mapE :: (a -> b) -> Event a -> Event b
mapE = (<$>)

-- |
-- Filter values, semantically @filter p xs@.
--
filterE :: (a -> Bool) -> Event a -> Event a
filterE p = EPred p

-- |
-- Retain values, semantically @retain p xs@.
--
retainE :: (a -> Bool) -> Event a -> Event a
retainE p = EPred (not . p)

-- |
-- Separate chunks of values.
--
-- > scatterE [e1,e2..] = [e1] <> [e2] ..
--
scatterE :: Event [a] -> Event a
scatterE = EConcat

-- |
-- Discard empty values.
--
justE :: Event (Maybe a) -> Event a
justE = EConcat . fmap maybeToList

-- |
-- Partition values, semantically @partition p xs@.
-- 
-- > let (x, y) = partitionE p e in mergeE x y  ≡  e
--
partitionE :: (a -> Bool) -> Event a -> (Event a, Event a)
partitionE p e = (filterE p e, retainE p e)

-- | 
-- Partition values of different types. See also 'partitionE'.
--
-- > let (x, y) in eitherE x y = splitE e  ≡  e
-- 
splitE :: Event (Either a b) -> (Event a, Event b)
splitE e = (justE $ fromLeft <$> e, justE $ fromRight <$> e)

unzipE :: Event (a, b) -> (Event a, Event b)
unzipE e = (fst <$> e, snd <$> e)

unzipR :: Reactive (a, b) -> (Reactive a, Reactive b)
unzipR r = (fst <$> r, snd <$> r)

-- |
-- Replace values, semantically @x <$ e@.
--
replaceE :: b -> Event a -> Event b
replaceE x = (x <$)

-- |
-- Throw away values of the event.
--
-- This is of course just @() <$ x@ but it is useful to fix the type in some cases.
--
tickE :: Event a -> Event ()
tickE = replaceE ()

-- |
-- Discard values, using an arbitrary empty element.
--
tickME :: Monoid b => Event a -> Event b
tickME = replaceE mempty

-- |
-- Event accumulator.
--
-- > a `accumE` e = (a `accumR` e) `sample` e
-- > a `accumR` e = a `stepper` (a `accumE` e)
--        
accumE :: a -> Event (a -> a) -> Event a
a `accumE` e = (a `accumR` e) `sample` e

-- |
-- Create a past-dependent event.
--
-- > scanlE f z x = foldpE (flip f) f z x
--        
foldpE :: (a -> b -> b) -> b -> Event a -> Event b
foldpE f a e = a `accumE` (f <$> e)

-- |
-- Create a past-dependent event. This combinator corresponds to 'scanl' on streams.
--
-- > scanlE f z x = foldpE (flip f) f z x
--        
scanlE :: (a -> b -> a) -> a -> Event b -> Event a
scanlE f = foldpE (flip f)
        
-- |
-- Create a past-dependent event using a 'Monoid' instance.
--
monoidE :: Monoid a => Event a -> Event a
monoidE = scanlE mappend mempty

liftMonoidE :: Monoid m => (a -> m) -> (m -> a) -> Event a -> Event a
liftMonoidE i o = fmap o . monoidE . fmap i

sumE :: Num a => Event a -> Event a
sumE = liftMonoidE Sum getSum

productE :: Num a => Event a -> Event a
productE = liftMonoidE Product getProduct

allE :: Event Bool -> Event Bool
allE = liftMonoidE All getAll

anyE :: Event Bool -> Event Bool
anyE = liftMonoidE Any getAny


-- |
-- Get just the first value.
--
firstE :: Event a -> Event a
firstE = justE . fmap snd . foldpE g (True,Nothing)
    where
        g c (True, _)  = (False,Just c)    -- first time output
        g c (False, _) = (False,Nothing)   -- then no output
            
-- |
-- Get all but the first value.
--
restE :: Event a -> Event a
restE = justE . fmap snd . foldpE g (True,Nothing)
    where        
        g c (True, _)  = (False,Nothing) -- first time no output
        g c (False, _) = (False,Just c)  -- then output

-- |
-- Count values.
--
countE :: Enum b => Event a -> Event b
countE = accumE (toEnum 0) . fmap (const succ)

-- |
-- Delay by one value.
--
lastE :: Event a -> Event a
lastE = fmap snd . recallE

-- |
-- Delay by @n@ values.
--
delayE :: Int -> Event a -> Event a
delayE n = foldr (.) id (replicate n lastE)

-- |
-- Buffer up to /n/ values. When the buffer is full, old elements will be rotated out.
--
-- > bufferE n e = [[e1],[e1,e2]..[e1..en],[e2..en+1]..]
--
bufferE :: Int -> Event a -> Event [a]
bufferE n = (reverse <$>) . foldpE g []
    where
        g x xs = x : take (n-1) xs

-- |
-- Gather event values into chunks of regular size.
--
-- > gatherE n e = [[e1..en],[en+1..e2n]..]
--
gatherE :: Int -> Event a -> Event [a]
gatherE n = (reverse <$>) . filterE (\xs -> length xs == n) . foldpE g []
    where
        g x xs | length xs <  n  =  x : xs
               | length xs == n  =  x : []
               | otherwise       = error "gatherE: Wrong length"

-- |
-- Pack with last value.
--
recallE :: Event a -> Event (a, a)
recallE = recallEWith (,)

-- |
-- Pack with last value.
--
-- recallEWith :: (a -> a -> b) -> Event a -> Event b
-- recallEWith f = justE . fmap k . bufferE 2
--     where
--         k []      = Nothing
--         k [x]     = Nothing
--         k (a:b:_) = Just $ f a b

-- Note: flipped order from Reactive
recallEWith f e 
    = (joinMaybes' . fmap combineMaybes) 
    $ dup Nothing `accumE` fmap (shift . Just) e
    where      
        shift b (_,a) = (a,b)
        dup x         = (x,x)
        joinMaybes'   = justE
        combineMaybes = uncurry (liftA2 f)

{-

TODO not sure about these

eventMain :: Event (Maybe Bool) -> IO ()
eventMain = eventMain' . (fmap . fmap) (\r -> if r then ExitSuccess else ExitFailure (-1))

eventMain' :: Event (Maybe ExitCode) -> IO ()
eventMain' e = do
    code <- runLoopUntil e
    exitWith code        
-}

-------------------------------------------------------------------------------------
-- Reactive API
-------------------------------------------------------------------------------------

-- |
-- Reactive has a lifted is a monoid: 'mempty' is the constant empty value and
-- mappend combines values according to 'mappend' on values.
--
instance Monoid a => Monoid (Reactive a) where
    mempty  = pure mempty
    mappend = liftA2 mappend

-- |
-- Reactive is a functor: 'fmap' transforms the value at each point in time.
--
instance Functor Reactive where
    fmap f = (pure f <*>)

-- |
-- Reactive is an applicative functor: 'pure' is a constant value and @fr \<*> xr@ applies the
-- function @fr t@ to the value @xr t@.
--
instance Applicative Reactive where
    pure = RConst
    -- pure x = x `stepper` never 
    (<*>) = RApply

-- instance Monad Reactive where
--     return  = pure
--     x >>= k = (RJoin . fmap k) x

instance IsString a => IsString (Reactive a) where
    fromString = pure . fromString

instance Eq (Reactive b) where
    (==) = noFun "(==)"
    (/=) = noFun "(/=)"

instance Ord b => Ord (Reactive b) where
    min = liftA2 min
    max = liftA2 max

instance Enum a => Enum (Reactive a) where
    succ           = fmap succ
    pred           = fmap pred
    toEnum         = pure . toEnum
    fromEnum       = noFun "fromEnum"
    enumFrom       = noFun "enumFrom"
    enumFromThen   = noFun "enumFromThen"
    enumFromTo     = noFun "enumFromTo"
    enumFromThenTo = noFun "enumFromThenTo"

instance Num a => Num (Reactive a) where
    (+)         = liftA2 (+)
    (*)         = liftA2 (*)
    (-)         = liftA2 (-)
    abs         = fmap abs
    signum      = fmap signum
    fromInteger = pure . fromInteger

instance (Num a, Ord a) => Real (Reactive a) where
  toRational = noFun "toRational"

instance Integral a => Integral (Reactive a) where
    quot      = liftA2 quot
    rem       = liftA2 rem
    div       = liftA2 div
    mod       = liftA2 mod
    quotRem   = (fmap.fmap) unzipR (liftA2 quotRem)
    divMod    = (fmap.fmap) unzipR (liftA2 divMod)
    toInteger = noFun "toInteger"

instance Fractional b => Fractional (Reactive b) where
    recip        = fmap recip
    fromRational = pure . fromRational

instance Floating b => Floating (Reactive b) where
    pi    = pure pi
    sqrt  = fmap sqrt
    exp   = fmap exp
    log   = fmap log
    sin   = fmap sin
    cos   = fmap cos
    asin  = fmap asin
    atan  = fmap atan
    acos  = fmap acos
    sinh  = fmap sinh
    cosh  = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acosh

instance AdditiveGroup v => AdditiveGroup (Reactive v) where
    zeroV   = pure   zeroV
    (^+^)   = liftA2 (^+^)
    negateV = liftA   negateV

instance VectorSpace v => VectorSpace (Reactive v) where
    type Scalar (Reactive v) = Scalar v
    (*^) s = fmap (s *^)
    

-- |
-- A non-reactive reactive.
--
alwaysR :: a -> Reactive a
alwaysR = pure

-- |
-- Step between values.
--
stepper  :: a -> Event a -> Reactive a
stepper x e = RStep (newVar x) e

-- |
-- Switch between time-varying values.
--
-- switcher :: Reactive a -> Event (Reactive a) -> Reactive a
-- r `switcher` e = RJoin (r `stepper` e)
-- r `switcher` e = join (r `stepper` e)

-- |
-- Step between values without initial.
--
maybeStepper :: Event a -> Reactive (Maybe a)
maybeStepper e = Nothing `stepper` fmap Just e

-- |
-- Switch between time-varying values without initial.
--
-- maybeSwitcher :: Event (Reactive a) -> Reactive (Maybe a)
-- maybeSwitcher e = pure Nothing `switcher` fmap (fmap Just) e

-- |
-- Step between values without initial, failing if sampled before the first step.
--
eventToReactive :: Event a -> Reactive a
eventToReactive = stepper (error "eventToReactive: ")

-- |
-- Switch between the values of a time-varying value when an event occurs.
--
-- sampleAndHold :: Reactive b -> Event a -> Reactive b
-- sampleAndHold r e = r `switcher` (pure <$> r `sample` e) 
-- sampleAndHold r e = (liftA2 change) r (maybeStepper $ sample r e)
--     where
--         change a Nothing  = a
--         change a (Just b) = b

sampleAndHold2 :: b -> Reactive b -> Event a -> Reactive b
sampleAndHold2 z r e = z `stepper` (r `sample` e) 


-- | 
-- Apply the values of an event to a time-varying function.
--
-- > r `apply` e = r `snapshotWith ($)` e
-- 
apply :: Reactive (a -> b) -> Event a -> Event b
r `apply` e = r `o` e where o = snapshotWith ($)

-- |
-- Sample a time-varying value.
--
-- > r `snapshot` e = snapshotWith const
--
sample :: Reactive b -> Event a -> Event b
sample = ESamp

-- |
-- Sample a time-varying value with the value of the trigger.
--
-- > r `snapshot` e = snapshotWith (,)
--
snapshot :: Reactive a -> Event b -> Event (a, b)
snapshot = snapshotWith (,)

-- |
-- Sample a time-varying value with the value of the trigger, using the given 
-- function to combine.
--
-- > r `snapshotWith f` e = (f <$> r) `apply` e
--
snapshotWith :: (a -> b -> c) -> Reactive a -> Event b -> Event c
snapshotWith f r e = sample (liftA2 f r (eventToReactive e)) e


-- | 
-- Filter an event based on a time-varying predicate.
-- 
-- > r `filter'` e = justE $ (partial <$> r) `apply` e
--
filter' :: Reactive (a -> Bool) -> Event a -> Event a
r `filter'` e = justE $ (partial <$> r) `apply` e

-- | 
-- Filter an event based on a time-varying toggle.
-- 
-- > r `gate` e = (const <$> r) `filter'` e
-- 
gate :: Reactive Bool -> Event a -> Event a
r `gate` e = (const <$> r) `filter'` e


-- | 
-- Efficient combination of 'accumE' and 'accumR'.
-- 
mapAccum :: a -> Event (a -> (b,a)) -> (Event b, Reactive a)
mapAccum acc ef = (fst <$> e, stepper acc (snd <$> e))
    where 
        e = accumE (emptyAccum,acc) ((. snd) <$> ef)
        emptyAccum = error "mapAccum: Empty accumulator"

-- |
-- Combine reactives. See also 'eitherE'.
--
zipR :: Reactive a -> Reactive b -> Reactive (a, b)
zipR = liftA2 (,)

-- |
-- Reactive accumulator.
--
-- > a `accumE` e = (a `accumR` e) `sample` e
-- > a `accumR` e = a `stepper` (a `accumE` e)
--        
accumR :: a -> Event (a -> a) -> Reactive a
accumR x = RAccum (newVar x)

-- |
-- Create a past-dependent reactive. This combinator corresponds to 'scanl' on streams.
--
-- > scanlR f z x = foldpR (flip f) f z x
--        
foldpR :: (a -> b -> b) -> b -> Event a -> Reactive b
foldpR f = scanlR (flip f)

-- |
-- Create a past-dependent reactive. This combinator corresponds to 'scanl' on streams.
--
-- > scanlR f z x = foldpR (flip f) f z x
--        
scanlR :: (a -> b -> a) -> a -> Event b -> Reactive a
scanlR f a e = a `stepper` scanlE f a e

-- |
-- Create a past-dependent event using a 'Monoid' instance.
--
monoidR :: Monoid a => Event a -> Reactive a
monoidR = scanlR mappend mempty

liftMonoidR :: Monoid m => (a -> m) -> (m -> a) -> Event a -> Reactive a
liftMonoidR i o = fmap o . monoidR . fmap i

sumR :: Num a => Event a -> Reactive a
sumR = liftMonoidR Sum getSum

productR :: Num a => Event a -> Reactive a
productR = liftMonoidR Product getProduct

allR :: Event Bool -> Reactive Bool
allR = liftMonoidR All getAll

anyR :: Event Bool -> Reactive Bool
anyR = liftMonoidR Any getAny

-- |
-- Count values.
--
countR :: Enum b => Event a -> Reactive b
countR = accumR (toEnum 0) . fmap (const succ)



onR :: Event a -> Reactive Bool
onR = fmap isJust . maybeStepper

offR :: Event a -> Reactive Bool
offR = fmap not . onR

toggleR :: Event a -> Reactive Bool
toggleR = fmap odd . countR

-- |
-- Difference of successive values.
--
diffE :: Num a => Event a -> Event a
diffE = recallEWith $ flip (-)



-- |
-- A generalized time behaviour.
--
time :: Fractional a => Reactive a
time = accumR 0 ((+ kStdPulseInterval) <$ kStdPulse)


-- |
-- Integrates a behaviour.
--
-- > integral pulse behavior
--
integral :: Fractional b => Event a -> Reactive b -> Reactive b
integral t b = sumR (snapshotWith (*) b (diffE (tx `sample` t)))
    where
        -- tx = time
        tx :: Fractional a => Reactive a
        tx = fmap (fromRational . toRational) $ systemTimeSecondsR



data TransportControl t 
    = Play      -- ^ Play from the current position.
    | Reverse   -- ^ Play in reverse from the current position.
    | Pause     -- ^ Stop playing, and retain current position.
    | Stop      -- ^ Stop and reset position.
    deriving (Eq, Ord, Show)
--    | Seek t    -- ^ Set current position.

isStop Stop = True
isStop _    = False

-- |
-- Generates a cursor that moves forward or backward continuously.
--
-- The cursor may be started, stopped, moved by sending a 'TransportControl' event.
--
-- > transport control pulse speed
--
transport :: (Ord t, Fractional t) => Event (TransportControl t) -> Event a -> Reactive t -> Reactive t
transport ctrl trig speed = position'
    where          
        -- action :: Reactive (TransportControl t)
        action    = Pause `stepper` ctrl

        -- direction :: Num a => Reactive a
        direction = action <$$> \a -> case a of
            Play     -> 1
            Reverse  -> (-1)
            Pause    -> 0         
            Stop     -> 0         
            
        -- position :: Num a => Reactive a
        position = integral trig (speed * direction)
        -- startPosition = position `sampleAndHold` (filterE isStop ctrl)
        startPosition = sampleAndHold2 0 position (filterE isStop ctrl)

        position'     = position - startPosition


-- |
-- Record a list of values.
--
record :: Ord t => Reactive t -> Event a -> Reactive [(t, a)]
record t x = foldpR append [] (t `snapshot` x)
    where
        append x xs = xs ++ [x]

-- |
-- Play back a list of values.
--
-- This function will sample the time behaviour at an arbitrary
-- small interval. To get precise control of how time is sampled,
-- use 'playback'' instead.
-- 
playback :: Ord t => Reactive t -> Reactive [(t,a)] -> Event a
playback t s = scatterE $ fmap snd <$> playback' oftenE t s

-- |
-- Play back a list of values.
-- 
playback' :: Ord t => Event b -> Reactive t -> Reactive [(t,a)] -> Event [(t, a)]
playback' p t s = cursor s (t `sample` p)
    where                             
        -- cursor :: Ord t => Reactive [(t,a)] -> Event t -> Event [(a,t)]
        cursor s = snapshotWith (flip occs) s . recallE

        -- occs :: Ord t => (t,t) -> [(a,t)] -> [(a,t)]
        occs (x,y) = filter (\(t,_) -> x < t && t <= y)


{-

modify   :: Event (a -> a) -> Reactive a -> Reactive a
set      :: Event a        -> Reactive b -> Reactive a
-}

       
-------------------------------------------------------------------------------------
-- Lifting IO etc
-------------------------------------------------------------------------------------

-- |
-- Event reading from external world.
-- The computation should be blocking and is polled exactly once per value.
--
-- This function can be used with standard I/O functions.
--
getE :: IO a -> Event a
getE k = unsafePerformIO $ do
    ch <- newChan
    forkIO $ cycleM $ 
        k >>= writeChan ch
    return (EChan ch)

-- |
-- Event reading from external world.
-- The computation should be non-blocking and may be polled repeatedly for each value.
--
-- This function should be used with /non-effectful/ functions, typically functions that
-- observe the current value of some external property.
-- You should /not/ use this function with standard I/O functions as this
-- may lead to non-deterministic reads (i.e. loss of data).
--
pollE :: IO (Maybe a) -> Event a
pollE = ESource . fmap maybeToList

-- Event interacting with the external world.
-- The computation should be non-blocking and its values will be contested.
--
-- modifyE :: (a -> IO b) -> Event a -> Event b
-- modifyE = ESink

-- |
-- Event writing to the external world.
--
-- This function can be used with standard I/O functions.
--
putE :: (a -> IO ()) -> Event a -> Event a
putE k = ESink $ \x -> do
    k x
    return x

-- |
-- Event reading from a channel.
--
readChanE :: Chan a -> Event a
readChanE = EChan

-- |
-- Event writing to a channel.
--
writeChanE :: Chan a -> Event a -> Event a
writeChanE ch e = ESink (writeChan ch) e `seqE` e

-- |
-- Event version of 'getChar'.
--
getCharE :: Event Char
getCharE = getE getChar 

-- |
-- Event version of 'putChar'.
--
putCharE :: Event Char -> Event Char
putCharE = putE putChar

-- |
-- Event version of 'getLine'.
--
getLineE :: Event String
getLineE = getE getLine 

-- |
-- Event version of 'putStrLn'.
--
putLineE :: Event String -> Event String
putLineE = putE putStrLn

systemTimeR :: Reactive UTCTime 
systemTimeR = eventToReactive $ pollE (Just <$> getCurrentTime)

systemTimeSecondsR :: Reactive DiffTime
systemTimeSecondsR = fmap utctDayTime systemTimeR

systemTimeDayR :: Reactive Day
systemTimeDayR = fmap utctDay systemTimeR


-- | 
-- An event occuring at the specified interval.
--
pulse :: DiffTime -> Event ()
pulse t = getE $ threadDelay (round (fromMicro t))
    where           
        fromMicro = (* 1000000)

-------------------------------------------------------------------------------------
-- Running
-------------------------------------------------------------------------------------

-- | 
-- Run the given event once.
--
run :: Event a -> IO ()
run e = do
    f <- prepE e
    runE f
    return ()

-- | 
-- Run the given event for ever.
--
runLoop :: Event a -> IO ()
runLoop e = do 
    f <- prepE e
    runLoop' f  
    where   
        runLoop' g = do
            runE g
            threadDelay kLoopInterval >> runLoop' g

-- | 
-- Run the given event until the first @Just x@  value, then return @x@.
--
runLoopUntil :: Event (Maybe a) -> IO a
runLoopUntil e = do 
    f <- prepE e
    runLoopUntil' f  
    where   
        runLoopUntil' g = do
            r <- runE g
            case (catMaybes r) of 
                []    -> threadDelay kLoopInterval >> runLoopUntil' g
                (a:_) -> return a




-------------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------------

type Source a = Event a
type Sink a   = Event a -> Event ()

-- |
-- Behaves like the original event but writes a given message to the standard
-- output for each value.
-- 
notify :: String -> Event a -> Event a
notify m x = putLineE (fmap (const m) x) `seqE` x

-- |
-- Behaves like the original event but writes its value, prepended by the given
-- message, for each value.
-- 
showing :: Show a => String -> Event a -> Event a
showing m x = putE k x
    where
        k x = putStrLn $ m ++ show x

-- |
-- Creates a new source and a computation that writes  it.
-- 
newSource :: IO (a -> IO (), Source a)
newSource = do
    ch <- newChan
    return (writeChan ch, readChanE ch)

-- |
-- Creates a new sink and a computation that reads from it.
-- 
newSink :: IO (IO (Maybe a), Sink a)
newSink = do
    ch <- newChan
    return (tryReadChan ch, tickE . writeChanE ch)  

runEvent :: Show a => Event a -> IO ()
runEvent = runLoop . showing ""

runReactive :: Show a => Reactive a -> IO ()
runReactive r = runEvent (r `sample` pulse (1/20))

unsafeGetReactive :: Reactive a -> a
unsafeGetReactive r = unsafePerformIO $ runRS r


-------------------------------------------------------------------------------------

partial :: (a -> Bool) -> (a -> Maybe a)
partial p x
    | p x       = Just x
    | otherwise = Nothing

list z f [] = z
list z f xs = f xs

filterMap p = catMaybes . map p   

cycleM x = x >> cycleM x 

single x = [x]

-- | Pass through @Just@ occurrences.
joinMaybes :: MonadPlus m => m (Maybe a) -> m a
joinMaybes = (>>= maybe mzero return)

-- | Pass through values satisfying @p@.
filterMP :: MonadPlus m => (a -> Bool) -> m a -> m a
filterMP p m = joinMaybes (liftM f m)
 where
   f a | p a        = Just a
       | otherwise  = Nothing

fromLeft  (Left  a) = Just a
fromLeft  (Right b) = Nothing
fromRight (Left  a) = Nothing
fromRight (Right b) = Just b                         

noFun = noOverloading "Reactive"
noOverloading ty meth = error $ meth ++ ": No overloading for " ++ ty


kStdPulseInterval :: Fractional a => a
kStdPulseInterval = (1/20)

kLoopInterval = round $ (1/10) * 1000000 -- us

kStdPulse = pulse kStdPulseInterval

(<$$>) = flip fmap


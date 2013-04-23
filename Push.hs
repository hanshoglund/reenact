
{-# LANGUAGE BangPatterns, FlexibleInstances #-}

module Main where
import Data.Monoid
import Control.Concurrent (threadDelay)
import Data.Maybe
import Data.Either
import Control.Applicative
import Data.Traversable hiding (mapM)
import Data.IORef
import qualified Data.Map as Map
import System.IO.Unsafe
import Unsafe.Coerce

{-
    See
        http://www.haskellforall.com/2012/12/the-continuation-monad.html
        http://paolocapriotti.com/blog/2012/06/04/continuation-based-relative-time-frp/
-}

-- | 
-- An event broadcasts (allows subscription) of input handlers.
--
newtype EventT m r a = E { runE :: (a -> m r) -> m r -> m r }
    -- arg 1 is the handler to register
    -- arg 2 is the continuation run while the handler is registered
    
-- | 
-- A reactive is an output together with a start and stop action.
--
newtype ReactiveT m r a = R { runR :: (m a -> m r) -> m r }
    -- arg 1 is the continuation run while the handler is registered
        -- this one has an output to sample the current value in scope

type Event    = EventT IO ()
type Reactive = ReactiveT IO ()

-- Note: for non-GHC, do not use EventT etc

--------------------------------------------------------------------------------
-- Prim

emptyPrim    :: Event a
unionPrim    :: Event a -> Event a -> Event a
scatterPrim  :: Event [a] -> Event a
mapPrim      :: (a -> b) -> Event a -> Event b

constPrim    :: a -> Reactive a
applyPrim    :: Reactive (a -> b) -> Reactive a -> Reactive b

stepperPrim  :: a -> Event a -> Reactive a
accumPrim    :: a -> Event (a -> a) -> Reactive a
snapshotPrim :: (a -> b -> c) -> Reactive a -> Event b -> Event c
-- joinPrim     :: Reactive (Reactive a) -> Reactive a

-- Handlers on empty are just ignored
emptyPrim = E $
    \_ k -> k

-- Handlers on (a <> b) are registered on both a and b
unionPrim (E a) (E b) = E $
    \h k -> a h (b h k)

-- Handlers on (f <$> a) are composed with f and registered on a
mapPrim f (E a) = E $
    \h k -> a (h . f) k

-- Handlers on (scatterEvent a) are composed with traverse and registered on a
scatterPrim (E a) = E $
    \h k -> a (mapM_ h) k

-- Registering handlers on snapshot will start the reactive, and register a modified handler on e
-- The modified handler pushes values from the reactive
-- Unregistering handlers on snapshot will stop the reactive
snapshotPrim f (R r) (E e) = E $ 
    \h k -> let h' x y = do { x' <- x; h $ f x' y }
            in r $ \x -> e (h' x) k
    
-- Starting a stepper registers a handler on the event that modifies a variable
-- Values are pulled from the variable
-- Stopping it unregisters the handler
accumPrim a (E e) = R $
    \k -> do
        putStrLn $ "----> Initializing accumulator to " ++ show (unsafeCoerce a::Int)
        v <- newIORef a
        e (modifyIORef v) $ k (readIORef v)

-- stepperPrim a e = accumPrim a (fmap const e)
stepperPrim a (E e) = R $
    \k -> do
        putStrLn $ "----> Initializing stepper to " ++ show (unsafeCoerce a::Int)
        v <- newIORef a
        e (writeIORef v) $ k (readIORef v)
    
constPrim a = R $ \k -> k (pure a)
applyPrim (R f) (R a) = R $ \k -> f (\f' -> a (\a' -> k $ f' <*> a'))


-- seqE
-- runLoop
-- getE
-- putE
-- getE
-- pollR TODO change mainline
         

newSource :: IO (a -> IO (), Event a)
newSource = do
    -- putStrLn "----> Source created"
    r <- newIORef (0,Map.empty)

    let write = \x -> do
        (_,hs) <- readIORef r
        -- putStrLn $ "----> Input, num handlers: " ++ show (Map.size hs)
        traverse (\h -> h x) hs
        return ()

    let register = \h k -> do
        -- putStrLn "----> Registered handler"
        (n,hs) <- readIORef r
        let hs' = Map.insert n h hs
        writeIORef r (n+1,hs')
        k
        -- putStrLn "----> Unregistered handler"
        (_,hs2) <- readIORef r
        let hs2' = Map.delete n hs2
        writeIORef r (n,hs2')
    
    return (write, E register)

newSink :: IO (IO (Maybe a), Event a -> Event ())
newSink = undefined




--------------------------------------------------------------------------------
-- API

instance Monoid (Event a) where
    mempty = emptyPrim
    mappend = unionPrim
instance Functor Event where
    fmap = mapPrim
instance Functor Reactive where
    fmap f = (pure f <*>)
instance Applicative Reactive where
    pure = constPrim
    (<*>) = applyPrim
-- instance Monad (ReactiveT IO ()) where
    -- return = constPrim
    -- x >>= k = (joinPrim . fmap k) x

filterE :: (a -> Bool) -> Event a -> Event a
filterE p = scatterE . fmap (filter p . single)

justE :: Event (Maybe a) -> Event a
justE = scatterE . fmap maybeToList

splitE :: Event (Either a b) -> (Event a, Event b)
splitE e = (justE $ fromLeft <$> e, justE $ fromRight <$> e)

eitherE :: Event a -> Event b -> Event (Either a b)
a `eitherE` b = (Left <$> a) <> (Right <$> b)

-- zipE :: (a, b) -> (Event a, Event b) -> E (a, b)
-- zipE = undefined

unzipE :: Event (a, b) -> (Event a, Event b)
unzipE e = (fst <$> e, snd <$> e)

replaceE :: b -> Event a -> Event b
replaceE x = (x <$)

accumE :: a -> Event (a -> a) -> Event a
a `accumE` e = (a `accumR` e) `sample` e

foldpE :: (a -> b -> b) -> b -> Event a -> Event b
foldpE f a e = a `accumE` (f <$> e)

scanlE :: (a -> b -> a) -> a -> Event b -> Event a
scanlE f = foldpE (flip f)
        
monoidE :: Monoid a => Event a -> Event a
monoidE = scanlE mappend mempty

sumE :: Num a => Event a -> Event a
sumE = over monoidE Sum getSum

productE :: Num a => Event a -> Event a
productE = over monoidE Product getProduct

allE :: Event Bool -> Event Bool
allE = over monoidE All getAll

anyE :: Event Bool -> Event Bool
anyE = over monoidE Any getAny

firstE :: Event a -> Event a
firstE = justE . fmap snd . foldpE g (True,Nothing)
    where
        g c (True, _)  = (False,Just c)
        g c (False, _) = (False,Nothing)
            
restE :: Event a -> Event a
restE = justE . fmap snd . foldpE g (True,Nothing)
    where        
        g c (True, _)  = (False,Nothing)
        g c (False, _) = (False,Just c)

countE :: Enum b => Event a -> Event b
countE = accumE (toEnum 0) . fmap (const succ)

lastE :: Event a -> Event a
lastE = fmap snd . recallE

delayE :: Int -> Event a -> Event a
delayE n = foldr (.) id (replicate n lastE)

bufferE :: Int -> Event a -> Event [a]
bufferE n = (reverse <$>) . foldpE g []
    where
        g x xs = x : take (n-1) xs

gatherE :: Int -> Event a -> Event [a]
gatherE n = (reverse <$>) . filterE (\xs -> length xs == n) . foldpE g []
    where
        g x xs | length xs <  n  =  x : xs
               | length xs == n  =  x : []
               | otherwise       = error "gatherE: Wrong length"

scatterE :: Event [a] -> Event a
scatterE = scatterPrim

recallE :: Event a -> Event (a, a)
recallE = recallWithE (,)

-- Note: flipped order from Reactive
recallWithE :: (a -> a -> b) -> Event a -> Event b
recallWithE f = justE . fmap combine . (dup Nothing `accumE`) . fmap (shift . Just)
    where      
        shift b (_,a) = (a,b)
        dup x         = (x,x)
        combine       = uncurry (liftA2 f)

stepper  :: a -> Event a -> Reactive a
stepper = stepperPrim

-- switcher  :: Reactive a -> Event (Reactive a) -> Reactive a
-- switcher a = joinPrim . stepper a

stepper' :: Event a -> Reactive (Maybe a)
stepper' e = Nothing `stepper` fmap Just e

hold :: Reactive a -> Event b -> Reactive (Maybe a)
hold r = hold' Nothing (fmap Just r)

hold' :: a -> Reactive a -> Event b -> Reactive a
hold' z r e = z `stepper` (r `sample` e) 

apply :: Reactive (a -> b) -> Event a -> Event b
r `apply` e = r `o` e where o = snapshotWith ($)

sample :: Reactive a -> Event b -> Event a
sample = snapshotWith const

snapshot :: Reactive a -> Event b -> Event (a, b)
snapshot = snapshotWith (,)

snapshotWith :: (a -> b -> c) -> Reactive a -> Event b -> Event c
snapshotWith = snapshotPrim

filter' :: Reactive (a -> Bool) -> Event a -> Event a
r `filter'` e = justE $ (partial <$> r) `apply` e

gate :: Reactive Bool -> Event a -> Event a
r `gate` e = (const <$> r) `filter'` e

accumR :: a -> Event (a -> a) -> Reactive a
accumR = accumPrim

mapAccum :: a -> Event (a -> (b,a)) -> (Event b, Reactive a)
mapAccum acc ef = (fst <$> e, stepper acc (snd <$> e))
    where 
        e = accumE (emptyAccum,acc) ((. snd) <$> ef)
        emptyAccum = error "mapAccum: Empty accumulator"

zipR :: Reactive a -> Reactive b -> Reactive (a, b)
zipR = liftA2 (,)

unzipR :: Reactive (a, b) -> (Reactive a, Reactive b)
unzipR r = (fst <$> r, snd <$> r)

foldpR :: (a -> b -> b) -> b -> Event a -> Reactive b
foldpR f = scanlR (flip f)

scanlR :: (a -> b -> a) -> a -> Event b -> Reactive a
scanlR f a e = a `stepper` scanlE f a e

monoidR :: Monoid a => Event a -> Reactive a
monoidR = scanlR mappend mempty

sumR :: Num a => Event a -> Reactive a
sumR = over monoidR Sum getSum

productR :: Num a => Event a -> Reactive a
productR = over monoidR Product getProduct

allR :: Event Bool -> Reactive Bool
allR = over monoidR All getAll

anyR :: Event Bool -> Reactive Bool
anyR = over monoidR Any getAny

countR :: Enum b => Event a -> Reactive b
countR = accumR (toEnum 0) . fmap (const succ)

toggleR :: Event a -> Reactive Bool
toggleR = fmap odd . countR

diffE :: Num a => Event a -> Event a
diffE = recallWithE $ flip (-)

-- time :: Fractional a => Reactive a
-- time = accumR 0 ((+ kStdPulseInterval) <$ kStdPulse)

integral :: Fractional b => Event a -> Reactive b -> Reactive b
integral t b = sumR (snapshotWith (*) b (diffE (tx `sample` t)))
    where
        -- tx = time
        tx :: Fractional a => Reactive a
        tx = fmap (fromRational . toRational) $ systemTimeSecondsR

systemTimeSecondsR = pure 0 -- FIXME
oftenE             = mempty -- FIXME


data TransportControl t 
    = Play      -- ^ Play from the current position.
    | Reverse   -- ^ Play in reverse from the current position.
    | Pause     -- ^ Stop playing, and retain current position.
    | Stop      -- ^ Stop and reset position.
    deriving (Eq, Ord, Show)

isStop Stop = True
isStop _    = False

transport :: (Ord t, Fractional t) => Event (TransportControl t) -> Event a -> Reactive t -> Reactive t
transport ctrl trig speed = position'
    where          
        -- action :: Reactive (TransportControl t)
        action    = Pause `stepper` ctrl

        -- direction :: Num a => Reactive a
        direction = (flip fmap) action $ \a -> case a of
            Play     -> 1
            Reverse  -> (-1)
            Pause    -> 0         
            Stop     -> 0         
            
        -- position :: Num a => Reactive a
        position = integral trig (speed * direction)
        startPosition = hold' 0 position (filterE isStop ctrl)

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
                                                           
noFun x = error x






-- Util
start :: Event a -> (a -> IO ()) -> IO () -> IO ()
start = runE

main = do
    -- (i1,e1) <- newSource              
    -- (i2,e2) <- newSource              
    -- 
    -- run putStrLn (e1 <> fmap (show . length) e1 <> fmap reverse e2)
    -- i1 "Hello"
    -- i1 "This"
    -- i2 "Is"
    -- i2 "Cool"
    -- i1 "Right?"
    -- i2 "Right?"

    (i1,e1) <- newSource
    (i2,e2) <- newSource
    (i3,e3) <- newSource

    -- let ev0 = e1 `eitherE` recallE e1
    -- let (ev1,ev2) = splitE ev0
    -- let ev  = fmap ((""++) . show) ev1 <> fmap (("               "++) . show) ev2

    -- let r1 = "-" `stepper` e1
    -- let r2 = fmap (flip replicate $ '.') $ 0 `stepper` (fmap length e1)
    -- let ev = liftA2 (++) r1 r2 `sample` e1
    -- 
    -- start ev putStrLn $ do
    --     i1 "Hello"
    --     sleep 0.5
    --     i1 "This"
    --     sleep 0.5
    --     i1 "Is"
    --     sleep 0.5
    --     i1 "Cool"
    --     sleep 2
    --     i1 "Right?"
    --     sleep 0.5
    --     i1 "There!"
    
    let r3 = 0 `stepper` countE e2 :: Reactive Int    
    let ev = r3 `sample` (e1 <> e2 <> e3)
    
    start (ev) (putStrLn . show) $ do
        i1 ()
        i2 ()
        i3 ()
        i2 ()
        i2 ()
        return ()





partial p x
    | p x       = Just x
    | otherwise = Nothing
list z f [] = z
list z f xs = f xs
filterMap p = catMaybes . map p   
cycleM x = x >> cycleM x 
single x = [x]
fromLeft  (Left  a) = Just a
fromLeft  (Right b) = Nothing
fromRight (Left  a) = Nothing
fromRight (Right b) = Just b                         
over f i o = fmap o . f . fmap i
sleep s = threadDelay (round $ s*1000000)

eventToReactive :: Event a -> Reactive a
eventToReactive = stepper (error "eventToReactive: ")



module Control.Reactive.Chan (
        Chan(..),
        newChan,
        dupChan,
        readChan,
        tryReadChan,
        writeChan,
  ) where

import Data.Monoid  
import Data.Maybe
import Control.Monad
import Control.Applicative

import Control.Concurrent (forkIO, forkOS, threadDelay)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TMVar

import System.IO.Unsafe

newtype Chan a = Chan { getChan :: TChan a }

newChan     :: IO (Chan a)
newChan               = do
    c' <- atomically $ newTChan
    return (Chan c')

dupChan     :: Chan a -> IO (Chan a)
dupChan (Chan c)      = do
    c' <- atomically . dupTChan $ c
    return (Chan c')    

writeChan   :: Chan a -> a -> IO ()
writeChan (Chan c) x  = atomically $ writeTChan c x

readChan    :: Chan a -> IO a
readChan  (Chan c)    = atomically $ readTChan c

tryReadChan :: Chan a -> IO (Maybe a)
tryReadChan (Chan c)  = atomically $ tryReadTChan c

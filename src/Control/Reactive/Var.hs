
module Control.Reactive.Var (
        Var(..),
        dupVar,
        newVar,
        readVar,
        swapVar
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

newtype Var a = Var { getVar :: TMVar a }

newVar :: a -> Var a
newVar = Var . unsafePerformIO . newTMVarIO

dupVar :: Var a -> IO (Var a)
dupVar v = atomically $ readTMVar (getVar v) >>= newTMVar >>= return . Var

readVar :: Var a -> IO a
readVar = atomically . readTMVar . getVar

swapVar :: Var a -> a -> IO a
swapVar v = atomically . swapTMVar (getVar v)


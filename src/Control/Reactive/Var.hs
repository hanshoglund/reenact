
{-# LANGUAGE BangPatterns #-}

module Control.Reactive.Var (
        Var(..),
        dupVar,
        newVar,
        readVar,
        writeVar,
        -- swapVar
  ) where

import Data.Monoid  
import Data.Maybe
import Control.Monad
import Control.Applicative

import Control.Concurrent (forkIO, forkOS, threadDelay)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar

import System.IO.Unsafe

newtype Var a = Var { getVar :: TVar a }

newVar :: a -> Var a
newVar = Var . unsafePerformIO . newTVarIO

dupVar :: Var a -> IO (Var a)
dupVar v = atomically $ readTVar (getVar v) >>= newTVar >>= return . Var

readVar :: Var a -> IO a
readVar = atomically . readTVar . getVar

writeVar :: Var a -> a -> IO ()
-- writeVar (Var v) x = atomically $ modifyTVar' v (const x)
writeVar (Var !v) x = atomically $ swapTVar v x >> return ()

-- swapVar :: Var a -> a -> IO a
-- swapVar v = atomically . swapTVar (getVar v)
-- 

module Resolve.DNS.Truncation where

import Resolve.Types
import Resolve.DNS.Types
import Resolve.DNS.EDNS.Types
import qualified Resolve.DNS.Transport.UDP as UDP

import Control.Monad
import Control.Monad.STM
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Exception



import Control.Concurrent.STM.TVar

import System.Log.Logger

nameM = "Resolve.DNS.Truncation"



truncation :: Config -> IO (Resolve [Question] Message)
truncation c = do
  return (\a -> do 
             p <- atomically $ do
               this <- readTVar (p_this c)
               that <- readTVar p_that
               return (if this < that then this else that)
             setPayload c p

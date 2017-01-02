module Resolve.Timeout where

import Resolve.Types
import Resolve.Exceptions
import Control.Concurrent
import Control.Exception

data Timeout = Timeout
  deriving (Show)

instance Exception Timeout where
  toException = errorToException
  fromException = errorFromException

timeout :: Int -> Resolve a b -> Resolve a b
timeout n r a = do 
  pid <- myThreadId
  bracket
    (forkIOWithUnmask $ \unmask ->
        unmask $ threadDelay n >> throwTo pid Timeout)
    (uninterruptibleMask_ . killThread)
    (\_ -> r a)

module Resolve.Retry where

import Resolve.Types

import Data.Typeable
import Control.Exception

import System.Log.Logger

data RetryOut = RetryOut
  deriving (Show, Typeable)

instance Exception RetryOut

nameM = "Resolve.Retry"

retry :: Int -> Resolve a b -> Resolve a b
retry n r a =
  let loop i = if i == 0 then throw RetryOut
        else do
        m <- try (r a)
        case m of
          Left e -> do
            debugM nameM $ "[" ++ (show i) ++ "/" ++ (show n) ++ "] " ++ (show (e :: SomeException))
            loop (i - 1)
          Right b -> return b
  in loop n

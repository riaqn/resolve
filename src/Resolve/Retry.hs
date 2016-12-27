module Resolve.Retry where

import Resolve.Types

import Data.Typeable
import Control.Exception

data RetryOut = RetryOut
  deriving (Show, Typeable)

instance Exception RetryOut

retry :: Int -> Resolve a b -> Resolve a b
retry n r a =
  let loop n = if n == 0 then throw RetryOut
        else do
        m <- try (r a)
        case m of
          Left e -> do
            let x = e :: SomeException
            loop (n - 1)
          Right b -> return b
  in loop n

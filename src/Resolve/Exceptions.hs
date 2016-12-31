module Resolve.Exceptions where

import Data.Typeable
import Control.Exception


data Error where
  Error :: Exception e => e -> Error
  deriving (Typeable)

instance Show Error where
  show (Error e) = show e

instance Exception Error

errorToException :: Exception e => e -> SomeException
errorToException = toException . Error

errorFromException x = do
  Error a <- fromException x
  cast a

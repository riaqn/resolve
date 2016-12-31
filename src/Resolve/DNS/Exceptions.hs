module Resolve.DNS.Exceptions where

import qualified Resolve.Exceptions as R

import Control.Exception
import Data.Typeable

data Error where
  Error :: Exception e => e -> Error
  deriving (Typeable)

instance Show Error where
  show (Error e) = show e

instance Exception Error where
  toException = R.errorToException
  fromException = R.errorFromException

errorToException :: Exception e => e -> SomeException
errorToException = toException . Error

errorFromException x = do
  Error a <- fromException x
  cast a





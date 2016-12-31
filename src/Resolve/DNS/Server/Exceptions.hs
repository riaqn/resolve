module Resolve.DNS.Server.Exceptions where

import qualified Resolve.DNS.Exceptions as DNS

import Data.Typeable
import Control.Exception

data Error where
  Error :: Exception e => e -> Error
  deriving  (Typeable)

instance Show Error where
  show (Error e) = show e

instance Exception Error where
  toException = DNS.errorToException
  fromException = DNS.errorFromException

errorToException :: Exception e => e -> SomeException
errorToException = toException . Error

errorFromException x = do
  Error a <- fromException x
  cast a

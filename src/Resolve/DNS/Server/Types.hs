module Resolve.DNS.Server.Types where

import Resolve.DNS.Types

import Data.Typeable
import Control.Exception

data ServerException where
  ServerException :: Exception e => e -> ServerException
  deriving  (Typeable)

instance Show ServerException where
  show (ServerException e) = show e

instance Exception ServerException where
  toException = dnsExceptionToException
  fromException = dnsExceptionFromException

serverExceptionToException :: Exception e => e -> SomeException
serverExceptionToException = toException . ServerException

serverExceptionFromException x = do
  ServerException a <- fromException x
  cast a

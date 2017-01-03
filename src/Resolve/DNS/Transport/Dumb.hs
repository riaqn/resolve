module Resolve.DNS.Transport.Dumb where

import Resolve.DNS.Transport.Types
import qualified Resolve.DNS.Transport.Exceptions as E

import Control.Exception
import Data.Typeable

data Error = NonExistTransport
  deriving (Typeable, Show)

instance Exception Error where
  toException = E.errorToException
  fromException = E.errorFromException

dumb :: Transport
dumb = Transport { send = \_ -> throwIO NonExistTransport
                 , recv = throwIO NonExistTransport
                 , delete = return ()
                 }

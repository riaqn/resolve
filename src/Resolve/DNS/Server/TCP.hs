module Resolve.DNS.Server.TCP where

import Resolve.Types

import Resolve.DNS.Types
import Resolve.DNS.Server.Types
import Data.ByteString
import qualified Data.ByteString.Lazy as BSL
import Data.Typeable

import qualified Resolve.DNS.Decode as D
import qualified Resolve.DNS.Encode as E

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Exception



data DecodeError = DecodeError String
  deriving (Typeable, Show)

instance Exception DecodeError where
  toException = serverExceptionToException
  fromException = serverExceptionFromException

data EncodeError = EncodeError String
  deriving (Typeable, Show)

instance Exception EncodeError where
  toException = serverExceptionToException
  fromException = serverExceptionFromException

encodeMessage :: Message -> BSL.ByteString
encodeMessage m = case E.encode E.message m of
  Left e -> throw $ EncodeError e
  Right b -> b

resolve :: Resolve Message Message -> Resolve BSL.ByteString BSL.ByteString
resolve r a = do
  a' <- case D.decodeMessage (BSL.toStrict a) of
    Left e -> throw $ DecodeError e
    Right a' -> return a'
  b' <- r a'

  return $ encodeMessage b'

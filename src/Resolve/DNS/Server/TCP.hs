module Resolve.DNS.Server.TCP where

import Resolve.Types

import Resolve.DNS.Types
import Resolve.DNS.Server.Exceptions
import Data.ByteString
import qualified Data.ByteString.Lazy as BSL
import Data.Typeable

import qualified Resolve.DNS.Decode as D
import qualified Resolve.DNS.Encode as E

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Exception


encodeMessage :: Message -> BSL.ByteString
encodeMessage m = case E.encode E.message m of
  Left e -> throw e
  Right b -> b

resolve :: Resolve Message Message -> Resolve BSL.ByteString BSL.ByteString
resolve r a = do
  a' <- case D.decodeMessage (BSL.toStrict a) of
    Left e -> throw $ D.Error e
    Right a' -> return a'
  b' <- r a'

  return $ encodeMessage b'

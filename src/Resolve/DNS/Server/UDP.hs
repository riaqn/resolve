module Resolve.DNS.Server.UDP where

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

data ResponseTooLong = ResponseTooLong
  deriving (Typeable, Show)

instance Exception ResponseTooLong where
  toException = serverExceptionToException
  fromException = serverExceptionFromException

maxLength = 512

encodeMessage :: Message -> BSL.ByteString
encodeMessage m = case E.encode E.message m of
  Left e -> throw $ EncodeError e
  Right b -> b

resolve :: Resolve Message Message -> Resolve ByteString ByteString
resolve r a = do
  a' <- case D.decodeMessage a of
    Left e -> throw $ DecodeError e
    Right a' -> return a'
  b' <- r a'

  let b = encodeMessage b'

  BSL.toStrict <$> if (BSL.null $ BSL.drop maxLength b) then
        return $ b
    else do
    let b_tc = encodeMessage $ b' { header = (header b') { tc = True }
                                  , question = question b'
                                  , answer = []
                                  , authority = []
                                  , additional = []
                                 }
    if (BSL.null $ BSL.drop maxLength b_tc) then
      return $ b_tc
      else
      throw ResponseTooLong

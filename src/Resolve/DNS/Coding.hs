module Resolve.DNS.Coding where

import Data.ByteString.Lazy
import Resolve.Types
import Resolve.DNS.Types

import qualified Resolve.DNS.Encode as E
import qualified Resolve.DNS.Decode as D

import Control.Exception

decode :: Resolve ByteString ByteString -> Resolve Message Message
decode r a = do
  bs_a <- case E.encode E.message a of
    Left e -> throwIO e
    Right bs -> return bs
  bs_b <- r bs_a
  
  case D.decodeMessage (toStrict bs_b) of
    Left e -> throwIO $ D.Error e
    Right b -> return b


encode :: Resolve Message Message -> Resolve ByteString ByteString
encode r bs_a = do
  a <- case D.decodeMessage (toStrict bs_a) of
    Left e -> throwIO $ D.Error e
    Right a -> return a

  b <- r a 
  
  case E.encode E.message a of
    Left e -> throwIO e
    Right bs -> return bs

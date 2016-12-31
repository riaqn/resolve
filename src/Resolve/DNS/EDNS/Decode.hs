module Resolve.DNS.EDNS.Decode where

import Prelude hiding (take)

import qualified Resolve.DNS.EDNS.Types as T
import qualified Resolve.DNS.EDNS.Exceptions as E
import Resolve.DNS.Types 
import qualified Resolve.DNS.Encode as DNSE
import qualified Resolve.DNS.EDNS.Exceptions
import Control.Monad.Trans.Except
import Control.Exception
import Data.Typeable

import Data.Attoparsec.ByteString hiding (option)
import Data.Attoparsec.Binary
import Data.Word
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

data Error = NameNotRoot
           | OptionParseError String
           | NotOPT
           deriving (Show, Typeable)

instance Exception Error where
  toException = E.errorToException
  fromException = E.errorFromException

opt :: RR -> Either Error T.OPT
opt rr = do
  let check p e = if p then Right () else Left e
  (c, d) <- case (rdata rr) of 
    RR_OTHER c 41 d -> return (c, d)
    _ -> Left NotOPT
  let t = ttl rr

  check (name rr == rootName) NameNotRoot



  case parseOnly options d of
    Left e -> Left $ OptionParseError e
    Right opts -> return $ T.OPT { T.udp_size = (fromCLASS c)
                                 , T.ext_rcode = fromIntegral $ (t `shiftR` 24) .&. 0xff
                                 , T.version = fromIntegral $ (t `shiftR` 16) .&. 0xff
                                 , T.dnssec_ok = testBit t 15
                                 , T.options = opts
                                 }

options :: Parser [(Word16, ByteString)]
options = do
  a <- many' option
  endOfInput
  return a

option :: Parser (Word16, ByteString)
option = do
  code <- anyWord16be
  length <- anyWord16be
  dat <- take $ fromIntegral length
  return (code, dat)

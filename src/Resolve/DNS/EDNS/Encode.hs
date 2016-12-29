module Resolve.DNS.EDNS.Encode where

import qualified Resolve.DNS.EDNS.Types as T
import Resolve.DNS.Types
import Resolve.DNS.Utils
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Data.ByteString.Builder
import Data.Word

import Data.Bits

data Error = DataTooLong
  deriving (Show)

opt :: T.OPT -> Either Error RR
opt opt = do
  opt' <- options (T.options opt)
  return $ RR { name = rootName
              , ttl = (fromIntegral $ T.ext_rcode opt) `shift` 24
                      .|. (fromIntegral $ T.version opt) `shift` 16
                      .|. (fromIntegral $ fromEnum $ T.dnssec_ok opt) `shift` 15
              , rdata = RR_OTHER (toCLASS $ T.udp_size opt) 41 (BSL.toStrict $ toLazyByteString opt')
              }

options :: [(Word16, ByteString)] -> Either Error Builder
options opts = mconcat <$> (mapM option opts)

option :: (Word16, ByteString) -> Either Error Builder
option (code, dat) = mconcat <$> (sequence $
  [ Right $ word16BE code
  , case safeFromIntegral $ BS.length dat of
      Nothing -> Left DataTooLong
      Just len -> Right $ word16BE len
  , Right $ byteString dat
  ])

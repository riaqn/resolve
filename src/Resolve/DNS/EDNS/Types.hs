module Resolve.DNS.EDNS.Types where

import Data.Word
import Data.ByteString

data OPT = OPT { udp_size :: Word16
               , ext_rcode :: Word8
               , version :: Word8
               , dnssec_ok :: Bool
               , options :: [(Word16, ByteString)]
               }
           deriving (Show, Eq)

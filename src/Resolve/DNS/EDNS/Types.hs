module Resolve.DNS.EDNS.Types where

import Data.Word
import Data.ByteString
import Control.Exception
import Data.Typeable

type Option = (Word16, ByteString)

data OPT = OPT { udp_size :: Word16
               , ext_rcode :: Word8
               , version :: Word8
               , dnssec_ok :: Bool
               , options :: [Option]
               }
           deriving (Show, Eq)


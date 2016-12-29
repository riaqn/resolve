module Resolve.DNS.Types where

import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Tuple (swap)
import Data.Maybe
import Data.Bits
import Data.BitVector
import Data.List
import Data.Typeable


import Resolve.Types

import Control.Exception

data DNSException where
  DNSException :: Exception e => e -> DNSException
  deriving (Typeable)

instance Show DNSException where
  show (DNSException e) = show e

instance Exception DNSException where
  toException = resolveExceptionToException
  fromException = resolveExceptionFromException

dnsExceptionToException :: Exception e => e -> SomeException
dnsExceptionToException = toException . DNSException

dnsExceptionFromException x = do
  DNSException a <- fromException x
  cast a

type LABEL = ByteString
newtype NAME = NAME [LABEL] deriving (Eq, Ord)

qclass_word = map (\(a, b) -> (CLASS a, b)) class_word
              ++ [(ANY, 5)]
              :: [(QCLASS, Word16)]

class_word = [ (IN, 1)
             , (CH, 3)
             , (HS, 4)
             ] :: [(CLASS, Word16)]

qtype_word = [ (Q_A, 1 :: Word16)
             , (Q_NS, 2)
             , (Q_CNAME, 5)
             , (Q_SOA, 6)
             , (Q_PTR, 12)
             , (Q_MX, 15)
             , (Q_TXT, 16)
             ]

opcode_word = map (\(a, b) -> (a, bitVec 4 b))
  [ (STD, bitVec 4 0)
  , (INV, bitVec 4 1)
  , (SSR, bitVec 4 2)
  ]

rcode_word = map (\(a, b) -> (a, bitVec 4 b))
  [ (NoErr, 0)
  , (FormatErr, 1)
  , (ServFail, 2)
  , (NameErr, 3)
  , (NotImpl, 4)
  , (Refused, 5)
  ]

qr_word = [ (Query, False)
          , (Response, True)]
             
data Message = Message {
    header     :: Header
  , question   :: [Question]
  , answer     :: [RR]
  , authority  :: [RR]
  , additional :: [RR]
  } deriving (Eq, Show)

data Header = Header { ident :: Word16
                     , qr     :: QR
                     , opcode :: OPCODE
                     , aa     :: Bool
                     , tc     :: Bool
                     , rd     :: Bool
                     , ra     :: Bool
                     , zero   :: Word8 -- should be Word3
                     , rcode  :: RCODE
                     } deriving (Eq, Show)

data QR = Query | Response deriving (Eq, Show)

toBool :: QR -> Bool
toBool Query = False
toBool Response = True

data OPCODE
  = STD
  | INV
  | SSR
  deriving (Eq, Show, Bounded)

data RCODE
  = NoErr
  | FormatErr
  | ServFail
  | NameErr
  | NotImpl
  | Refused
  | Other Word8 -- actually should be Word4
  deriving (Eq, Ord, Show)

data CLASS = IN
           | CH
           | HS
           | OTHER Word16
           deriving (Eq, Show, Ord)

data QCLASS = CLASS CLASS
            | ANY
            deriving (Eq, Show, Ord)

data QTYPE = Q_A
           | Q_NS
           | Q_CNAME
           | Q_SOA
           | Q_PTR
           | Q_MX
           | Q_TXT
           | Q_AXFR
           | Q_ALL
           | Q_OTHER Word16
           deriving (Eq, Show, Ord)

data Question = Question { qname  :: NAME
                         , qtype :: QTYPE
                         , qclass :: QCLASS
                         } deriving (Eq, Show)

data RR = RR { name :: NAME
             , ttl  :: Word32
             , rdata  :: RDATA
             }
        deriving (Eq, Show)

data RDATA_COM = CNAME NAME
               | MX Word16 NAME
               | NS NAME
               | PTR NAME
               | SOA NAME NAME Word32 Word32 Word32 Word32 Word32
               | TXT [ByteString]
               deriving (Eq, Ord, Show)

data RDATA = RR_COM CLASS RDATA_COM
           | RR_A Word32
           | RR_OTHER CLASS Word16 ByteString
    deriving (Eq, Ord, Show)

instance Show NAME where
  show (NAME xs) = intercalate "." (map BS.unpack xs)

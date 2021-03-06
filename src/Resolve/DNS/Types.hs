module Resolve.DNS.Types where

import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.BitVector
import Data.List

import Resolve.DNS.EDNS.Types


type LABEL = ByteString
newtype NAME = NAME {unname :: [LABEL]} deriving (Eq, Ord)

newtype IPv4 = IPv4 {unIPv4 :: Word32}
  deriving (Eq, Ord)

instance Show IPv4 where
  show (IPv4 w) = intercalate "." $ map (\b -> show $ (fromIntegral $ w `shiftR` (b * 8) :: Word8)) [3,2,1,0]
  

rootName :: NAME
rootName = NAME [BS.empty]

data Message = Message {
    header     :: Header
  , question   :: [Question]
  , answer     :: [RR]
  , authority  :: [RR]
  , additional :: [RR]
  , opt        :: Maybe OPT
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
  | OTH Word8  -- should be Word4
  deriving (Eq, Show)

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
           | RR_A IPv4
           | RR_OTHER CLASS Word16 ByteString
    deriving (Eq, Ord, Show)

instance Show NAME where
  show (NAME xs) = intercalate "." (map BS.unpack xs)

fromCLASS :: CLASS -> Word16
fromCLASS c = case c of
  IN -> 1
  CH -> 3
  HS -> 4
  OTHER i -> i 

fromQCLASS :: QCLASS -> Word16
fromQCLASS c = case c of
  CLASS c' -> fromCLASS c'
  ANY -> 5

fromQTYPE :: QTYPE -> Word16  
fromQTYPE t = case t of
  Q_A -> 1
  Q_NS -> 2
  Q_CNAME -> 5
  Q_SOA -> 6
  Q_PTR -> 12
  Q_MX -> 15
  Q_TXT -> 16
  Q_AXFR -> 252
  Q_OTHER i -> i

fromOPCODE :: OPCODE -> BitVector  
fromOPCODE c = bitVec 4 $ case c of
  STD -> 0 :: Word8
  INV -> 1
  SSR -> 2
  OTH i -> i
  
fromRCODE :: RCODE -> BitVector  
fromRCODE c = bitVec (4 :: Int) $ case c of
  NoErr -> 0
  FormatErr -> 1
  ServFail -> 2
  NameErr -> 3
  NotImpl -> 4
  Refused -> 5
  Other i -> i

fromQR :: QR -> Bool
fromQR c = case c of
  Query -> False
  Response -> True

toCLASS :: Word16 -> CLASS
toCLASS c = case c of
  1 -> IN
  3 -> CH 
  4 -> HS 
  i -> OTHER i 

toQCLASS :: Word16 -> QCLASS
toQCLASS c = case toCLASS c of
  OTHER i -> case i of
               5 -> ANY
               _ -> CLASS $ OTHER i
  j -> CLASS $ j

toQTYPE :: Word16   -> QTYPE 
toQTYPE t = case t of
  1 ->  Q_A 
  2 -> Q_NS 
  5 -> Q_CNAME 
  6 ->   Q_SOA
  12 ->   Q_PTR 
  15 ->   Q_MX 
  16 ->  Q_TXT 
  i ->   Q_OTHER i 

toOPCODE :: BitVector -> OPCODE 
toOPCODE c = case c of
  0 -> STD 
  1 -> INV 
  2 -> SSR
  i -> OTH $ fromIntegral i
  
toRCODE :: BitVector -> RCODE 
toRCODE c = case c of
  0 ->  NoErr
  1 ->  FormatErr
  2 ->  ServFail
  3 ->  NameErr
  4 ->  NotImpl
  5 ->  Refused
  i ->  Other $ fromIntegral i

toQR :: Bool -> QR
toQR c = case c of
  False -> Query 
  True -> Response

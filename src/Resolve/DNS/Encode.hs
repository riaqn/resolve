module Resolve.DNS.Encode where

import Control.Monad.Trans.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder 
import Resolve.DNS.Types hiding (header, name, qtype, question, qclass)
import qualified Resolve.DNS.Types as T 
import Data.Monoid
import Data.Word
import Data.BitVector
import qualified Data.HashMap as HM
import Data.Maybe
import Data.Hashable
import Data.IP

instance Hashable QCLASS where
  hashWithSalt i a = i `xor` (fromIntegral $ fromQCLASS a)
  
instance Hashable QTYPE where
  hashWithSalt i a = i `xor` (fromIntegral $ fromQTYPE a)

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
  Q_OTHER i -> i

fromOPCODE :: OPCODE -> BitVector  
fromOPCODE c = bitVec 4 $ case c of
  STD -> 0
  INV -> 1
  SSR -> 2
  
fromRCODE :: RCODE -> BitVector  
fromRCODE c = bitVec 4 $ case c of
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

type SPut a = a -> Either String Builder
type Put a = a -> Builder

encode :: SPut a -> a -> Either String BSL.ByteString
encode e a = case e a of
  Left e -> Left e
  Right b -> Right $ toLazyByteString b

message :: SPut Message
message m = fmap mconcat $ sequence [ Right $ header ( T.header m
                                                     , fromIntegral $ length $ T.question m
                                                     , fromIntegral $ length $ answer m
                                                     , fromIntegral $ length $ authority m
                                                     , fromIntegral $ length $ additional m)
                                          , fmap mconcat  (mapM question $ T.question m)
                                          , fmap mconcat (mapM rr $ answer m)
                                          , fmap mconcat (mapM rr $ authority m)
                                          , fmap mconcat (mapM rr $ additional m)]
  
header :: Put (Header, Word16, Word16, Word16, Word16)
header (h, qd, an, ns, ar) =
  (word16BE (ident h)) <>
  (word16BE $ fromIntegral ((fromBool $ fromQR $ qr h) #
                             (fromOPCODE $ opcode h) #
                             (fromBool $ aa h) #
                             (fromBool $ tc h) #
                             (fromBool $ rd h) #
                             (fromBool $ ra h) #
                             (bitVec 3 $ zero h) #
                             (fromRCODE $ rcode h))) <>
  word16BE qd <>
  word16BE an <>
  word16BE ns <>
  word16BE ar

label :: SPut LABEL
label l = if len < (fromIntegral (maxBound :: Word8)) then
                  Right $ (word8 (fromIntegral len)) <> (byteString l)
                else
                  Left "Too long the LABEL"
  where len = BS.length l

name :: SPut NAME
name (NAME n) = mconcat <$> mapM label n

qtype :: Put QTYPE
qtype t = word16BE $ fromQTYPE t

qclass :: Put QCLASS
qclass c = word16BE $ fromQCLASS c

question :: SPut Question
question q = mconcat <$> sequence [ name (qname q)
                                  , Right $ qtype (T.qtype q)
                                  , Right $ qclass (T.qclass q)]

rr :: SPut RR
rr rr = do
  b <- d
  let bs = toLazyByteString b
  let len = BSL.length bs
  if len < (fromIntegral (maxBound :: Word16)) then Right ()
    else Left "too large the RDATA"
    
  mconcat <$> sequence [ name $ T.name rr
                       , Right $ word16BE t
                       , Right $ word16BE $ fromCLASS $ c
                       , Right $ word32BE $ ttl rr
                       , Right $ word16BE (fromIntegral len)
                       , Right $ lazyByteString  bs]
  where (c, t, d) = case rdata rr of
          RR_COM c com -> case com of 
            CNAME n -> (c, 5, name n)
            NS n -> (c, 2, name n)
            SOA mname rname serial refresh retry expire minimum ->
              (c, 6, mconcat <$> sequence [ name mname
                                          , name rname
                                          , Right $ word32BE serial
                                          , Right $ word32BE refresh
                                          , Right $ word32BE retry
                                          , Right $ word32BE expire
                                          , Right $ word32BE minimum
                                          ])
            PTR n -> (c, 12, name n)
            MX ref n -> (c, 15, mappend <$> (Right $ word16BE ref) <*> (name n))
            TXT d -> (c, 16, Right $ mconcat $ map byteString d)
          RR_A ip -> (IN, 1, Right $ mconcat $ map (word8 . fromIntegral) (fromIPv4 ip))
          RR_OTHER c t d -> (c, t, Right $ byteString d)

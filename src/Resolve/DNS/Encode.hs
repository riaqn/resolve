module Resolve.DNS.Encode where

import Control.Monad.Trans.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder 
import Resolve.DNS.Types hiding (header, name, qtype, question, qclass)
import Resolve.DNS.Utils
import qualified Resolve.DNS.Exceptions as E
import qualified Resolve.DNS.Types as T
import qualified Resolve.DNS.EDNS.Encode as EE
import Data.Monoid
import Data.Word
import Data.BitVector
import Data.Maybe
import Data.Hashable

import Control.Exception
import Data.Typeable

instance Hashable QCLASS where
  hashWithSalt i a = i `xor` (fromIntegral $ fromQCLASS a)
  
instance Hashable QTYPE where
  hashWithSalt i a = i `xor` (fromIntegral $ fromQTYPE a)

data Error = EDNSError EE.Error
           | LabelTooLong
           | TooManyRR
           deriving (Show, Typeable)

instance Exception Error where
  toException = E.errorToException
  fromException = E.errorFromException


type SPut a = a -> Either Error Builder
type Put a = a -> Builder

encode :: SPut a -> a -> Either Error BSL.ByteString
encode e a = case e a of
  Left e -> Left e
  Right b -> Right $ toLazyByteString b

message :: SPut Message
message m = fmap mconcat $ sequence [ do
                                        let len x = case safeFromIntegral (length x) of
                                              Nothing -> Left TooManyRR
                                              Just y -> Right y
                                        nque <- len $ T.question m
                                        nans <- len $ answer m
                                        naut <- len $ authority m
                                        nadd <- len $ additional m
                                        return $ header ( T.header m
                                                        , nque
                                                        , nans
                                                        , naut
                                                        , nadd)
                                    , mconcat <$>  (mapM question $ T.question m)
                                    , mconcat <$> (mapM rr $ answer m)
                                    , mconcat <$> (mapM rr $ authority m)
                                    , mconcat <$> case opt m of
                                                    Nothing -> mapM rr $ additional m
                                                    Just opt' -> do
                                                      opt_ <- case EE.opt opt' of
                                                        Left e -> Left $ EDNSError e
                                                        Right a -> Right a
                                                      mapM rr $ (opt_ : additional m)]
  
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
label l = case safeFromIntegral (BS.length l) of
  Nothing -> Left LabelTooLong
  Just len -> Right $ (word8 len) <> (byteString l)

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
  len <- case safeFromIntegral (BSL.length bs) of
    Nothing -> Left LabelTooLong
    Just x -> Right x
    
  mconcat <$> sequence [ name $ T.name rr
                       , Right $ word16BE t
                       , Right $ word16BE $ fromCLASS $ c
                       , Right $ word32BE $ ttl rr
                       , Right $ word16BE len
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
          RR_A ip -> (IN, 1, Right $ word32BE ip)
          RR_OTHER c t d -> (c, t, Right $ byteString d)

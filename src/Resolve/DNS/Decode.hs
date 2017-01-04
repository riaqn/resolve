module Resolve.DNS.Decode where

import Prelude hiding (take)
import Resolve.DNS.Types hiding (name, header, question, qname, qclass, qtype)
import qualified Resolve.DNS.Types as T
import qualified Resolve.DNS.Exceptions as E
import qualified Resolve.DNS.EDNS.Decode as ED


import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Attoparsec.Binary 
import Data.Attoparsec.ByteString
import Data.Word
import Data.Bits
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad

import Control.Exception
import Data.Typeable

import Data.BitVector hiding (not, foldr)

type SGet = ReaderT ByteString Parser

data Error = Error String
  deriving (Show, Typeable)

instance Exception Error where
  toException = E.errorToException
  fromException = E.errorFromException


decodeMessage :: ByteString -> Either String Message
decodeMessage bs = parseOnly (runReaderT message bs) bs

message :: SGet T.Message
message = do
    (h, nque, nans, naut, nadd) <- lift header
    if tc h then
      return $ T.Message h [] [] [] [] Nothing
      else do
      que <- count (fromIntegral nque) question
      ans <- count (fromIntegral nans) rr
      aut <- count (fromIntegral naut) rr
      add <- count (fromIntegral nadd) rr

      let m = foldr (\rr m -> do
                (l_opt, l_rst) <- m
                case ED.opt rr of
                  Left e -> case e of
                    ED.NotOPT -> return (l_opt, rr:l_rst)
                    e -> Left $ show e
                  Right opt -> case l_opt of
                    Nothing -> return (Just opt, l_rst)
                    Just _ -> Left $ "more than one OPT"
            ) (return (Nothing, [])) add
      case m of
        Left e -> error e
        Right (opt', add') -> return $ T.Message h que ans aut add' opt'

toBool :: (Eq a, Num a) => a -> Bool
toBool = (/= 0)

isEnd :: Word8 -> Bool
isEnd c = (c == 0) || (testBit c 7) && (testBit c 8)

extractNAME :: ByteString -> Int -> Either String T.NAME
extractNAME m i = parseOnly (runReaderT name m) (BS.drop i m)
                         
name :: SGet T.NAME
name = do
  m <- ask
  c <- lift anyWord8
  if all (testBit c) [6,7] then do
    c' <- lift anyWord8
    let pos = ((fromIntegral (c .&. 0b111111) :: Word16) `shift` 8) .|. (fromIntegral c')
    case extractNAME m (fromIntegral pos) of
      Left e -> error e
      Right r -> return r
      
    else if not (any (testBit c) [6,7]) then do
    l <- lift $ take (fromIntegral c)
    if c == 0 then return $ NAME [l]
      else do 
      (T.NAME tail) <- name
      return $ NAME $ l : tail
    else
    error $ "NAME format not recognized"


charString :: SGet ByteString
charString = do
  n <- lift anyWord8
  lift $ take (fromIntegral n)

rr :: SGet T.RR
rr = do
  n <- name
  (c, t, ttl, rdata) <- lift $ do 
    t <- anyWord16be
    c_ <- anyWord16be
    let c = toCLASS c_ 
    ttl <- anyWord32be
    rdlength <- anyWord16be
    rdata <- take (fromIntegral rdlength)
    return (c, t, ttl, rdata)
  let p = case (t, c) of
        (5, _) -> RR_COM c <$> CNAME <$> name
        (15, _) -> RR_COM c <$> (MX <$> (lift anyWord16be) <*> name)
        (2, _) -> RR_COM c <$> NS <$> name
        (12, _) -> RR_COM c <$> PTR <$> name
        (6, _) -> RR_COM c <$> (SOA <$> name <*> name <*> lift anyWord32be
                    <*> lift anyWord32be <*> lift anyWord32be <*> lift anyWord32be <*> lift anyWord32be)
        (16, _) -> RR_COM c <$> TXT <$> many1 charString
        (1, IN) -> RR_A <$> IPv4 <$> (lift $ do
                                ip <- anyWord32be
                                flag <- atEnd
                                when (not flag) $ error "IPv4 is not 4B?"
                                return ip)
        _ -> RR_OTHER c t <$> lift takeByteString
  bs <- ask
  case parseOnly (runReaderT p bs) rdata of
    Left e -> error $ e ++ show (c, t, ttl, rdata)
    Right r -> return $ RR n ttl r

qname = Resolve.DNS.Decode.name

qtype :: Parser T.QTYPE
qtype = do
  n <- anyWord16be
  return $ toQTYPE n 

qclass :: Parser T.QCLASS
qclass = do
  n <- anyWord16be
  return $ toQCLASS n 
    
question :: SGet T.Question
question = Question <$> qname
                <*> lift qtype
                <*> lift qclass

header :: Parser (T.Header, Word16, Word16, Word16, Word16)
header = do 
  i <- anyWord16be
  f_ <- anyWord16be
  qd <- anyWord16be
  an <- anyWord16be
  ns <- anyWord16be
  ar <- anyWord16be
  let f = bitVec 16 f_
  return $ (T.Header { T.ident = i
                     , T.qr = toQR $ f @. 15
                     , T.opcode = toOPCODE $ f @@ (14, 11)
                     , T.aa = f @. 10
                     , T.tc = f @. 9
                     , T.rd = f @. 8
                     , T.ra = f @. 7
                     , T.zero = fromIntegral $ f @@ (6, 4)
                     , T.rcode = toRCODE $ f @@ (3, 0)
                     }
           , qd, an, ns, ar)

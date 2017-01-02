module Resolve.DNS.Utils where

import Data.ByteString
import Data.Word
import Data.Bits

safeFromIntegral :: (Integral a, Integral b) => a -> Maybe b
safeFromIntegral x = let y = fromIntegral x
  in if (fromIntegral y == x) then Just y
  else Nothing

toWord16 :: ByteString -> Word16
toWord16 bs = Data.ByteString.foldl (\a b -> a `shift` 8 .|. (fromIntegral b)) 0 bs 

fromWord16 :: Word16 -> ByteString
fromWord16 w = cons (fromIntegral (w `shift` (-8) .&. 0xff)) $ cons (fromIntegral (w .&. 0xff)) empty

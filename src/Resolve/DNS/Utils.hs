module Resolve.DNS.Utils where

import Data.ByteString
import Data.Word
import Data.Bits

safeFromIntegral :: (Integral a, Integral b) => a -> Maybe b
safeFromIntegral x = let y = fromIntegral x
  in if (fromIntegral y == x) then Just y
  else Nothing

toWord16 :: ByteString -> Word16
toWord16 bs = ((fromIntegral $ index bs 0) `shift` 8) .|. (fromIntegral $ index bs 1)

fromWord16 :: Word16 -> ByteString
fromWord16 w = cons (fromIntegral (w `shift` (-8) .&. 0xff)) $ cons (fromIntegral (w .&. 0xff)) empty

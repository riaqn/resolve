module Resolve.DNS.Utils where

safeFromIntegral :: (Integral a, Integral b) => a -> Maybe b
safeFromIntegral x = let y = fromIntegral x
  in if (fromIntegral y == x) then Just y
  else Nothing

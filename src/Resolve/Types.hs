module Resolve.Types where

type Resolve a b = a -> IO b
data Resolver a b = Resolver { resolve :: Resolve a b
                             , delete :: IO ()
                             }

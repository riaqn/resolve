module Resolve.DNS.Transport.Types where

import Data.ByteString.Lazy

type Send = ByteString -> IO ()
type Recv = IO ByteString
type Delete = IO ()

data Transport = Transport { send :: Send
                           , recv :: Recv
                           , delete :: IO ()
                           }

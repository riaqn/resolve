module Resolve.DNS.Transport.Types where

import Resolve.DNS.Types

type Send = Message -> IO ()
type Recv = IO Message
type Delete = IO ()

data Transport = Transport { send :: Send
                           , recv :: Recv
                           , delete :: IO ()
                           }

module Resolve.DNS.Transport.Helper.LiveTCP where

import Resolve.DNS.Transport.Types
import qualified Resolve.DNS.Transport.LiveTCP as TCP

import Network.Socket

data Config = Config { host :: HostName
                     , port :: ServiceName
                     , passive :: Bool
                     }
              deriving (Show)

lname = "Resolve.DNS.Helper.LiveTCP"              

new :: Config -> IO Transport
new c = do
  let hints = defaultHints { addrSocketType = Stream}
  addr:_ <- getAddrInfo (Just hints) (Just $ host c) (Just $ port c)
  TCP.new $ TCP.Config { TCP.family = addrFamily addr
                       , TCP.protocol = addrProtocol addr
                       , TCP.server = addrAddress addr
                       , TCP.passive = passive c
                       }

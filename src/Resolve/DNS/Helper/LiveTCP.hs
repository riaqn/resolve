module Resolve.DNS.Helper.LiveTCP where

import qualified Resolve.DNS.LiveTCP as TCP
import Resolve.Types
import Resolve.DNS.Types
import System.Log.Logger

import Network.Socket

data Config = Config { host :: HostName
                     , port :: ServiceName
                     }
              deriving (Show)

lname = "Resolve.DNS.Helper.LiveTCP"              

new :: Config -> IO (Resolver Message Message)
new c = do
  let hints = defaultHints { addrSocketType = Stream}
  addr:_ <- getAddrInfo (Just hints) (Just $ host c) (Just $ port c)
  TCP.new $ TCP.Config { TCP.family = addrFamily addr
                       , TCP.protocol = addrProtocol addr
                       , TCP.server = addrAddress addr
                       }

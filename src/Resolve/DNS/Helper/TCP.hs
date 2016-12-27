module Resolve.DNS.Helper.TCP where

import qualified Resolve.DNS.TCP as TCP
import Resolve.Types
import Resolve.DNS.Types

import Network.Socket

data Config = Config { host :: HostName
                     , port :: ServiceName
                     }

new :: Config -> IO (Resolver Message Message)
new c = do
  let hints = defaultHints { addrSocketType = Stream}
  addr:_ <- getAddrInfo (Just hints) (Just $ host c) (Just $ port c)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock $ addrAddress addr
  TCP.new $ TCP.Config { TCP.socket = sock}

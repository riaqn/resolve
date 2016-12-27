module Resolve.DNS.Helper.UDP where

import qualified Resolve.DNS.UDP as UDP
import Resolve.Types
import Resolve.DNS.Types

import Network.Socket

data Config = Config { host :: HostName
                     , port :: ServiceName
                     }

new :: Config -> IO (Resolver Message Message)
new c = do
  let hints = defaultHints { addrSocketType = Datagram}
  addr:_ <- getAddrInfo (Just hints) (Just $ host c) (Just $ port c)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  UDP.new $ UDP.Config { UDP.socket = sock
                       , UDP.server = addrAddress addr}

module Resolve.DNS.Transport.Helper.TCP where

import Resolve.DNS.Transport.Types
import qualified Resolve.DNS.Transport.TCP as TCP


import Network.Socket

data Config = Config { host :: HostName
                     , port :: ServiceName
                     }
              deriving (Show)

new :: Config -> IO Transport
new c = do
  let hints = defaultHints { addrSocketType = Stream}
  addr:_ <- getAddrInfo (Just hints) (Just $ host c) (Just $ port c)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock $ addrAddress addr
  TCP.new $ TCP.Config { TCP.socket = sock}

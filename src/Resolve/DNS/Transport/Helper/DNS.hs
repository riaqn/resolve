module Resolve.DNS.Helper.DNS where

import Network.Socket

import Resolve.Types
import Resolve.DNS.Types

import qualified Resolve.DNS.Helper.UDP as UDP
import qualified Resolve.DNS.Helper.LiveTCP as TCP

import qualified Resolve.DNS.Truncation as T

import Control.Exception


data Config = Config { host :: HostName
                     , port :: ServiceName
                     }

new :: Config -> IO (Resolver Message Message)
new c = do
  bracketOnError
    (do
        u <- UDP.new $ UDP.Config {UDP.host = host c, UDP.port = port c}
        t <- TCP.new $ TCP.Config {TCP.host = host c, TCP.port = port c}
        return $ Resolver { resolve = T.truncation $ T.Config {T.udp = resolve u, T.tcp = resolve t}
                          , delete = do
                              delete u
                              delete t
                          }
    )
    (\r -> delete r)
    return
  

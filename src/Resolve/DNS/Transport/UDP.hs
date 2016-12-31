module Resolve.DNS.Transport.UDP where

import Resolve.Types

import Resolve.DNS.Types
import Resolve.DNS.Exceptions
import Resolve.DNS.Utils
import Resolve.DNS.Encode as E
import Resolve.DNS.Decode as D
import qualified Resolve.DNS.Transport.Types as T

import Data.Typeable
import Data.ByteString.Builder
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL


import Network.Socket hiding (recvFrom, sendTo, socket)
import Network.Socket.ByteString

import Data.Bits

import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Concurrent
import Control.Exception
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import Control.Monad.STM

import System.Log.Logger

nameM = "Resolve.DNS.Transport.UDP"

data Config = Config { socket :: Socket
                     , server :: SockAddr
                     , payload :: IO Int
                     }

data QueryTooLong = QueryTooLong
           deriving (Typeable, Show, Eq)

instance Exception QueryTooLong where
    toException = errorToException
    fromException = errorFromException

new :: Config -> IO T.Transport 
new c = do
  bracketOnError
    (do
        let recv' = let loop = do
                          let nameF = nameM ++ ".recv"
                          p <- payload c
                          (bs, sa) <- (recvFrom (socket c) p)
                          if sa /= (server c) then loop
                            else case D.decodeMessage bs of
                                   Left e -> throwIO $ D.Error e
                                   Right m -> return m
                    in
                      loop
                      
        let send' = \m -> do
              p <- payload c
              case E.encode E.message m of
                Left e -> throwIO $ e
                Right bs -> if BSL.length bs > fromIntegral p then
                            throwIO QueryTooLong
                            else void $ sendTo (socket c) (BSL.toStrict bs) (server c)

        
        return $ T.Transport { T.send = send'
                             , T.recv = recv'
                             , T.delete = return ()
                             }
    )
    (\t -> T.delete t)
    (\t  -> return t
    )

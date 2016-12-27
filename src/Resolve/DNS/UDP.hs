module Resolve.DNS.UDP where

import qualified Resolve.DNS.Channel as C
import Resolve.Types

import Resolve.DNS.Types
import Network.Socket hiding (send, sendTo, recv, recvFrom, socket)
import Network.Socket.ByteString
import qualified Data.ByteString as BS

import Data.Typeable

import Control.Exception
import Control.Monad

import System.Log.Logger

nameM = "Resolve.DNS.UDP"


data Config = Config { socket :: Socket
                     , server :: SockAddr
                     }

data QueryTooLong = QueryTooLong
           deriving (Typeable, Show)

instance Exception QueryTooLong where
    toException = dnsExceptionToException
    fromException = dnsExceptionFromException

maxLength = 512

new :: Config -> IO (Resolver Message Message)
new c = do
  chan <- C.new C.Config { C.send = \a -> do
                             let nameF = nameM ++ ".send"
                             if BS.length a > maxLength then
                               throw QueryTooLong
                               else do
                               debugM nameF $ "sending " ++ (show $ BS.length a) ++ "B"
                               void $ sendTo (socket c) a (server c)
                               debugM nameF $ "sent " ++ (show $ BS.length a) ++ "B"

                
                         , C.recv = let loop = do
                                          let nameF = nameM ++ ".recv"
                                          debugM nameF $ "recving "
                                          (bs, sa) <- (recvFrom (socket c) maxLength)
                                          debugM nameF $ "recvd " ++ (show $ BS.length bs) ++ "B"
                                          if sa /= (server c) then loop
                                            else return bs
                                    in
                                      loop
                         , C.nick = "UDP<" ++ (show $ socket $ c) ++ ">" ++ (show $ server $ c)
                         }
  return $ Resolver { resolve = resolve chan
                    , delete = delete chan
                    }

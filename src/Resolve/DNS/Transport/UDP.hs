module Resolve.DNS.Transport.UDP where

import qualified Resolve.DNS.Transport.Types as T

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL


import Network.Socket hiding (recvFrom, sendTo, socket)
import Network.Socket.ByteString

import Control.Monad
import Control.Exception

import System.Log.Logger

nameM = "Resolve.DNS.Transport.UDP"

data Config = Config { socket :: Socket
                     , server :: SockAddr
                     , p_max :: Int
                     }

new :: Config -> IO T.Transport 
new c = do
  bracketOnError
    (do
        let recv' = let loop = do
                          let nameF = nameM ++ ".recv"
                          debugM nameF $ "recving..."
                          (bs, sa) <- (recvFrom (socket c) $ p_max c)
                          if sa /= (server c) then do
                            debugM nameF $ "recvd " ++ (show $ BS.length bs) ++ "B from " ++ (show sa)
                            loop
                            else do
                            debugM nameF $ "recvd " ++ (show $ BS.length bs) ++ "B"
                            return $ BSL.fromStrict bs 
                    in
                      loop
                      
        let send' = \bs -> do
              let nameF = nameM ++ ".send"
              void $ sendTo (socket c) (BSL.toStrict bs) (server c)
              debugM nameF $ "sent " ++ (show $ BSL.length bs) ++ "b"

        
        return $ T.Transport { T.send = send'
                             , T.recv = recv'
                             , T.delete = return ()
                             }
    )
    (\t -> T.delete t)
    (\t  -> return t
    )

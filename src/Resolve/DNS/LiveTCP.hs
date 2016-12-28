module Resolve.DNS.LiveTCP where

import qualified Resolve.DNS.TCP as TCP
import Resolve.Types
import qualified Resolve.DNS.Channel as C
import Resolve.DNS.Types

import Control.Monad
import Control.Monad.STM
import Control.Monad.Trans.Except
import Control.Concurrent.STM.TMVar
import Control.Concurrent

import Data.Maybe
import Data.Unique

import Control.Exception


import Network.Socket  hiding (Closed)

import System.Log.Logger

nameM = "Resolve.DNS.LiveTCP"

data Config = Config { family :: Family
                     , protocol :: ProtocolNumber
                     , server :: SockAddr                       
                     }
              deriving (Show)

data Record = Record { resolver :: Resolver Message Message
                     , unique :: Unique
                     , sock :: Socket
                     }
              
new :: Config -> IO (Resolver Message Message)
new c = do
  res <- newEmptyTMVarIO
  l <- newEmptyTMVarIO -- whoever holds the lock will reconnect

  let del r = do
        delete (resolver r)
        close (sock r)
        x <- atomically $ do
          x <- tryReadTMVar res
          case x of
            Nothing -> return False
            Just r' -> if ((unique r) == (unique r')) then (void $ takeTMVar res) >> return True
                       else return False
        when x $ debugM nameM $ (show c) ++ " connection closed, deleted"

  let loop a = do
        let nameF = nameM ++ ".resolve"
        bracket
          (atomically $ do
              x <- tryReadTMVar res
              when (isNothing x) $ putTMVar l ()
              return x)
          (\x -> atomically $ when (isNothing x) $ takeTMVar l)
          (\x -> case x of
              Just r -> do
                b <- try (resolve (resolver r) a)
                case b of
                  Left C.Dead -> do
                    del r
                    loop a
                  Right b' -> return b'
              Nothing -> do
                debugM nameF $ (show c) ++ " trying to reconnect"
                bracketOnError
                  (socket (family c) (Stream) (protocol c))
                  (\s -> close s)
                  (\s -> do 
                      connect s (server c)
                      debugM nameF $ (show c ) ++ " reconnected"
                      bracketOnError 
                        (TCP.new $ TCP.Config { TCP.socket = s})
                        delete
                        (\r -> do
                            u <- newUnique
                            atomically $ do 
                              putTMVar res $ Record { resolver = r
                                                    , unique = u
                                                    , sock = s}
                        )
                  )
                loop a)
          
  return $ Resolver { resolve = loop
                    , delete = do
                        x <- atomically $ tryReadTMVar res
                        case x of
                          Nothing -> return ()
                          Just r -> del r
                    }


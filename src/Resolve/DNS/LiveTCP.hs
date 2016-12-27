module Resolve.DNS.LiveTCP where

import qualified Resolve.DNS.TCP as TCP
import Resolve.Types
import qualified Resolve.DNS.Channel as C
import Resolve.DNS.Types

import Control.Monad
import Control.Monad.Trans.Except
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Exception

import Network.Socket  hiding (Closed)

import           Control.Concurrent.ReadWriteLock        ( RWLock )
import qualified Control.Concurrent.ReadWriteLock as RWL

import System.Log.Logger

nameM = "Resolve.DNS.LiveTCP"

data Config = Config { family :: Family
                     , protocol :: ProtocolNumber
                     , server :: SockAddr                       
                     }
              deriving (Show)
              
new :: Config -> IO (Resolver Message Message)
new c = do
  r <- TCP.newClosed
  res <- newMVar (Nothing, r) 
  l <- RWL.new -- RW lock

  let del = do
        r <- takeMVar res
        delete (snd r)
        case fst r of
          Nothing -> return ()
          Just s -> close s
        
  let loop a = do
        let nameF = nameM ++ ".resolve"
        b <- bracket_
          (RWL.acquireRead l)
          (RWL.releaseRead l)
          (do
              r <- readMVar res
              b <- try (resolve (snd r) a)
              debugM nameF $ show b
              return b
          )
        case b of
          Left C.Dead -> do
            infoM nameF $ (show c) ++ " connection closed, reconnecting"            
            bracket 
              (RWL.tryAcquireWrite l)
              (\w -> when w $ RWL.releaseWrite l)
              (\w -> when w $ do
                  del 
                  bracketOnError
                    (socket (family c) (Stream) (protocol c))
                    (\s -> do
                        debugM nameF "definitely not expecting this!"
                        close s)
                    (\s -> do 
                        connect s (server c)
                        infoM nameF $ (show c ) ++ " reconnected"
                        bracketOnError 
                          (TCP.new $ TCP.Config { TCP.socket = s})
                          delete
                          (\r -> putMVar res (Just s, r))
                    )
              )
            debugM nameF "now let's try again"
            loop a
          Right b' -> return b'
          
  return $ Resolver { resolve = loop
                    , delete = del
                    }


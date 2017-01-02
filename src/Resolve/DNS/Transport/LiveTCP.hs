module Resolve.DNS.Transport.LiveTCP where

import qualified Resolve.DNS.Transport.Types as T
import qualified Resolve.DNS.Transport.TCP as TCP

import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TMVar

import Data.Maybe
import Data.Unique

import Network.Socket hiding (Closed)

import System.Log.Logger
import Control.Exception

nameM = "Resolve.DNS.Transport.LiveTCP"

data Config = Config { family :: Family
                     , protocol :: ProtocolNumber
                     , server :: SockAddr
                     , passive :: Bool
                     }
              deriving (Show)

data Record = Record { transport :: T.Transport
                     , unique :: Unique
                     , sock :: Socket
                     }

data LiveTCP = LiveTCP { config :: Config
                       , res :: TMVar Record
                       , lock :: TMVar ()
                       }              

new :: Config -> IO T.Transport
new c = do
  r <- newEmptyTMVarIO
  l <- newEmptyTMVarIO -- whoever holds the lock will reconnect

  let live  = LiveTCP { config = c
                      , res = r
                      , lock = l
                      }

  return $ T.Transport { T.send = \m -> wrap live (\t -> T.send t m)
                       , T.recv = (if passive c then wrap' else wrap) live (\t -> T.recv t)
                       , T.delete = do
                           x <- atomically $ tryReadTMVar (res live)
                           case x of
                             Nothing -> return ()
                             Just r -> del live r
                       }
    
del :: LiveTCP -> Record -> IO ()
del live r = do
      T.delete (transport r)
      close (sock r)
      void $ atomically $ do
        x <- tryReadTMVar (res live)
        case x of
          Nothing -> return False
          Just r' -> if ((unique r) == (unique r')) then (void $ takeTMVar $ res live) >> return True
                     else return False

wrap' :: LiveTCP -> (T.Transport -> IO b) -> IO b
wrap' live = \f -> do
  t <- atomically $ readTMVar $ res live
  b <- try (f (transport t))
  case b of
    Left TCP.Closed -> del live t >> wrap' live f
    Right b' -> return b'
  
wrap :: LiveTCP -> (T.Transport -> IO b) -> IO b
wrap live = \f -> do
  let nameF = nameM ++ ".wrap"
  b' <- bracket
    (atomically $ do
        x <- tryReadTMVar $ res live
        when (isNothing x) $ putTMVar (lock live) ()
        return x)
    (\x -> atomically $ when (isNothing x) $ takeTMVar (lock live))
    (\x -> case x of
        Just t -> do
          b <- try (f (transport t))
          case b of
            Left TCP.Closed -> del live t >> return Nothing
            Right b' -> return $ Just b'
        Nothing -> do
          bracketOnError
            (socket (family $ config live) (Stream) (protocol $ config live))
            (\s -> close s)
            (\s -> do 
                connect s (server $ config live)
                debugM nameF $ (show $ config live ) ++ " reconnected"
                bracketOnError 
                  (TCP.new $ TCP.Config { TCP.socket = s})
                  T.delete
                  (\t -> do
                      u <- newUnique
                      atomically $ do 
                        putTMVar (res live) $ Record { transport = t
                                              , unique = u
                                              , sock = s}
                  )
            )
          return Nothing
    )
  case b' of
    Nothing -> wrap live f
    Just b -> return b

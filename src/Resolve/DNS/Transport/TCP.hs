module Resolve.DNS.Transport.TCP where

import Resolve.DNS.Transport.Exceptions
import Resolve.DNS.Utils
import Resolve.DNS.Encode as E
import qualified Resolve.DNS.Transport.Types as T

import Data.Typeable
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BSL


import Network.Socket hiding (recv, send, socket, Closed)
import Network.Socket.ByteString.Lazy

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

nameM = "Resolve.DNS.Transport.TCP"

data Config = Config { socket :: Socket
                     }

data Closed = Closed
  deriving (Show, Typeable)

instance Exception Closed where
    toException = errorToException
    fromException = errorFromException

data QueryTooLong = QueryTooLong
           deriving (Typeable, Show)

instance Exception QueryTooLong where
    toException = errorToException
    fromException = errorFromException

data DecodeError = DecodeError String
           deriving (Typeable, Show)

instance Exception DecodeError where
    toException = errorToException
    fromException = errorFromException

data EncodeError = EncodeError E.Error
           deriving (Typeable, Show)

instance Exception EncodeError where
    toException = errorToException
    fromException = errorFromException

    
new :: Config -> IO T.Transport 
new c = do
  qi <- newEmptyTMVarIO
  si <- newTVarIO True
  l <- newEmptyMVar
  
  bracketOnError
    (do
        ti <- forkFinally
          (forever $ runExceptT $ do -- EitherT String IO ()
              let recvAll' n = if n == 0 then return mempty
                    else do  -- IO ()
                    bs <- recv (socket c) n
                    when (BSL.null bs) $ throwIO ThreadKilled
                    mappend (lazyByteString bs) <$> (recvAll' $ n - (BSL.length bs))
                  recvAll n = do
                    toLazyByteString <$> recvAll' n
              n <- lift $ recvAll 2
              let n' = ((fromIntegral $ BSL.index n 0) `shift` 8) .|. (fromIntegral $ BSL.index n 1)
              bs <- lift $ recvAll $ n'
              lift $ atomically $ putTMVar qi $ bs
          )
          (\_ -> do
              debugM nameM "recv died"
              atomically $ writeTVar si False)
          
        let send' = \bs -> bracket_
                           (putMVar l ())
                           (takeMVar l)
                           (do
                               let nameF = nameM ++ ".send"
                               let sendAll bs = if BSL.null bs then
                                     return ()
                                     else do
                                     n <- catch (send (socket c) bs)
                                       (\e -> do
                                           debugM nameF $ show (e :: SomeException)
                                           throwIO Closed
                                       )
                                     sendAll (BSL.drop n bs)
                               
                               len <-  case safeFromIntegral (BSL.length bs) of
                                   Nothing -> throwIO QueryTooLong
                                   Just x -> return x
                               debugM nameF $ "sending.."
                               sendAll $ toLazyByteString $ word16BE len
                               sendAll $ bs
                               debugM nameF $ "sent"
                           )

        
        return $ T.Transport { T.send = send'
                             , T.recv = do
                                   v <- atomically $ do
                                     v <- readTVar si
                                     if v then do 
                                       x <- takeTMVar qi
                                       return $ Just x
                                       else tryTakeTMVar qi
                                   case v of
                                     Nothing -> throw Closed
                                     Just x -> return x
                             , T.delete = do 
                                 killThread ti
                             }
    )
    (\t -> T.delete t)
    (\t  -> return t
    )

newClosed :: IO T.Transport
newClosed = do
  return $ T.Transport { T.send = \_ -> throwIO Closed
                       , T.recv = throwIO Closed
                       , T.delete = return ()
                       }

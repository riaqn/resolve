module Resolve.DNS.TCP where

import Resolve.Types
import Resolve.DNS.Types
import Resolve.DNS.Utils
import qualified Resolve.DNS.Channel as C

import Data.Typeable
import Data.ByteString.Builder
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL


import Network.Socket hiding (recv, send, socket, Closed)
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

nameM = "Resolve.DNS.TCP"

data Config = Config { socket :: Socket
                     }

data Closed = Closed
  deriving (Show, Typeable)

instance Exception Closed where
    toException = dnsExceptionToException
    fromException = dnsExceptionFromException

data QueryTooLong = QueryTooLong
           deriving (Typeable, Show)

instance Exception QueryTooLong where
    toException = dnsExceptionToException
    fromException = dnsExceptionFromException
    
new :: Config -> IO (Resolver Message Message)
new c = do
  qi <- newEmptyTMVarIO
  qo <- newEmptyTMVarIO
  si <- newTVarIO True
  so <- newTVarIO True
  
  bracketOnError
    (do
        chan <- C.new C.Config { C.send =  \a -> do
                                   v <- atomically $ do
                                     v <- readTVar so
                                     when v $ putTMVar qo a
                                     return v
                                   when (not v) $ throw Closed
                               , C.recv = do
                                   v <- atomically $ do
                                     v <- readTVar si
                                     if v then do 
                                       x <- takeTMVar qi
                                       return $ Just x
                                       else tryTakeTMVar qi
                                   case v of
                                     Nothing -> throw Closed
                                     Just x -> return x
                               , C.nick = "TCP<" ++ (show $ socket c) ++ ">"
                               }

        to <- forkIOWithUnmask $ \unmask -> unmask $ finally
          (forever $ do
              let nameF = nameM ++ ".send"
              let sendAll bs = if BS.null bs then
                                 return ()
                               else do
                    n <- send (socket c) bs
                    sendAll (BS.drop n bs)
              bs <- atomically $ takeTMVar qo
              len <-  case safeFromIntegral (BS.length bs) of
                Nothing -> throwIO QueryTooLong
                Just x -> return x
              sendAll $ BSL.toStrict $ toLazyByteString $ word16BE len
              sendAll $ bs)
          (do
              debugM nameM "send died"
              atomically $ writeTVar so False)

    
        ti <- forkIOWithUnmask $ \unmask -> unmask $ finally
          (forever $ runExceptT $ do -- EitherT String IO ()
              let nameF = nameM ++ ".recv"
              let recvAll' n = if n == 0 then return mempty
                    else do  -- IO ()
                    bs <- recv (socket c) n

                    when (BS.null bs) $ do
                      throwTo to ThreadKilled
                      throw ThreadKilled
                    mappend (byteString bs) <$> (recvAll' $ n - (BS.length bs))
                  recvAll n = do
                    BSL.toStrict <$> toLazyByteString <$> recvAll' n
              n <- lift $ recvAll 2
              let n' = ((fromIntegral $ BS.index n 0) `shift` 8) .|. (fromIntegral $ BS.index n 1)
              d <- lift $ recvAll $ n'
              lift $ atomically $ putTMVar qi $ d)
          (do
              debugM nameM "recv died"
              atomically $ writeTVar si False)
        
        return (resolve chan, do
                   delete chan
                   killThread ti
                   killThread to
               )
    )
    (\(_, d) -> d)
    (\(r, d) -> return $ Resolver { resolve = r
                                  , delete = d
                                  }
    )

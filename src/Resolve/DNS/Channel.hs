{-# LANGUAGE Unsafe #-}
module Resolve.DNS.Channel where

import qualified Resolve.Types as T
import Resolve.DNS.Types
import qualified Resolve.DNS.Encode as E
import qualified Resolve.DNS.Decode as D

import Data.Word
import Data.Hashable
import Data.Either
import Data.Maybe

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Control.Monad.STM
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar


import Data.Typeable

import qualified Control.Concurrent.ReadWriteLock as RWL

import System.Log.Logger


import qualified STMContainers.Map as M

nameM = "Resolve.DNS.Channel"

type Send = ByteString -> IO ()
type Recv = IO ByteString

data EncodeError = EncodeError String 
                 deriving (Typeable, Show)

data Dead = Dead
             deriving (Typeable, Show)

instance Exception EncodeError where
  toException = dnsExceptionToException
  fromException = dnsExceptionFromException

instance Exception Dead where
  toException = dnsExceptionToException
  fromException = dnsExceptionFromException

data Config = Config { send :: Send
                     , recv :: Recv
                     , nick :: String
                     }
                
data Resolver = Resolver { tid :: ThreadId
                         , book :: M.Map Word16 (TMVar Message)
                         , config :: Config
                         , dead :: TVar Bool
                       }

new :: Config -> IO (T.Resolver Message Message)
new c = do
  b <- M.newIO
  si <- newTVarIO False

  bracketOnError
    (forkIOWithUnmask $ \unmask -> unmask $ finally (forever $ runExceptT $ do  -- EitherT String IO ()
        let nameF = nameM ++ ".recv"
        bs <- lift $ recv c
        lift $ debugM nameF $ (nick c) ++ "recvd "  ++ (show $ BS.length bs) ++ "B"
        m <- ExceptT $ return $ D.decodeMessage bs
        lift $ debugM nameF $ (nick c) ++ " -> " ++ (show m)
        let ident' = (ident $ header $ m)
        r <- lift $ atomically $ lookupAndDelete b ident'
        case r of
          Nothing -> throwE "ID not in book"
          Just mvar -> lift $ atomically $ tryPutTMVar mvar m ) 
      (do
          debugM nameM $ (nick c) ++ " going dead"
          atomically $ writeTVar si True))
    killThread
    (\t -> do 
        let chan = Resolver { tid = t
                            , book = b
                            , config = c
                            , dead = si
                            }
        return $ T.Resolver { T.delete = delete chan
                            , T.resolve = resolve chan
                            }
    )
    
delete r = killThread (tid r)

resolve r a = do
  let nameF = nameM ++ ".resolve"
  mvar <- newEmptyTMVarIO
  bracketOnError
    (atomically $ allocate (book r) (ident $ header $ a) mvar)
    (\ident_ -> atomically $ lookupAndDelete (book r) ident_)
    (\ident_ -> do 
        let a_ = a { header = (header a) { ident = ident_ }}

        debugM nameF $ (nick $ config $ r) ++ " <- " ++ (show a_)

        bs <- case E.encode E.message a_ of
          Left s -> throw $ EncodeError s
          Right b -> return $ BSL.toStrict b


        forkIO $ do
          debugM nameF $ (nick $ config r) ++ "trying to send " ++ show bs
          catch ((send $ config $ r) bs) (\e -> debugM nameF $ show (e :: SomeException))
            
        x <- atomically $ do
          a <- readTVar $ dead r
          if a then tryTakeTMVar mvar
            else 
            Just <$> takeTMVar mvar
        case x of
          Nothing -> throwIO Dead
          Just x -> return x
    )
    
allocate :: (Eq i, Hashable i, Num i) => M.Map i a -> i -> a -> STM i
allocate b i a = let loop i = do
                       m <- do
                         r <- M.lookup i b
                         case r of
                           Nothing -> do
                             M.insert a i b
                             return True
                           Just _ -> return False
                       if m then return i
                         else loop (i + 1)
                 in loop i

lookupAndDelete :: (Eq i, Hashable i) => M.Map i a -> i -> STM (Maybe a)
lookupAndDelete b i = do
  mvar <- M.lookup i b
  case mvar of
    Nothing -> return Nothing
    Just mvar -> do
      M.delete i b
      return $ Just mvar

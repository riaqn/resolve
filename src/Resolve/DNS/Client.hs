{-# LANGUAGE Unsafe #-}
module Resolve.DNS.Client where

import qualified Resolve.Types as T
import Resolve.DNS.Types
import Resolve.DNS.Transport.Types

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

import System.Log.Logger


import qualified STMContainers.Map as M

nameM = "Resolve.DNS.Client"

data Dead = Dead
             deriving (Typeable, Show)

instance Exception Dead where
  toException = dnsExceptionToException
  fromException = dnsExceptionFromException

data Config = Config { transport :: Transport
                     , nickname :: String
                     }
              
data Record = Record { book :: M.Map Word16 (TMVar Message)
                     , config :: Config
                     , dead :: TVar Bool
                     }

new :: Config -> IO (T.Resolver Message Message)
new c = do
  b <- M.newIO
  d <- newTVarIO False

  bracketOnError
    (do
        tid <- forkFinally
          (forever $ runExceptT $ do  -- EitherT String IO ()
              let nameF = nameM ++ ".recv"
              m <- lift $ recv $ transport c
              let ident' = (ident $ header $ m)
              r <- lift $ atomically $ lookupAndDelete b ident'
              case r of
                Nothing -> throwE "ID not in book"
                Just mvar -> lift $ atomically $ tryPutTMVar mvar m)
          (\_ -> atomically $ writeTVar d True)
          
        return (T.Resolver { T.resolve = resolve $ Record { book = b
                                                        , config = c
                                                        , dead = d
                                                        }
                           , T.delete = killThread tid
                           })
    )
    (\r -> T.delete r)
    (\r -> return r)
    

resolve :: Record -> T.Resolve Message Message
resolve r a = do 
  mvar <- newEmptyTMVarIO
  bracketOnError
    (atomically $ allocate (book r) (ident $ header $ a) mvar)
    (\ident_ -> atomically $ lookupAndDelete (book r) ident_)
    (\ident_ -> do 
        let a_ = a { header = (header a) { ident = ident_ }}
        forkIO $ (send $ transport $ config $ r) a
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

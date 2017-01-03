module Resolve.DNS.Transport.Multiplex where

import Resolve.DNS.Utils
import Resolve.DNS.Exceptions
import qualified Resolve.DNS.Transport.Types as T

import Data.Word

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL

import Control.Monad.STM
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar


import Data.Typeable

import qualified STMContainers.Map as M

nameM = "Resolve.DNS.Transport.Multiplex"

data Dead = Dead
             deriving (Typeable, Show)

instance Exception Dead where
  toException = errorToException
  fromException = errorFromException

data Multiplex = Multiplex { book :: M.Map Word16 (TMVar ByteString)
                           , dead :: TVar Bool
                           , transport :: T.Transport
                           , delete :: IO ()
                           , ident :: TVar Word16
                           }

multiplex :: T.Transport -> IO Multiplex
multiplex t = do
  b <- M.newIO
  d <- newTVarIO False
  s <- newTVarIO 0
  
  bracketOnError
    (do
        tid <- forkFinally
          (forever $ do  -- EitherT String IO ()
              bs <- T.recv $ t
              forkIO $ void $ runExceptT $ do 
                when (BSL.length bs < 2) $ throwE "shorter than 2B?"
                let (bs_id, bs_res) = BSL.splitAt 2 bs
                let ident = toWord16 $ BSL.toStrict bs_id
                r <- lift $ atomically $ M.lookup ident b
                case r of
                  Nothing -> throwE "ID not in book"
                  Just mvar -> lift $ atomically $ putTMVar mvar bs_res)
          (\_ -> atomically $ writeTVar d True)
          
        return $ Multiplex { book = b
                           , dead = d
                           , transport = t
                           , delete = killThread tid
                           , ident = s
                           }
    )
    (\r -> delete r)
    (\r -> return r)

new :: Multiplex -> IO T.Transport
new m = do
  r <- newEmptyTMVarIO
  bracketOnError
    (do
        let allocate = do
              x <- atomically $ do
                i <- readTVar $ ident m
                writeTVar (ident m) (i + 1)
                x <- M.lookup i $ book m
                case x of
                  Nothing -> do
                    M.insert r i (book m)
                    return $ Just i
                  Just _ -> return Nothing
              case x of
                Nothing -> allocate
                Just i -> return i

        ident <- allocate
        let bs_ident = fromWord16 ident
  
        return $ T.Transport { T.send = \bs -> T.send (transport m) (BSL.append (BSL.fromStrict bs_ident) bs)
                             , T.recv = do
                                 x <- atomically $ do
                                   d <- readTVar $ dead m
                                   if d then tryTakeTMVar r
                                     else Just <$> takeTMVar r
                                 case x of
                                   Nothing -> throwIO Dead
                                   Just b -> return b
                             , T.delete = atomically $ M.delete ident (book m)
                             }
    )
    (\t -> T.delete t)
    (\t -> return t)

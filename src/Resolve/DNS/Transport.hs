module Resolve.DNS.Transport where

import qualified Resolve.Types as T
import Resolve.DNS.Utils
import Resolve.DNS.Exceptions
import Resolve.DNS.Transport.Types

import Data.Word
import Data.Hashable

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

nameM = "Resolve.DNS.Client"

data Dead = Dead
             deriving (Typeable, Show)

instance Exception Dead where
  toException = errorToException
  fromException = errorFromException

data Record = Record { book :: M.Map Word16 (TMVar ByteString)
                     , dead :: TVar Bool
                     , transport :: Transport
                     }

new :: Transport -> IO (T.Resolver ByteString ByteString)
new t = do
  b <- M.newIO
  d <- newTVarIO False

  bracketOnError
    (do
        tid <- forkFinally
          (forever $ runExceptT $ do  -- EitherT String IO ()
              bs <- lift $ recv $ t
              let (bs_id, _) = BSL.splitAt 2 bs
              let ident = toWord16 $ BSL.toStrict bs_id
              r <- lift $ atomically $ lookupAndDelete b ident
              case r of
                Nothing -> throwE "ID not in book"
                Just mvar -> lift $ atomically $ tryPutTMVar mvar bs)
          (\_ -> atomically $ writeTVar d True)
          
        return (T.Resolver { T.resolve = resolve $ Record { book = b
                                                          , dead = d
                                                          , transport = t
                                                          }
                           , T.delete = killThread tid
                           })
    )
    (\r -> T.delete r)
    (\r -> return r)

resolve :: Record -> T.Resolve ByteString ByteString
resolve r a = do 
  mvar <- newEmptyTMVarIO
  let (a_id, a_res) = BSL.splitAt 2 a
  bracketOnError
    (atomically $ allocate (book r) (toWord16 $ BSL.toStrict a_id) mvar)
    (\ident_ -> atomically $ lookupAndDelete (book r) ident_)
    (\ident_ -> do 
        let a_ = BSL.append (BSL.fromStrict $ fromWord16 ident_) a_res
        void $ forkIO $ (send $ transport $ r) a_
        b <- atomically $ do
          a <- readTVar $ dead r
          if a then tryTakeTMVar mvar
            else 
            Just <$> takeTMVar mvar
        case b of
          Nothing -> throwIO Dead
          -- MASQUERADE
          Just b -> do
            let (_, b_res) = BSL.splitAt 2 b
            return $ BSL.append a_id b_res
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

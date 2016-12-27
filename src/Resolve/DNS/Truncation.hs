module Resolve.DNS.Truncation where

import Resolve.Types
import Resolve.DNS.Types
import qualified Resolve.DNS.Channel as C
import qualified Resolve.DNS.UDP as UDP
import qualified Resolve.DNS.LiveTCP as TCP

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Exception

import System.Log.Logger

nameM = "Resolve.DNS.Truncation"


data Config = Config { udp :: Resolve Message Message
                     , tcp :: Resolve Message Message
                     }

truncation :: Config -> Resolve Message Message
truncation c a = do
  let nameF = nameM
  b <- try (udp c a)
  res <- case b of
    Left UDP.QueryTooLong -> do 
        debugM nameF "query too long"
        return Nothing
    Right b -> if tc $ header $ b then do
      debugM nameF "response truncated"
      return Nothing
      else 
      return $ Just b
  case res of
    Nothing -> do
      debugM nameF "using TCP"
      tcp c a
    Just b -> return b


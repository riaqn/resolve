module Resolve.DNS.Lookup where

import qualified Resolve.Types as T
import qualified Resolve.DNS.Exceptions as E
import Resolve.DNS.Types hiding (Response, Query)
import qualified Resolve.DNS.Types as DNS
import Resolve.DNS.EDNS.Types as EDNS
import qualified Resolve.DNS.EDNS.Encode as EE


import qualified Resolve.DNS.Transport.UDP as UDP

import Control.Monad.STM
import Control.Monad
import Control.Exception
import Control.Concurrent.STM.TVar
import Data.Typeable
import Data.List
import Data.Word

data Query = Query { qquestion :: [Question]
                   , qopt :: [Option]
                   }
             
data Response = Response { ranswer ::  [RR]
                         , rauthority :: [RR]
                         , radditional :: [RR]
                         , ropt :: Maybe [Option]
                         }

data Error = Truncated
           | WierdResponse String
           | ErrorResponse (Maybe Word8) RCODE
             deriving (Typeable, Show)

instance Exception Error where
  toException = E.errorToException
  fromException = E.errorFromException

data Config = Config { udp :: T.Resolve Message Message
                     , tcp :: T.Resolve Message Message
                     , setPayload :: Word16 -> IO ()
                     , p_this :: IO (Word16)
                     }

data Lookup = Lookup { config :: Config
                     , p_that :: TVar Word16
                     } 

new :: Config -> IO (T.Resolver Query Response)
new c = do
  p <- newTVarIO 512 -- server's UDP payload
  return $ T.Resolver { T.resolve = resolve $ Lookup { config = c
                                                     , p_that = p
                                                     }
                      , T.delete = return ()
                    }

resolve :: Lookup -> T.Resolve Query Response
resolve l q = do
  m <- tryJust (\e -> if (fromException e == Just UDP.QueryTooLong) then Just Nothing
                      else case fromException e of
                             Just Truncated -> Just Nothing
                             _ -> Nothing)
       (resolve' (udp $ config $ l) l q)

  b' <- case m of
    Left b -> return Nothing
    Right b -> (return $ Just b)
    
  case b' of
      Just b -> return b
      Nothing -> resolve' (tcp $ config $ l) l q
  
resolve' :: T.Resolve Message Message -> Lookup -> T.Resolve Query Response
resolve' r l q = do
  this <- p_this $ config $ l
  that <- atomically $ readTVar (p_that l)
  
  setPayload (config l) (if this < that then this else that)
  
  b <- r $ Message { header = Header { ident = 0
                                     , qr = DNS.Query
                                     , opcode = STD
                                     , aa = False
                                     , tc = False
                                     , rd = True
                                     , ra = False
                                     , zero = 0
                                     , rcode = NoErr
                                     }
                   , question = (qquestion q)
                   , answer = []
                   , authority = []
                   , additional = []
                   , opt = Just $ OPT { udp_size = this
                                      , ext_rcode = 0
                                      , version = 0
                                      , dnssec_ok = False
                                      , options = (qopt q)
                                      }
                   }
  let h = header b
  when (ident h /= 0 || 
        qr h /= DNS.Query ||
        opcode h /= STD ||
        rd h /= True
       ) $ throwIO $ WierdResponse "some field is strange"

  case opt b of
    Nothing -> return ()
    Just o -> do
      atomically $ when (udp_size o >= 512) $ writeTVar (p_that l) $ udp_size o
      when ((version o /= 0) || (dnssec_ok o /= False)) $ throwIO $ WierdResponse "some EDNS field is strange"
    
  when ((maybe False (\o -> ext_rcode o /=0 ) $ opt  b) || rcode h /= NoErr) $ throwIO $ ErrorResponse  (ext_rcode <$> opt b) (rcode h)
  when (tc $ h ) $ throwIO $ Truncated
  let  xs = qquestion q
       ys = question b
  when (not $ null (xs \\ ys) && null (ys \\ xs)) $ throwIO $ WierdResponse "question section not match"

  return $ Response { ranswer = answer b
                    , rauthority = answer b
                    , radditional = additional b
                    , ropt = options <$> opt b
                    }

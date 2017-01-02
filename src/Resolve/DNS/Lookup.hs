module Resolve.DNS.Lookup where

import qualified Resolve.Types as R
import Resolve.DNS.Types hiding (Query, Response)
import qualified Resolve.DNS.Types as T
import Resolve.DNS.EDNS.Types
import qualified Resolve.DNS.Encode as EC
import qualified Resolve.DNS.Decode as DC


import qualified Resolve.DNS.Exceptions as E

import Control.Monad.STM
import Control.Monad
import Control.Exception
import Control.Concurrent.STM.TVar
import Data.Typeable
import Data.Maybe
import Data.Word
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL

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
           | NoResolver
             deriving (Typeable, Show)

instance Exception Error where
  toException = E.errorToException
  fromException = E.errorFromException

data Config = Config { udp :: Maybe (R.Resolve ByteString ByteString, IO Word16)
                     , tcp :: Maybe (R.Resolve ByteString ByteString)
                     }

data Lookup = Lookup { config :: Config
                     , p_that :: TVar Word16
                     } 

new :: Config -> IO (R.Resolver Query Response)
new c = do
  p <- newTVarIO 512
  return $ R.Resolver { R.resolve = resolve $ Lookup { config = c
                                                     , p_that = p
                                                     }
                      , R.delete = return ()
                      }

resolve :: Lookup -> R.Resolve Query Response
resolve l q = do
  this <- fromMaybe (return 512) (snd <$> (udp $ config $ l))
  let a =  Message { header = Header { ident = 0
                                     , qr = T.Query
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
  bs_a <- case EC.encode EC.message a of
    Left e -> throwIO e
    Right bs -> return bs

  that <- atomically $ readTVar $ p_that l
  let p = min this that

  let resolve' r l bs_a = do
        bs_b <- r bs_a
        b <- case DC.decodeMessage (BSL.toStrict bs_b) of
          Left e -> throwIO $ DC.Error e
          Right b -> return b

        case opt b of
          Nothing -> return ()
          Just o -> do
            atomically $ when (udp_size o >= 512) $ writeTVar (p_that l) $ udp_size o
            when ((version o /= 0) || (dnssec_ok o /= False)) $ throwIO $ WierdResponse "some EDNS field is strange"
        return b

  b <- if BSL.length bs_a > fromIntegral p then return Nothing
    else maybe (return Nothing) (\r -> do
                                    b <- resolve' r l bs_a
                                    if tc $ header $ b then return Nothing
                                      else return $ Just b
                                ) (fst <$> (udp $ config $ l))

  b <- case b of
    Nothing -> maybe (throwIO NoResolver) (\r -> do
                                              b <- resolve' r l bs_a
                                              when (tc $ header $ b) $ throwIO $ WierdResponse "TC bit is set in a TCP response"
                                              return b) (tcp $ config $ l)
    Just b -> return b

  let h = header b
  when (ident h /= 0 || 
        qr h /= T.Response ||
        opcode h /= STD ||
        rd h /= True
       ) $ throwIO $ WierdResponse "some field is strange"
    
  when ((maybe False (\o -> ext_rcode o /=0 ) $ opt  b) || rcode h /= NoErr) $
    throwIO $ ErrorResponse  (ext_rcode <$> opt b) (rcode h)
  
  return $ Response { ranswer = answer b
                    , rauthority = answer b
                    , radditional = additional b
                    , ropt = options <$> opt b
                    }

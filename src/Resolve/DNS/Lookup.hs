module Resolve.DNS.Lookup
  ( Query(Query, qquestion, qopt)
  , Response(Response, ranswer, rauthority, radditional, ropt)
  , Config(Config, tcp, udp)
  , new
  ) where

import qualified Resolve.Types as R
import Resolve.DNS.Types hiding (Query, Response)
import qualified Resolve.DNS.Types as T
import Resolve.DNS.EDNS.Types
import qualified Resolve.DNS.Encode as EC
import qualified Resolve.DNS.Decode as DC

import qualified Resolve.DNS.Transport.Types as TT
import qualified Resolve.DNS.Transport.Multiplex as M

import qualified Resolve.DNS.Exceptions as E

import Control.Monad.STM
import Control.Monad
import Control.Exception
import Control.Concurrent.STM.TVar
import Data.Typeable
import Data.List
import Data.Word
import qualified Data.ByteString.Lazy as BSL

import System.Log.Logger

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

nameM = "Resolve.DNS.Lookup"

data Config = Config { udp :: (TT.Transport , IO Word16)
                     , tcp :: TT.Transport
                     }

data Lookup = Lookup { m_udp :: M.Multiplex 
                     , m_tcp :: M.Multiplex
                     , p_this :: IO Word16
                     , p_that :: TVar Word16
                     } 

new :: Config -> IO (R.Resolver Query Response)
new c = do
  p <- newTVarIO 512
  bracketOnError
    (do
        m_udp' <- M.multiplex $ (fst $ udp c)
        m_tcp' <- M.multiplex $ tcp c
        return $ R.Resolver { R.resolve = resolve $ Lookup { m_udp = m_udp'
                                                           , m_tcp = m_tcp'
                                                           , p_that = p
                                                           , p_this = snd $ udp c
                                                           }
                            , R.delete = do
                                M.delete m_udp'
                                M.delete m_tcp'
                      }
    )
    (\r -> R.delete r)
    return

resolve :: Lookup -> R.Resolve Query Response
resolve l q = do
  this <- p_this l
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
                   , opt = Just $ OPT { udp_size = max this 512
                                      , ext_rcode = 0
                                      , version = 0
                                      , dnssec_ok = False
                                      , options = (qopt q)
                                      }
                   }
  debugM nameM $ "sending " ++ (show a)
  bs_a <- case EC.encode EC.message a of
    Left e -> throwIO e
    Right bs -> return bs

  that <- atomically $ readTVar $ p_that l
  let p = min this that

  let resolve' m p_that bs_a = do
        bracket
          (M.new m)
          (\t -> TT.delete t)
          (\t -> do 
              let (bs_a_id, bs_a') = BSL.splitAt 2 bs_a
              TT.send t bs_a'

              let loop = do 
                    bs_b' <- TT.recv t
                    let bs_b = BSL.append bs_a_id bs_b'
                    b <- case DC.decodeMessage (BSL.toStrict bs_b) of
                           Left e -> do
                             debugM nameM $ "decode error: " ++ show (e)
                             loop
                           Right b -> return b
                    if (null $ question b \\ question a) && (null $ question a \\ question b) then
                      return b
                      else loop
              b <- loop
              case opt b of
                Nothing -> return ()
                Just o -> do
                  atomically $ when (udp_size o >= 512) $ writeTVar p_that $ udp_size o
                  when ((version o /= 0) || (dnssec_ok o /= False)) $ throwIO $ WierdResponse "some EDNS field is strange"
              return b
          )

  b <- if BSL.length bs_a > fromIntegral p then return Nothing
    else do
    b <- resolve' (m_udp l) (p_that l) bs_a
    debugM nameM $ "UDP recvd " ++ (show b)
    if tc $ header $ b then return Nothing
      else return $ Just b

  b <- case b of
    Nothing -> do 
      b <- resolve' (m_tcp l) (p_that l) bs_a
      debugM nameM $ "TCP recvd " ++ (show b)
      when (tc $ header $ b) $ throwIO $ WierdResponse "TC bit is set in a TCP response"
      return b
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
                    , rauthority = authority b
                    , radditional = additional b
                    , ropt = options <$> opt b
                    }

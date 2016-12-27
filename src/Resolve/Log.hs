module Resolve.Log where

import Resolve.Types
import Control.Exception

log :: (Show a, Show b) => (String -> IO()) -> (String -> IO()) -> Resolve a b -> Resolve a b
log error info r a = do
  catch 
    (do
        b <- r a
        info $ show a ++ "->" ++ show b
        return b
    )
    (\e -> do
        error $ show a ++ "->" ++ show (e :: SomeException)
        throw e
    )

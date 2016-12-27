module Resolve.Types where

import Data.Typeable
import Control.Exception

type Resolve a b = a -> IO b
data Resolver a b = Resolver { resolve :: Resolve a b
                             , delete :: IO ()
                             }

data ResolveException where
  ResolveException :: Exception e => e -> ResolveException
  deriving (Typeable)

instance Show ResolveException where
  show (ResolveException e) = show e

instance Exception ResolveException

resolveExceptionToException :: Exception e => e -> SomeException
resolveExceptionToException = toException . ResolveException

resolveExceptionFromException x = do
  ResolveException a <- fromException x
  cast a

module Hydra.Util.Context where

import Hydra.Compute
import Hydra.Core
import Hydra.Monads
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms

import qualified Data.Map as M


getAttr :: String -> Flow s (Maybe (Term Meta))
getAttr key = Flow q
  where
    q s0 t0 = FlowWrapper (Just $ M.lookup key $ traceOther t0) s0 t0

nextCount :: String -> Flow s Int
nextCount attrName = do
  c <- getAttr attrName
  count <- case c of
    Nothing -> pure 0
    Just l -> Terms.expectInt32 l
  putAttr attrName (Terms.int32 $ count + 1)
  return count

putAttr :: String -> Term Meta -> Flow s ()
putAttr key val = Flow q
  where
    q s0 t0 = FlowWrapper (Just ()) s0 (t0 {traceOther = M.insert key val $ traceOther t0})

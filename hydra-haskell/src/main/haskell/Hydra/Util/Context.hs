module Hydra.Util.Context where

import Hydra.All
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms

import qualified Data.Map as M
import qualified Data.Maybe as Y


getAttr :: String -> Flow s (Maybe (Term Meta))
getAttr key = Flow q
  where
    q s0 t0 = FlowWrapper (Just $ M.lookup key $ traceOther t0) s0 t0

getAttrWithDefault :: String -> Term Meta -> Flow s (Term Meta)
getAttrWithDefault key def = Y.fromMaybe def <$> getAttr key

nextCount :: String -> Flow s Int
nextCount attrName = do
  count <- getAttrWithDefault attrName (Terms.int32 0) >>= Terms.expectInt32
  putAttr attrName (Terms.int32 $ count + 1)
  return count

putAttr :: String -> Term Meta -> Flow s ()
putAttr key val = Flow q
  where
    q s0 t0 = FlowWrapper (Just ()) s0 (t0 {traceOther = M.insert key val $ traceOther t0})

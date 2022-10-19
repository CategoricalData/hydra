module Hydra.Util.Context where

import Hydra.Monads
import qualified Hydra.Impl.Haskell.Dsl.Literals as Literals


nextCount :: String -> Flow s Int
nextCount attrName = do
  c <- getAttr attrName
  count <- case c of
    Nothing -> pure 0
    Just l -> Literals.expectInt32 l
  putAttr attrName (Literals.int32 $ count + 1)
  return count

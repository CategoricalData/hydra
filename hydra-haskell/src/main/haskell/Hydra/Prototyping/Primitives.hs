module Hydra.Prototyping.Primitives (
    lookupPrimitiveFunction,
  ) where

import Hydra.Core
import Hydra.Evaluation

import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Set  as S


lookupPrimitiveFunction :: Context -> Name -> Maybe PrimitiveFunction
lookupPrimitiveFunction context fn = M.lookup fn $ contextFunctions context

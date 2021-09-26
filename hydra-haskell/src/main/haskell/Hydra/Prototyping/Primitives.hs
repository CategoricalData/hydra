module Hydra.Prototyping.Primitives (
  lookupPrimitiveFunction,
  primitiveFunctionArity,
) where

import Hydra.Core
import Hydra.Evaluation

import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Set  as S


lookupPrimitiveFunction :: Context -> Name -> Maybe PrimitiveFunction
lookupPrimitiveFunction context fn = M.lookup fn $ contextFunctions context

primitiveFunctionArity :: PrimitiveFunction -> Int
primitiveFunctionArity = arity . primitiveFunctionType
  where
    arity (FunctionType dom cod) = 1 + case cod of
      TypeFunction ft -> arity ft
      _ -> 0

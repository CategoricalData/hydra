-- | Type inference following Algorithm W.

module Hydra.Inference where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Typing as Typing
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | An empty inference context
emptyInferenceContext :: Typing.InferenceContext
emptyInferenceContext = Typing.InferenceContext {
  Typing.inferenceContextSchemaTypes = (M.fromList []),
  Typing.inferenceContextPrimitiveTypes = (M.fromList []),
  Typing.inferenceContextDataTypes = (M.fromList []),
  Typing.inferenceContextDebug = False}

-- | Type variable naming convention follows Haskell: t0, t1, etc.
normalTypeVariable :: (Int -> Core.Name)
normalTypeVariable i = (Core.Name (Strings.cat2 "t" (Literals.showInt32 i)))
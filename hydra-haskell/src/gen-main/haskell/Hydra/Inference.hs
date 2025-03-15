-- | Type inference following Algorithm W.

module Hydra.Inference where

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
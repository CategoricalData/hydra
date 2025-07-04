-- | Types supporting type inference.

module Hydra.Typing where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | The context provided to type inference, including various typing enviroments.
data InferenceContext = 
  InferenceContext {
    -- | A fixed typing environment which is derived from the schema of the graph.
    inferenceContextSchemaTypes :: (M.Map Core.Name Core.TypeScheme),
    -- | A fixed typing environment which is derived from the set of primitives in the graph.
    inferenceContextPrimitiveTypes :: (M.Map Core.Name Core.TypeScheme),
    -- | A mutable typing environment which is specific to the current graph being processed. This environment is (usually) smaller than the schema and primitive typing environments, and is subject to global substitutions.
    inferenceContextDataTypes :: (M.Map Core.Name Core.TypeScheme),
    inferenceContextDebug :: Bool}
  deriving (Eq, Ord, Read, Show)

_InferenceContext = (Core.Name "hydra.typing.InferenceContext")

_InferenceContext_schemaTypes = (Core.Name "schemaTypes")

_InferenceContext_primitiveTypes = (Core.Name "primitiveTypes")

_InferenceContext_dataTypes = (Core.Name "dataTypes")

_InferenceContext_debug = (Core.Name "debug")

-- | The result of applying inference rules to a term.
data InferenceResult = 
  InferenceResult {
    inferenceResultTerm :: Core.Term,
    inferenceResultType :: Core.Type,
    inferenceResultSubst :: TypeSubst}
  deriving (Eq, Ord, Read, Show)

_InferenceResult = (Core.Name "hydra.typing.InferenceResult")

_InferenceResult_term = (Core.Name "term")

_InferenceResult_type = (Core.Name "type")

_InferenceResult_subst = (Core.Name "subst")

-- | A substitution of term variables for terms
newtype TermSubst = 
  TermSubst {
    unTermSubst :: (M.Map Core.Name Core.Term)}
  deriving (Eq, Ord, Read, Show)

_TermSubst = (Core.Name "hydra.typing.TermSubst")

-- | An assertion that two types can be unified into a single type
data TypeConstraint = 
  TypeConstraint {
    typeConstraintLeft :: Core.Type,
    typeConstraintRight :: Core.Type,
    -- | A description of the type constraint which may be used for tracing or debugging
    typeConstraintComment :: String}
  deriving (Eq, Ord, Read, Show)

_TypeConstraint = (Core.Name "hydra.typing.TypeConstraint")

_TypeConstraint_left = (Core.Name "left")

_TypeConstraint_right = (Core.Name "right")

_TypeConstraint_comment = (Core.Name "comment")

-- | A substitution of type variables for types
newtype TypeSubst = 
  TypeSubst {
    unTypeSubst :: (M.Map Core.Name Core.Type)}
  deriving (Eq, Ord, Read, Show)

_TypeSubst = (Core.Name "hydra.typing.TypeSubst")

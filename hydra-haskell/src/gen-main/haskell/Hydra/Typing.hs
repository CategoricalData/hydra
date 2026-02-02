-- Note: this is an automatically generated file. Do not edit.

-- | Types supporting type inference and type reconstruction.

module Hydra.Typing where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | The context provided to type inference, including various typing environments.
data InferenceContext = 
  InferenceContext {
    -- | A fixed typing environment which is derived from the schema of the graph.
    inferenceContextSchemaTypes :: (M.Map Core.Name Core.TypeScheme),
    -- | A fixed typing environment which is derived from the set of primitives in the graph.
    inferenceContextPrimitiveTypes :: (M.Map Core.Name Core.TypeScheme),
    -- | A mutable typing environment which is specific to the current graph being processed. This environment is (usually) smaller than the schema and primitive typing environments, and is subject to global substitutions.
    inferenceContextDataTypes :: (M.Map Core.Name Core.TypeScheme),
    -- | A mutable map from type variable names to their accumulated class constraints. This is populated during type inference when operations requiring Eq or Ord are encountered.
    inferenceContextClassConstraints :: (M.Map Core.Name Core.TypeVariableMetadata),
    -- | Whether to enable debug output during type inference
    inferenceContextDebug :: Bool}
  deriving (Eq, Ord, Read, Show)

_InferenceContext = (Core.Name "hydra.typing.InferenceContext")

_InferenceContext_schemaTypes = (Core.Name "schemaTypes")

_InferenceContext_primitiveTypes = (Core.Name "primitiveTypes")

_InferenceContext_dataTypes = (Core.Name "dataTypes")

_InferenceContext_classConstraints = (Core.Name "classConstraints")

_InferenceContext_debug = (Core.Name "debug")

-- | The result of applying inference rules to a term.
data InferenceResult = 
  InferenceResult {
    -- | The term which was inferred
    inferenceResultTerm :: Core.Term,
    -- | The inferred type of the term
    inferenceResultType :: Core.Type,
    -- | The type substitution resulting from unification
    inferenceResultSubst :: TypeSubst,
    -- | Class constraints discovered during inference (e.g., Ord constraints from Map.lookup)
    inferenceResultClassConstraints :: (M.Map Core.Name Core.TypeVariableMetadata)}
  deriving (Eq, Ord, Read, Show)

_InferenceResult = (Core.Name "hydra.typing.InferenceResult")

_InferenceResult_term = (Core.Name "term")

_InferenceResult_type = (Core.Name "type")

_InferenceResult_subst = (Core.Name "subst")

_InferenceResult_classConstraints = (Core.Name "classConstraints")

-- | A substitution of term variables for terms
newtype TermSubst = 
  TermSubst {
    unTermSubst :: (M.Map Core.Name Core.Term)}
  deriving (Eq, Ord, Read, Show)

_TermSubst = (Core.Name "hydra.typing.TermSubst")

-- | An assertion that two types can be unified into a single type
data TypeConstraint = 
  TypeConstraint {
    -- | The left-hand side of the constraint
    typeConstraintLeft :: Core.Type,
    -- | The right-hand side of the constraint
    typeConstraintRight :: Core.Type,
    -- | A description of the type constraint which may be used for tracing or debugging
    typeConstraintComment :: String}
  deriving (Eq, Ord, Read, Show)

_TypeConstraint = (Core.Name "hydra.typing.TypeConstraint")

_TypeConstraint_left = (Core.Name "left")

_TypeConstraint_right = (Core.Name "right")

_TypeConstraint_comment = (Core.Name "comment")

-- | A typing environment used for type reconstruction (typeOf) over System F terms
data TypeContext = 
  TypeContext {
    -- | A mapping of lambda- and let-bound variables to their types
    typeContextTypes :: (M.Map Core.Name Core.Type),
    -- | Any additional metadata about lambda- and let-bound variables
    typeContextMetadata :: (M.Map Core.Name Core.Term),
    -- | The set of type variables introduced by enclosing type lambdas
    typeContextTypeVariables :: (S.Set Core.Name),
    -- | The set of term variables introduced by lambdas (even if untyped)
    typeContextLambdaVariables :: (S.Set Core.Name),
    -- | The set of term variables introduced by let bindings (even if untyped)
    typeContextLetVariables :: (S.Set Core.Name),
    -- | The schema types, primitive types, and data types of the graph
    typeContextInferenceContext :: InferenceContext}
  deriving (Eq, Ord, Read, Show)

_TypeContext = (Core.Name "hydra.typing.TypeContext")

_TypeContext_types = (Core.Name "types")

_TypeContext_metadata = (Core.Name "metadata")

_TypeContext_typeVariables = (Core.Name "typeVariables")

_TypeContext_lambdaVariables = (Core.Name "lambdaVariables")

_TypeContext_letVariables = (Core.Name "letVariables")

_TypeContext_inferenceContext = (Core.Name "inferenceContext")

-- | A substitution of type variables for types
newtype TypeSubst = 
  TypeSubst {
    unTypeSubst :: (M.Map Core.Name Core.Type)}
  deriving (Eq, Ord, Read, Show)

_TypeSubst = (Core.Name "hydra.typing.TypeSubst")

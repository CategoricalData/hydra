-- Note: this is an automatically generated file. Do not edit.

-- | Environment types for Coq code generation

module Hydra.Coq.Environment where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M
import qualified Data.Set as S

-- | Cross-module state threaded through the Coq encoder
data CoqEnvironment =
  CoqEnvironment {
    -- | The Hydra namespace of the module currently being encoded (e.g. "hydra.core"). Used by the name resolver to decide whether a cross-namespace reference needs to stay qualified.
    coqEnvironmentCurrentNamespace :: String,
    -- | Number of constructors in each union type, keyed by local type name (e.g. "Term" -> 14). Used to decide whether a match is exhaustive.
    coqEnvironmentConstructorCounts :: (M.Map String Int),
    -- | Local names (without namespace prefix) that are defined in more than one module. References to these must be kept fully qualified.
    coqEnvironmentAmbiguousNames :: (S.Set String),
    -- | Accessor names for record fields that were sanitized to unit due to Coq's strict positivity requirement. Applications of these accessors are replaced with hydra_unreachable at emission time.
    coqEnvironmentSanitizedAccessors :: (S.Set String)}
  deriving (Eq, Ord, Read, Show)

_CoqEnvironment = Core.Name "hydra.coq.environment.CoqEnvironment"

_CoqEnvironment_currentNamespace = Core.Name "currentNamespace"

_CoqEnvironment_constructorCounts = Core.Name "constructorCounts"

_CoqEnvironment_ambiguousNames = Core.Name "ambiguousNames"

_CoqEnvironment_sanitizedAccessors = Core.Name "sanitizedAccessors"

-- | Environment types for Coq code generation.
-- Threaded through the term and type encoders so that they have access to
-- cross-module state (constructor counts, ambiguous-name sets, etc.) without
-- passing each piece as a separate argument.

module Hydra.Sources.Coq.Environment where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                           ((>:))
import qualified Hydra.Dsl.Types                           as T
import qualified Hydra.Sources.Kernel.Types.Core           as Core


ns :: Namespace
ns = Namespace "hydra.coq.environment"

def :: String -> Type -> Binding
def = datatype ns

environment :: String -> Type
environment = typeref ns

core :: String -> Type
core = typeref Core.ns

module_ :: Module
module_ = Module ns (map toTypeDef definitions) [] [Core.ns] $
    Just "Environment types for Coq code generation"
  where
    definitions = [
      coqEnvironment]

-- | Cross-module state threaded through the Coq encoder.
-- Populated by the host-side pipeline (heads/haskell/.../ExtGeneration.hs) before
-- each module is encoded; consulted by encodeUnionElim, encodeTerm, etc. to
-- make output-level decisions (e.g., whether to emit a catch-all match arm or
-- whether a reference needs to stay fully qualified).
coqEnvironment :: Binding
coqEnvironment = def "CoqEnvironment" $
  doc "Cross-module state threaded through the Coq encoder" $
  T.record [
    "currentNamespace">:
      doc "The Hydra namespace of the module currently being encoded (e.g. \"hydra.core\"). Used by the name resolver to decide whether a cross-namespace reference needs to stay qualified." $
      T.string,
    "constructorCounts">:
      doc "Number of constructors in each union type, keyed by local type name (e.g. \"Term\" -> 14). Used to decide whether a match is exhaustive." $
      T.map T.string T.int32,
    "ambiguousNames">:
      doc "Local names (without namespace prefix) that are defined in more than one module. References to these must be kept fully qualified." $
      T.set T.string,
    "sanitizedAccessors">:
      doc "Accessor names for record fields that were sanitized to unit due to Coq's strict positivity requirement. Applications of these accessors are replaced with hydra_unreachable at emission time." $
      T.set T.string]

{-# LANGUAGE ScopedTypeVariables #-}

-- | Haskell-specific convenience layer over the generated Hydra.Dsl.Graph module.
-- Re-exports all generated DSL functions and adds non-standard helpers.

module Hydra.Overlay.Haskell.Dsl.Typed.Graph (
  module Hydra.Dsl.Graph,
  module Hydra.Overlay.Haskell.Dsl.Typed.Graph,
) where

import Hydra.Kernel
import Hydra.Overlay.Haskell.Dsl.Typed.Phantoms
import Hydra.Dsl.Graph

import qualified Hydra.Dsl.Lib.Maps as Maps
import qualified Hydra.Dsl.Lib.Sets as Sets


-- | Non-standard helpers

comparisonEqualTo :: TypedTerm Comparison
comparisonEqualTo = injectUnit _Comparison _Comparison_equalTo

comparisonGreaterThan :: TypedTerm Comparison
comparisonGreaterThan = injectUnit _Comparison _Comparison_greaterThan

comparisonLessThan :: TypedTerm Comparison
comparisonLessThan = injectUnit _Comparison _Comparison_lessThan

emptyGraph :: TypedTerm Graph
emptyGraph = graph
    Maps.empty  -- boundTerms
    Maps.empty  -- boundTypes
    Maps.empty  -- classConstraints
    Sets.empty  -- lambdaVariables
    Maps.empty  -- metadata
    Maps.empty  -- primitives
    Maps.empty  -- schemaTypes
    Sets.empty  -- typeVariables

{-# LANGUAGE ScopedTypeVariables #-}

-- | Haskell-specific convenience layer over the generated Hydra.Core.Dsl.Graph module.
-- Re-exports all generated DSL functions and adds non-standard helpers.

module Hydra.Core.Overlay.Haskell.Dsl.Meta.Graph (
  module Hydra.Core.Dsl.Graph,
  module Hydra.Core.Overlay.Haskell.Dsl.Meta.Graph,
) where

import Hydra.Kernel
import Hydra.Core.Overlay.Haskell.Dsl.Phantoms
import Hydra.Core.Dsl.Graph

import qualified Hydra.Core.Dsl.Lib.Maps as Maps
import qualified Hydra.Core.Dsl.Lib.Sets as Sets


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

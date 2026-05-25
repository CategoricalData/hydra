-- | Haskell-specific convenience layer over the generated Hydra.Dsl.Graph module.
-- Re-exports all generated DSL functions and adds non-standard helpers.

module Hydra.Dsl.Meta.Graph (
  module Hydra.Dsl.Graph,
  module Hydra.Dsl.Meta.Graph,
) where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms
import Hydra.Dsl.Graph

import qualified Hydra.Dsl.Meta.Lib.Maps as Maps
import qualified Hydra.Dsl.Meta.Lib.Sets as Sets


-- | Non-standard helpers

comparisonLessThan :: TTerm Comparison
comparisonLessThan = injectUnit _Comparison _Comparison_lessThan

comparisonEqualTo :: TTerm Comparison
comparisonEqualTo = injectUnit _Comparison _Comparison_equalTo

comparisonGreaterThan :: TTerm Comparison
comparisonGreaterThan = injectUnit _Comparison _Comparison_greaterThan

emptyGraph :: TTerm Graph
emptyGraph = graph
    Maps.empty  -- boundTerms
    Maps.empty  -- boundTypes
    Maps.empty  -- classConstraints
    Sets.empty  -- lambdaVariables
    Maps.empty  -- metadata
    Maps.empty  -- primitives
    Maps.empty  -- schemaTypes
    Sets.empty  -- typeVariables


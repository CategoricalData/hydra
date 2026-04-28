-- | Haskell-specific convenience layer over the generated Hydra.Dsl.Graph module.
-- Re-exports all generated DSL functions and adds non-standard helpers.

module Hydra.Dsl.Meta.Graph (
  module Hydra.Dsl.Graph,
  module Hydra.Dsl.Meta.Graph,
  DslGraph.primitiveName,
  DslGraph.primitiveTypeScheme,
  DslGraph.graphPrimitives,
) where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms
import Hydra.Dsl.Graph hiding (primitiveName, primitiveType, graphPrimitives)
import qualified Hydra.Dsl.Graph as DslGraph

import qualified Hydra.Dsl.Meta.Lib.Lists as Lists
import qualified Hydra.Dsl.Meta.Lib.Maps as Maps
import qualified Hydra.Dsl.Meta.Lib.Sets as Sets

import qualified Data.Map as M
import qualified Data.Set as S


-- | Non-standard helpers

comparisonLessThan :: TTerm Comparison
comparisonLessThan = injectUnit _Comparison _Comparison_lessThan

comparisonEqualTo :: TTerm Comparison
comparisonEqualTo = injectUnit _Comparison _Comparison_equalTo

comparisonGreaterThan :: TTerm Comparison
comparisonGreaterThan = injectUnit _Comparison _Comparison_greaterThan

typeClassEquality :: TTerm TypeClass
typeClassEquality = injectUnit _TypeClass _TypeClass_equality

typeClassOrdering :: TTerm TypeClass
typeClassOrdering = injectUnit _TypeClass _TypeClass_ordering

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

graphPrimitiveTypes :: TTerm Graph -> TTerm (M.Map Name TypeScheme)
graphPrimitiveTypes g = Maps.fromList (Lists.map
    ("_gpt_p" ~> pair (DslGraph.primitiveName $ var "_gpt_p") (DslGraph.primitiveTypeScheme $ var "_gpt_p"))
    (Maps.elems $ DslGraph.graphPrimitives g))

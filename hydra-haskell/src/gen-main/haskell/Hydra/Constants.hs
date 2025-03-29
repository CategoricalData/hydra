-- | A module for tier-0 constants.

module Hydra.Constants where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | An empty graph; no elements, no primitives, no schema, and an arbitrary body.
emptyGraph :: Graph.Graph
emptyGraph = Graph.Graph {
  Graph.graphElements = M.empty,
  Graph.graphEnvironment = M.empty,
  Graph.graphTypes = M.empty,
  Graph.graphBody = Core.TermLiteral $ Core.LiteralString "empty graph",
  Graph.graphPrimitives = M.empty,
  Graph.graphSchema = Nothing}

ignoredVariable :: String
ignoredVariable = "_"

key_classes :: Core.Name
key_classes = (Core.Name "classes")

key_debugId :: Core.Name
key_debugId = (Core.Name "debugId")

key_deprecated :: Core.Name
key_deprecated = (Core.Name "_deprecated")

key_description :: Core.Name
key_description = (Core.Name "description")

key_exclude :: Core.Name
key_exclude = (Core.Name "exclude")

-- | A flag which tells the language coders to encode a given encoded type as a term rather than a native type
key_firstClassType :: Core.Name
key_firstClassType = (Core.Name "firstClassType")

key_maxLength :: Core.Name
key_maxLength = (Core.Name "_maxLength")

key_minLength :: Core.Name
key_minLength = (Core.Name "_minLength")

key_preserveFieldName :: Core.Name
key_preserveFieldName = (Core.Name "_preserveFieldName")

key_type :: Core.Name
key_type = (Core.Name "type")

-- | A placeholder name for row types as they are being constructed
placeholderName :: Core.Name
placeholderName = (Core.Name "Placeholder")

-- | A maximum depth for nested flows. Currently, this is set very high because deep flows are common in type inference over the Hydra kernel.
maxTraceDepth :: Int
maxTraceDepth = 4000

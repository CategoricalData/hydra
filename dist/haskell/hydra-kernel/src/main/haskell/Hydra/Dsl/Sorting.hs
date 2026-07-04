-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.sorting

module Hydra.Dsl.Sorting where

import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Docs as Docs
import qualified Hydra.Dsl.Ast as DslAst
import qualified Hydra.Dsl.Coders as DslCoders
import qualified Hydra.Dsl.Constants as DslConstants
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Docs as DslDocs
import qualified Hydra.Dsl.Error.Checking as ErrorChecking
import qualified Hydra.Dsl.Error.Core as DslErrorCore
import qualified Hydra.Dsl.Error.File as DslErrorFile
import qualified Hydra.Dsl.Error.Packaging as DslErrorPackaging
import qualified Hydra.Dsl.Error.System as DslErrorSystem
import qualified Hydra.Dsl.Errors as DslErrors
import qualified Hydra.Dsl.File as DslFile
import qualified Hydra.Dsl.Graph as DslGraph
import qualified Hydra.Dsl.Json.Model as JsonModel
import qualified Hydra.Dsl.Packaging as DslPackaging
import qualified Hydra.Dsl.Parsing as DslParsing
import qualified Hydra.Dsl.Paths as DslPaths
import qualified Hydra.Dsl.Query as DslQuery
import qualified Hydra.Dsl.Relational as DslRelational
import qualified Hydra.Dsl.System as DslSystem
import qualified Hydra.Dsl.Tabular as DslTabular
import qualified Hydra.Dsl.Testing as DslTesting
import qualified Hydra.Dsl.Time as DslTime
import qualified Hydra.Dsl.Topology as DslTopology
import qualified Hydra.Dsl.Typing as DslTyping
import qualified Hydra.Dsl.Util as DslUtil
import qualified Hydra.Dsl.Validation as DslValidation
import qualified Hydra.Dsl.Variants as DslVariants
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.File as ErrorFile
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Error.System as ErrorSystem
import qualified Hydra.Errors as Errors
import qualified Hydra.File as File
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Sorting as Sorting
import qualified Hydra.System as System
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Time as Time
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S

-- | DSL reference to hydra.sorting.adjacencyListToMap
adjacencyListToMap :: Ord t0 => (Typed.TypedTerm [(t0, [t1])] -> Typed.TypedTerm (M.Map t0 [t1]))
adjacencyListToMap arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.sorting.adjacencyListToMap")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.sorting.adjacencyListsToGraph
adjacencyListsToGraph :: Typed.TypedTerm [(t0, [t0])] -> Typed.TypedTerm (M.Map Int [Int], (Int -> Maybe t0))
adjacencyListsToGraph arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.sorting.adjacencyListsToGraph")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.sorting.createOrderingIsomorphism
createOrderingIsomorphism :: Typed.TypedTerm [t0] -> Typed.TypedTerm [t0] -> Typed.TypedTerm (Topology.OrderingIsomorphism t1)
createOrderingIsomorphism arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.sorting.createOrderingIsomorphism")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.sorting.findReachableNodes
findReachableNodes :: Ord t0 => (Typed.TypedTerm (t0 -> S.Set t0) -> Typed.TypedTerm t0 -> Typed.TypedTerm (S.Set t0))
findReachableNodes arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.sorting.findReachableNodes")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.sorting.initialState
initialState :: Typed.TypedTerm Topology.TarjanState
initialState = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.sorting.initialState"))

-- | DSL reference to hydra.sorting.popStackUntil
popStackUntil :: Typed.TypedTerm Int -> Typed.TypedTerm Topology.TarjanState -> Typed.TypedTerm ([Int], Topology.TarjanState)
popStackUntil arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.sorting.popStackUntil")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.sorting.propagateTags
propagateTags :: Ord t1 => (Typed.TypedTerm [(t0, [t0])] -> Typed.TypedTerm [(t0, [t1])] -> Typed.TypedTerm [(t0, (S.Set t1))])
propagateTags arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.sorting.propagateTags")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))

-- | DSL reference to hydra.sorting.strongConnect
strongConnect :: Typed.TypedTerm (M.Map Int [Int]) -> Typed.TypedTerm Int -> Typed.TypedTerm Topology.TarjanState -> Typed.TypedTerm Topology.TarjanState
strongConnect arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.sorting.strongConnect")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.sorting.stronglyConnectedComponents
stronglyConnectedComponents :: Typed.TypedTerm (M.Map Int [Int]) -> Typed.TypedTerm [[Int]]
stronglyConnectedComponents arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.sorting.stronglyConnectedComponents")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.sorting.topologicalSort
topologicalSort :: Typed.TypedTerm [(t0, [t0])] -> Typed.TypedTerm (Either [[t0]] [t0])
topologicalSort arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.sorting.topologicalSort")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.sorting.topologicalSortComponents
topologicalSortComponents :: Typed.TypedTerm [(t0, [t0])] -> Typed.TypedTerm [[t0]]
topologicalSortComponents arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.sorting.topologicalSortComponents")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.sorting.topologicalSortNodes
topologicalSortNodes :: Typed.TypedTerm (t0 -> t1) -> Typed.TypedTerm (t0 -> [t1]) -> Typed.TypedTerm [t0] -> Typed.TypedTerm [[t0]]
topologicalSortNodes arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.sorting.topologicalSortNodes")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

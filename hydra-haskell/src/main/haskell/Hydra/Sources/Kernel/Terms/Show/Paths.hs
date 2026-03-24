module Hydra.Sources.Kernel.Terms.Show.Paths where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths       as Paths
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Module       as Module
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Names as Names
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting


ns :: Namespace
ns = Namespace "hydra.show.paths"

module_ :: Module
module_ = Module ns elements
    [Names.ns, Rewriting.ns]
    kernelTypesNamespaces $
    Just ("Utilities for working with subterm steps and paths.")
  where
   elements = [
     toTermDefinition subtermStep,
     toTermDefinition termToSubtermGraph]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

subtermStep :: TBinding (SubtermStep -> Maybe String)
subtermStep = define "subtermStep" $
  doc "Convert a subterm step to a string representation" $
  "step" ~>
  "idx" <~ ("i" ~> nothing) $  -- TODO: restore index functionality
  "idxSuff" <~ ("suffix" ~> "i" ~>
    Maybes.map ("s" ~> Strings.cat2 (var "s") (var "suffix")) (var "idx" @@ var "i")) $
  cases _SubtermStep (var "step")
    Nothing [
    _SubtermStep_annotatedBody>>: constant nothing,
    _SubtermStep_applicationFunction>>: constant (just (string "fun")),
    _SubtermStep_applicationArgument>>: constant (just (string "arg")),
    _SubtermStep_lambdaBody>>: constant (just (string "body")),
    _SubtermStep_unionCasesDefault>>: constant (just (string "default")),
    _SubtermStep_unionCasesBranch>>: "name" ~> just (Strings.cat2 (string ".") (Core.unName (var "name"))),
    _SubtermStep_letBody>>: constant (just (string "in")),
    _SubtermStep_letBinding>>: "name" ~> just (Strings.cat2 (Core.unName (var "name")) (string "=")),
    _SubtermStep_listElement>>: "i" ~> var "idx" @@ var "i",
    _SubtermStep_mapKey>>: "i" ~> var "idxSuff" @@ (string ".key") @@ var "i",
    _SubtermStep_mapValue>>: "i" ~> var "idxSuff" @@ (string ".value") @@ var "i",
    _SubtermStep_maybeTerm>>: constant (just (string "just")),
    _SubtermStep_productTerm>>: "i" ~> var "idx" @@ var "i",
    _SubtermStep_recordField>>: "name" ~> just (Strings.cat2 (string ".") (Core.unName (var "name"))),
    _SubtermStep_setElement>>: "i" ~> var "idx" @@ var "i",
    _SubtermStep_sumTerm>>: constant nothing,
    _SubtermStep_typeLambdaBody>>: constant nothing,
    _SubtermStep_typeApplicationTerm>>: constant nothing,
    _SubtermStep_injectionTerm>>: constant nothing,
    _SubtermStep_wrappedTerm>>: constant nothing]

termToSubtermGraph :: TBinding (M.Map Namespace String -> Term -> SubtermGraph)
termToSubtermGraph = define "termToSubtermGraph" $
  doc "Build a subterm graph from a term" $
  lambda "namespaces" $ lambda "term" $ lets [
    "dontCareStep">: Paths.subtermStepAnnotatedBody,
    "helper">: lambdas ["ids", "mroot", "path", "state", "stepTerm"] $ lets [
      "step">: Pairs.first $ var "stepTerm",
      "currentTerm">: Pairs.second $ var "stepTerm",
      "nodesEdges">: Pairs.first $ var "state",
      "visited">: Pairs.second $ var "state",
      "nodes">: Pairs.first $ var "nodesEdges",
      "edges">: Pairs.second $ var "nodesEdges",
      "nextPath">: Lists.cons (var "step") (var "path")]
      $ match _Term (Just $
          Lists.foldl
            (var "helper" @@ var "ids" @@ var "mroot" @@ var "nextPath")
            (var "state")
            (Rewriting.subtermsWithSteps @@ var "currentTerm")) [
        _Term_let>>: lambda "letExpr" $ lets [
          "bindings">: Core.letBindings $ var "letExpr",
          "env">: Core.letBody $ var "letExpr",
          "bindingNames">: Lists.map (unaryFunction Core.bindingName) (var "bindings"),
          -- First fold: build nodes and update ids for each binding name
          "addBindingName">: lambdas ["nodesVisitedIds", "name"] $ lets [
            "currentNodesVisited">: Pairs.first $ var "nodesVisitedIds",
            "currentIds">: Pairs.second $ var "nodesVisitedIds",
            "currentNodes">: Pairs.first $ var "currentNodesVisited",
            "currentVisited">: Pairs.second $ var "currentNodesVisited",
            "rawLabel">: Names.compactName @@ var "namespaces" @@ var "name",
            "uniqueLabel">: Names.uniqueLabel @@ var "currentVisited" @@ var "rawLabel",
            "node">: Paths.subtermNode (var "name") (var "rawLabel") (var "uniqueLabel"),
            "newVisited">: Sets.insert (var "uniqueLabel") (var "currentVisited"),
            "newNodes">: Lists.cons (var "node") (var "currentNodes"),
            "newIds">: Maps.insert (var "name") (var "node") (var "currentIds")]
            $ pair (pair (var "newNodes") (var "newVisited")) (var "newIds"),
          "nodesVisitedIds1">: Lists.foldl
            (var "addBindingName")
            (pair (pair (list ([] :: [TTerm SubtermNode])) (var "visited")) (var "ids"))
            (var "bindingNames"),
          "nodes1">: Pairs.first $ Pairs.first $ var "nodesVisitedIds1",
          "visited1">: Pairs.second $ Pairs.first $ var "nodesVisitedIds1",
          "ids1">: Pairs.second $ var "nodesVisitedIds1",
          -- Second fold: process each binding term
          "addBindingTerm">: lambdas ["currentState", "nodeBinding"] $ lets [
            "root">: Pairs.first $ var "nodeBinding",
            "binding">: Pairs.second $ var "nodeBinding",
            "term1">: Core.bindingTerm $ var "binding"]
            $ var "helper" @@ var "ids1" @@ just (var "root") @@ list ([] :: [TTerm SubtermStep]) @@ var "currentState" @@
              pair (var "dontCareStep") (var "term1"),
          "nodeBindingPairs">: Lists.zip (var "nodes1") (var "bindings"),
          "stateAfterBindings">: Lists.foldl
            (var "addBindingTerm")
            (pair (pair (Lists.concat2 (var "nodes1") (var "nodes")) (var "edges")) (var "visited1"))
            (var "nodeBindingPairs")]
          $ var "helper" @@ var "ids1" @@ var "mroot" @@ var "nextPath" @@ var "stateAfterBindings" @@
            pair Paths.subtermStepLetBody (var "env"),
        _Term_variable>>: lambda "name" $
          Maybes.maybe (var "state")
            (lambda "root" $
              Maybes.maybe (var "state")
                (lambda "node" $ lets [
                  "edge">: Paths.subtermEdge (var "root")
                    (Paths.subtermPath $ Lists.reverse $ var "nextPath") (var "node"),
                  "newEdges">: Lists.cons (var "edge") (var "edges")]
                  $ pair (pair (var "nodes") (var "newEdges")) (var "visited"))
                (Maps.lookup (var "name") (var "ids")))
            (var "mroot")]
      @@ var "currentTerm",
    "initialState">: pair (pair (list ([] :: [TTerm SubtermNode])) (list ([] :: [TTerm SubtermEdge]))) Sets.empty,
    "result">: var "helper" @@ Maps.empty @@ nothing @@ list ([] :: [TTerm SubtermStep]) @@ var "initialState" @@
      pair (var "dontCareStep") (var "term"),
    "finalNodesEdges">: Pairs.first $ var "result",
    "finalNodes">: Pairs.first $ var "finalNodesEdges",
    "finalEdges">: Pairs.second $ var "finalNodesEdges"] $
    Paths.subtermGraph (var "finalNodes") (var "finalEdges")

module Hydra.Sources.Kernel.Terms.Show.Accessors where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Names as Names
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting


ns :: Namespace
ns = Namespace "hydra.show.accessors"

module_ :: Module
module_ = Module ns elements
    [Names.ns, Rewriting.ns]
    kernelTypesNamespaces $
    Just ("Utilities for working with term accessors.")
  where
   elements = [
     toBinding termAccessor,
     toBinding termToAccessorGraph] -- TODO: move out of hydra.show.accessors

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

termAccessor :: TBinding (TermAccessor -> Maybe String)
termAccessor = define "termAccessor" $
  doc "Convert a term accessor to a string representation" $
  "accessor" ~>
  "idx" <~ ("i" ~> nothing) $  -- TODO: restore index functionality
  "idxSuff" <~ ("suffix" ~> "i" ~>
    Maybes.map ("s" ~> Strings.cat2 (var "s") (var "suffix")) (var "idx" @@ var "i")) $
  cases _TermAccessor (var "accessor")
    Nothing [
    _TermAccessor_annotatedBody>>: constant nothing,
    _TermAccessor_applicationFunction>>: constant (just (string "fun")),
    _TermAccessor_applicationArgument>>: constant (just (string "arg")),
    _TermAccessor_lambdaBody>>: constant (just (string "body")),
    _TermAccessor_unionCasesDefault>>: constant (just (string "default")),
    _TermAccessor_unionCasesBranch>>: "name" ~> just (Strings.cat2 (string ".") (Core.unName (var "name"))),
    _TermAccessor_letBody>>: constant (just (string "in")),
    _TermAccessor_letBinding>>: "name" ~> just (Strings.cat2 (Core.unName (var "name")) (string "=")),
    _TermAccessor_listElement>>: "i" ~> var "idx" @@ var "i",
    _TermAccessor_mapKey>>: "i" ~> var "idxSuff" @@ (string ".key") @@ var "i",
    _TermAccessor_mapValue>>: "i" ~> var "idxSuff" @@ (string ".value") @@ var "i",
    _TermAccessor_maybeTerm>>: constant (just (string "just")),
    _TermAccessor_productTerm>>: "i" ~> var "idx" @@ var "i",
    _TermAccessor_recordField>>: "name" ~> just (Strings.cat2 (string ".") (Core.unName (var "name"))),
    _TermAccessor_setElement>>: "i" ~> var "idx" @@ var "i",
    _TermAccessor_sumTerm>>: constant nothing,
    _TermAccessor_typeLambdaBody>>: constant nothing,
    _TermAccessor_typeApplicationTerm>>: constant nothing,
    _TermAccessor_injectionTerm>>: constant nothing,
    _TermAccessor_wrappedTerm>>: constant nothing]

-- TODO: move out of hydra.show.accessors; this is not strictly a "show" function
termToAccessorGraph :: TBinding (M.Map Namespace String -> Term -> AccessorGraph)
termToAccessorGraph = define "termToAccessorGraph" $
  doc "Build an accessor graph from a term" $
  lambda "namespaces" $ lambda "term" $ lets [
    "dontCareAccessor">: Accessors.termAccessorAnnotatedBody,
    "helper">: lambdas ["ids", "mroot", "path", "state", "accessorTerm"] $ lets [
      "accessor">: Pairs.first $ var "accessorTerm",
      "currentTerm">: Pairs.second $ var "accessorTerm",
      "nodesEdges">: Pairs.first $ var "state",
      "visited">: Pairs.second $ var "state",
      "nodes">: Pairs.first $ var "nodesEdges",
      "edges">: Pairs.second $ var "nodesEdges",
      "nextPath">: Lists.cons (var "accessor") (var "path")]
      $ match _Term (Just $
          Lists.foldl
            (var "helper" @@ var "ids" @@ var "mroot" @@ var "nextPath")
            (var "state")
            (Rewriting.subtermsWithAccessors @@ var "currentTerm")) [
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
            "node">: Accessors.accessorNode (var "name") (var "rawLabel") (var "uniqueLabel"),
            "newVisited">: Sets.insert (var "uniqueLabel") (var "currentVisited"),
            "newNodes">: Lists.cons (var "node") (var "currentNodes"),
            "newIds">: Maps.insert (var "name") (var "node") (var "currentIds")]
            $ pair (pair (var "newNodes") (var "newVisited")) (var "newIds"),
          "nodesVisitedIds1">: Lists.foldl
            (var "addBindingName")
            (pair (pair (list ([] :: [TTerm AccessorNode])) (var "visited")) (var "ids"))
            (var "bindingNames"),
          "nodes1">: Pairs.first $ Pairs.first $ var "nodesVisitedIds1",
          "visited1">: Pairs.second $ Pairs.first $ var "nodesVisitedIds1",
          "ids1">: Pairs.second $ var "nodesVisitedIds1",
          -- Second fold: process each binding term
          "addBindingTerm">: lambdas ["currentState", "nodeBinding"] $ lets [
            "root">: Pairs.first $ var "nodeBinding",
            "binding">: Pairs.second $ var "nodeBinding",
            "term1">: Core.bindingTerm $ var "binding"]
            $ var "helper" @@ var "ids1" @@ just (var "root") @@ list ([] :: [TTerm TermAccessor]) @@ var "currentState" @@
              pair (var "dontCareAccessor") (var "term1"),
          "nodeBindingPairs">: Lists.zip (var "nodes1") (var "bindings"),
          "stateAfterBindings">: Lists.foldl
            (var "addBindingTerm")
            (pair (pair (Lists.concat2 (var "nodes1") (var "nodes")) (var "edges")) (var "visited1"))
            (var "nodeBindingPairs")]
          $ var "helper" @@ var "ids1" @@ var "mroot" @@ var "nextPath" @@ var "stateAfterBindings" @@
            pair Accessors.termAccessorLetEnvironment (var "env"),
        _Term_variable>>: lambda "name" $
          Maybes.maybe (var "state")
            (lambda "root" $
              Maybes.maybe (var "state")
                (lambda "node" $ lets [
                  "edge">: Accessors.accessorEdge (var "root")
                    (Accessors.accessorPath $ Lists.reverse $ var "nextPath") (var "node"),
                  "newEdges">: Lists.cons (var "edge") (var "edges")]
                  $ pair (pair (var "nodes") (var "newEdges")) (var "visited"))
                (Maps.lookup (var "name") (var "ids")))
            (var "mroot")]
      @@ var "currentTerm",
    "initialState">: pair (pair (list ([] :: [TTerm AccessorNode])) (list ([] :: [TTerm AccessorEdge]))) Sets.empty,
    "result">: var "helper" @@ Maps.empty @@ nothing @@ list ([] :: [TTerm TermAccessor]) @@ var "initialState" @@
      pair (var "dontCareAccessor") (var "term"),
    "finalNodesEdges">: Pairs.first $ var "result",
    "finalNodes">: Pairs.first $ var "finalNodesEdges",
    "finalEdges">: Pairs.second $ var "finalNodesEdges"] $
    Accessors.accessorGraph (var "finalNodes") (var "finalEdges")

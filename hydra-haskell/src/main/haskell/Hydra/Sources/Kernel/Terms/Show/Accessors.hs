{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Show.Accessors where

-- Standard imports for term-level kernel modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Json          as Json
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Mantle        as Mantle
import qualified Hydra.Dsl.Module        as Module
import qualified Hydra.Dsl.TTerms        as TTerms
import qualified Hydra.Dsl.TTypes        as TTypes
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Names as Names
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting


module_ :: Module
module_ = Module (Namespace "hydra.show.accessors") elements
    [Names.module_, Rewriting.module_]
    kernelTypesModules $
    Just ("Utilities for working with term accessors.")
  where
   elements = [
     el termAccessorDef,
     el termToAccessorGraphDef] -- TODO: move out of hydra.show.accessors

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

termAccessorDef :: TBinding (TermAccessor -> Maybe String)
termAccessorDef = define "termAccessor" $
  doc "Convert a term accessor to a string representation" $
  lambda "accessor" $ lets [
    "idx">: lambda "i" nothing,  -- TODO: restore index functionality
    "idxSuff">: lambda "suffix" $ lambda "i" $
      Optionals.map (lambda "s" $ Strings.cat2 (var "s") (var "suffix")) (var "idx" @@ var "i")]
    $ match _TermAccessor Nothing [
      _TermAccessor_annotatedSubject>>: constant nothing,
      _TermAccessor_applicationFunction>>: constant $ just $ string "fun",
      _TermAccessor_applicationArgument>>: constant $ just $ string "arg",
      _TermAccessor_lambdaBody>>: constant $ just $ string "body",
      _TermAccessor_unionCasesDefault>>: constant $ just $ string "default",
      _TermAccessor_unionCasesBranch>>: lambda "name" $ just $
        Strings.cat2 (string ".") (Core.unName $ var "name"),
      _TermAccessor_letEnvironment>>: constant $ just $ string "in",
      _TermAccessor_letBinding>>: lambda "name" $ just $
        Strings.cat2 (Core.unName $ var "name") (string "="),
      _TermAccessor_listElement>>: lambda "i" $ var "idx" @@ var "i",
      _TermAccessor_mapKey>>: lambda "i" $ var "idxSuff" @@ string ".key" @@ var "i",
      _TermAccessor_mapValue>>: lambda "i" $ var "idxSuff" @@ string ".value" @@ var "i",
      _TermAccessor_optionalTerm>>: constant $ just $ string "just",
      _TermAccessor_productTerm>>: lambda "i" $ var "idx" @@ var "i",
      _TermAccessor_recordField>>: lambda "name" $ just $
        Strings.cat2 (string ".") (Core.unName $ var "name"),
      _TermAccessor_setElement>>: lambda "i" $ var "idx" @@ var "i",
      _TermAccessor_sumTerm>>: constant nothing,
      _TermAccessor_typeLambdaBody>>: constant nothing,
      _TermAccessor_typeApplicationTerm>>: constant nothing,
      _TermAccessor_injectionTerm>>: constant nothing,
      _TermAccessor_wrappedTerm>>: constant nothing] @@ var "accessor"

termToAccessorGraphDef :: TBinding (M.Map Namespace String -> Term -> AccessorGraph)
termToAccessorGraphDef = define "termToAccessorGraph" $
  doc "Build an accessor graph from a term" $
  lambda "namespaces" $ lambda "term" $ lets [
    "dontCareAccessor">: Mantle.termAccessorAnnotatedSubject,
    "helper">: lambdas ["ids", "mroot", "path", "state", "accessorTerm"] $ lets [
      "accessor">: first $ var "accessorTerm",
      "currentTerm">: second $ var "accessorTerm",
      "nodesEdges">: first $ var "state",
      "visited">: second $ var "state",
      "nodes">: first $ var "nodesEdges",
      "edges">: second $ var "nodesEdges",
      "nextPath">: Lists.cons (var "accessor") (var "path")]
      $ match _Term (Just $
          Lists.foldl
            (var "helper" @@ var "ids" @@ var "mroot" @@ var "nextPath")
            (var "state")
            (ref Rewriting.subtermsWithAccessorsDef @@ var "currentTerm")) [
        _Term_let>>: lambda "letExpr" $ lets [
          "bindings">: Core.letBindings $ var "letExpr",
          "env">: Core.letEnvironment $ var "letExpr",
          "bindingNames">: Lists.map (unaryFunction Core.bindingName) (var "bindings"),
          -- First fold: build nodes and update ids for each binding name
          "addBindingName">: lambdas ["nodesVisitedIds", "name"] $ lets [
            "currentNodesVisited">: first $ var "nodesVisitedIds",
            "currentIds">: second $ var "nodesVisitedIds",
            "currentNodes">: first $ var "currentNodesVisited",
            "currentVisited">: second $ var "currentNodesVisited",
            "rawLabel">: ref Names.compactNameDef @@ var "namespaces" @@ var "name",
            "uniqueLabel">: ref Names.uniqueLabelDef @@ var "currentVisited" @@ var "rawLabel",
            "node">: Accessors.accessorNode (var "name") (var "rawLabel") (var "uniqueLabel"),
            "newVisited">: Sets.insert (var "uniqueLabel") (var "currentVisited"),
            "newNodes">: Lists.cons (var "node") (var "currentNodes"),
            "newIds">: Maps.insert (var "name") (var "node") (var "currentIds")]
            $ pair (pair (var "newNodes") (var "newVisited")) (var "newIds"),
          "nodesVisitedIds1">: Lists.foldl
            (var "addBindingName")
            (pair (pair (list []) (var "visited")) (var "ids"))
            (var "bindingNames"),
          "nodes1">: first $ first $ var "nodesVisitedIds1",
          "visited1">: second $ first $ var "nodesVisitedIds1",
          "ids1">: second $ var "nodesVisitedIds1",
          -- Second fold: process each binding term
          "addBindingTerm">: lambdas ["currentState", "nodeBinding"] $ lets [
            "root">: first $ var "nodeBinding",
            "binding">: second $ var "nodeBinding",
            "term1">: Core.bindingTerm $ var "binding"]
            $ var "helper" @@ var "ids1" @@ just (var "root") @@ list [] @@ var "currentState" @@
              pair (var "dontCareAccessor") (var "term1"),
          "nodeBindingPairs">: Lists.zip (var "nodes1") (var "bindings"),
          "stateAfterBindings">: Lists.foldl
            (var "addBindingTerm")
            (pair (pair (Lists.concat2 (var "nodes1") (var "nodes")) (var "edges")) (var "visited1"))
            (var "nodeBindingPairs")]
          $ var "helper" @@ var "ids1" @@ var "mroot" @@ var "nextPath" @@ var "stateAfterBindings" @@
            pair Mantle.termAccessorLetEnvironment (var "env"),
        _Term_variable>>: lambda "name" $
          Optionals.maybe (var "state")
            (lambda "root" $
              Optionals.maybe (var "state")
                (lambda "node" $ lets [
                  "edge">: Accessors.accessorEdge (var "root")
                    (Accessors.accessorPath $ Lists.reverse $ var "nextPath") (var "node"),
                  "newEdges">: Lists.cons (var "edge") (var "edges")]
                  $ pair (pair (var "nodes") (var "newEdges")) (var "visited"))
                (Maps.lookup (var "name") (var "ids")))
            (var "mroot")]
      @@ var "currentTerm",
    "initialState">: pair (pair (list []) (list [])) Sets.empty,
    "result">: var "helper" @@ Maps.empty @@ nothing @@ list [] @@ var "initialState" @@
      pair (var "dontCareAccessor") (var "term"),
    "finalNodesEdges">: first $ var "result",
    "finalNodes">: first $ var "finalNodesEdges",
    "finalEdges">: second $ var "finalNodesEdges"]
    $ Accessors.accessorGraph (var "finalNodes") (var "finalEdges")

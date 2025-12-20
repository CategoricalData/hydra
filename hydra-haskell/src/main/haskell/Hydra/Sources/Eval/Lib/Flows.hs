
module Hydra.Sources.Eval.Lib.Flows where

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
import           Prelude hiding ((++), map)
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore


ns :: Namespace
ns = Namespace "hydra.eval.lib.flows"

define :: String -> TTerm a -> TBinding a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    [ExtractCore.ns, Monads.ns, ShowCore.ns]
    kernelTypesNamespaces $
    Just ("Evaluation-level implementations of Flow functions for the Hydra interpreter.")
  where
    elements = [
      toBinding apply_,
      toBinding bind_,
      toBinding foldl_,
      toBinding map_,
      toBinding mapElems_,
      toBinding mapKeys_,
      toBinding mapList_,
      toBinding mapMaybe_,
      toBinding mapSet_]

-- | Interpreter-friendly applicative apply for Flow.
-- apply flowFun flowArg: applies the function inside flowFun to the value inside flowArg.
apply_ :: TBinding (Term -> Term -> Flow s Term)
apply_ = define "apply" $
  doc "Interpreter-friendly applicative apply for Flow." $
  "flowFun" ~> "flowArg" ~>
  -- Build: flowFun >>= \f -> flowArg >>= \x -> pure (f x)
  produce $ Core.termApplication $ Core.application
    (Core.termApplication $ Core.application
      (Core.termFunction $ Core.functionPrimitive $ encodedName _flows_bind)
      (var "flowFun"))
    (Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "f") nothing $
      Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termFunction $ Core.functionPrimitive $ encodedName _flows_bind)
          (var "flowArg"))
        (Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "x") nothing $
          Core.termApplication $ Core.application
            (Core.termFunction $ Core.functionPrimitive $ encodedName _flows_pure)
            (Core.termApplication $ Core.application
              (Core.termVariable $ wrap _Name $ string "f")
              (Core.termVariable $ wrap _Name $ string "x"))))

-- | Interpreter-friendly monadic bind for Flow.
-- Pattern matches on the flow term to extract the wrapped function,
-- then builds a new Flow that sequences the computation.
bind_ :: TBinding (Term -> Term -> Flow s Term)
bind_ = define "bind" $
  doc "Interpreter-friendly monadic bind for Flow." $
  "flowTerm" ~> "funTerm" ~>
  -- Pattern match on flowTerm to see if it's a TermWrap (Flow)
  cases _Term (var "flowTerm")
    (Just (Monads.unexpected @@ string "flow term" @@ (ShowCore.term @@ var "flowTerm"))) [
    _Term_wrap>>: "wrappedTerm" ~>
      "innerFun" <~ Core.wrappedTermBody (var "wrappedTerm") $
      produce $ Core.termWrap $ Core.wrappedTerm
        (wrap _Name $ string "hydra.compute.Flow")
        (Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "s") nothing $
          Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "t") nothing $
            -- Build: let fs = innerFun s t
            --        in maybe (FlowState Nothing fs.state fs.trace)
            --                 (\v -> unwrap (funTerm v) fs.state fs.trace)
            --                 fs.value
            Core.termApplication $ Core.application
              (Core.termApplication $ Core.application
                (Core.termApplication $ Core.application
                  (Core.termFunction $ Core.functionPrimitive $ encodedName _maybes_maybe)
                  -- default (Nothing): return FlowState with Nothing
                  (Core.termRecord $ Core.record (wrap _Name $ string "hydra.compute.FlowState") $
                    list [
                      Core.field (wrap _Name $ string "value") (Core.termMaybe nothing),
                      Core.field (wrap _Name $ string "state") (projectFs (string "state")),
                      Core.field (wrap _Name $ string "trace") (projectFs (string "trace"))]))
                -- Just case: \v -> unwrap (funTerm v) fs.state fs.trace
                (Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "v") nothing $
                  Core.termApplication $ Core.application
                    (Core.termApplication $ Core.application
                      (Core.termFunction $ Core.functionElimination $ Core.eliminationWrap $ wrap _Name $ string "hydra.compute.Flow")
                      (Core.termApplication $ Core.application (var "funTerm") (Core.termVariable $ wrap _Name $ string "v")))
                    (Core.termApplication $ Core.application
                      (projectFs (string "state"))
                      (projectFs (string "trace")))))
              -- fs.value
              (projectFs (string "value")))]
  where
    -- Run the inner function to get FlowState: innerFun s t
    runFlow :: TTerm Term
    runFlow = Core.termApplication $ Core.application
      (Core.termApplication $ Core.application
        (var "innerFun")
        (Core.termVariable $ wrap _Name $ string "s"))
      (Core.termVariable $ wrap _Name $ string "t")
    -- Project a field from the FlowState
    projectFs :: TTerm String -> TTerm Term
    projectFs fieldName = Core.termApplication $ Core.application
      (Core.termFunction $ Core.functionElimination $ Core.eliminationRecord $ Core.projection
        (wrap _Name $ string "hydra.compute.FlowState")
        (wrap _Name fieldName))
      runFlow

-- | Interpreter-friendly foldl for Flow.
-- foldl funTerm initTerm listTerm: folds over the list with Flow effects.
foldl_ :: TBinding (Term -> Term -> Term -> Flow s Term)
foldl_ = define "foldl" $
  doc "Interpreter-friendly foldl for Flow." $
  "funTerm" ~> "initTerm" ~> "listTerm" ~>
  "elements" <<~ ExtractCore.list @@ var "listTerm" $
  -- Build: fold over elements using bind
  -- foldl f init [e1,e2,...] = bind (f init e1) (\acc1 -> bind (f acc1 e2) (\acc2 -> ...))
  produce $ Lists.foldl
    ("acc" ~> "el" ~>
      Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termFunction $ Core.functionPrimitive $ encodedName _flows_bind)
          (var "acc"))
        (Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "accVal") nothing $
          Core.termApplication $ Core.application
            (Core.termApplication $ Core.application (var "funTerm") (Core.termVariable $ wrap _Name $ string "accVal"))
            (var "el")))
    (Core.termApplication $ Core.application
      (Core.termFunction $ Core.functionPrimitive $ encodedName _flows_pure)
      (var "initTerm"))
    (var "elements")

-- | Interpreter-friendly functor map for Flow.
-- map funTerm flowTerm: applies funTerm to the value inside flowTerm.
map_ :: TBinding (Term -> Term -> Flow s Term)
map_ = define "map" $
  doc "Interpreter-friendly functor map for Flow." $
  "funTerm" ~> "flowTerm" ~>
  -- Pattern match on flowTerm to see if it's a TermWrap (Flow)
  cases _Term (var "flowTerm")
    (Just (Monads.unexpected @@ string "flow term" @@ (ShowCore.term @@ var "flowTerm"))) [
    _Term_wrap>>: "wrappedTerm" ~>
      "innerFun" <~ Core.wrappedTermBody (var "wrappedTerm") $
      produce $ Core.termWrap $ Core.wrappedTerm
        (wrap _Name $ string "hydra.compute.Flow")
        (Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "s") nothing $
          Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "t") nothing $
            -- Build: let fs = innerFun s t
            --        in FlowState (map (\v -> funTerm v) fs.value) fs.state fs.trace
            Core.termRecord $ Core.record (wrap _Name $ string "hydra.compute.FlowState") $
              list [
                Core.field (wrap _Name $ string "value")
                  (Core.termApplication $ Core.application
                    (Core.termApplication $ Core.application
                      (Core.termFunction $ Core.functionPrimitive $ encodedName _maybes_map)
                      (Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "v") nothing $
                        Core.termApplication $ Core.application (var "funTerm") (Core.termVariable $ wrap _Name $ string "v")))
                    (projectFsMap (string "value"))),
                Core.field (wrap _Name $ string "state") (projectFsMap (string "state")),
                Core.field (wrap _Name $ string "trace") (projectFsMap (string "trace"))])]
  where
    -- Run the inner function to get FlowState: innerFun s t
    runFlowMap :: TTerm Term
    runFlowMap = Core.termApplication $ Core.application
      (Core.termApplication $ Core.application
        (var "innerFun")
        (Core.termVariable $ wrap _Name $ string "s"))
      (Core.termVariable $ wrap _Name $ string "t")
    -- Project a field from the FlowState
    projectFsMap :: TTerm String -> TTerm Term
    projectFsMap fieldName = Core.termApplication $ Core.application
      (Core.termFunction $ Core.functionElimination $ Core.eliminationRecord $ Core.projection
        (wrap _Name $ string "hydra.compute.FlowState")
        (wrap _Name fieldName))
      runFlowMap

-- | Interpreter-friendly mapElems for Map with Flow.
-- mapElems funTerm mapTerm: applies funTerm to each value in the map.
mapElems_ :: TBinding (Term -> Term -> Flow s Term)
mapElems_ = define "mapElems" $
  doc "Interpreter-friendly mapElems for Map with Flow." $
  "funTerm" ~> "mapTerm" ~>
  cases _Term (var "mapTerm")
    (Just (Monads.unexpected @@ string "map value" @@ (ShowCore.term @@ var "mapTerm"))) [
    _Term_map>>: "m" ~>
      "pairs" <~ Maps.toList (var "m") $
      -- Build: sequence (map (\(k,v) -> map (\v' -> (k, v')) (funTerm v)) pairs) >>= fromList
      produce $ Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termFunction $ Core.functionPrimitive $ encodedName _flows_bind)
          (Core.termApplication $ Core.application
            (Core.termFunction $ Core.functionPrimitive $ encodedName _flows_sequence)
            (Core.termList $ Lists.map
              ("p" ~>
                "k" <~ Pairs.first (var "p") $
                "v" <~ Pairs.second (var "p") $
                Core.termApplication $ Core.application
                  (Core.termApplication $ Core.application
                    (Core.termFunction $ Core.functionPrimitive $ encodedName _flows_map)
                    (Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "v'") nothing $
                      Core.termPair $ pair (var "k") (Core.termVariable $ wrap _Name $ string "v'")))
                  (Core.termApplication $ Core.application (var "funTerm") (var "v")))
              (var "pairs"))))
        (Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "newPairs") nothing $
          Core.termApplication $ Core.application
            (Core.termFunction $ Core.functionPrimitive $ encodedName _flows_pure)
            (Core.termApplication $ Core.application
              (Core.termFunction $ Core.functionPrimitive $ encodedName _maps_fromList)
              (Core.termVariable $ wrap _Name $ string "newPairs")))]

-- | Interpreter-friendly mapKeys for Map with Flow.
-- mapKeys funTerm mapTerm: applies funTerm to each key in the map.
mapKeys_ :: TBinding (Term -> Term -> Flow s Term)
mapKeys_ = define "mapKeys" $
  doc "Interpreter-friendly mapKeys for Map with Flow." $
  "funTerm" ~> "mapTerm" ~>
  cases _Term (var "mapTerm")
    (Just (Monads.unexpected @@ string "map value" @@ (ShowCore.term @@ var "mapTerm"))) [
    _Term_map>>: "m" ~>
      "pairs" <~ Maps.toList (var "m") $
      -- Build: sequence (map (\(k,v) -> map (\k' -> (k', v)) (funTerm k)) pairs) >>= fromList
      produce $ Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termFunction $ Core.functionPrimitive $ encodedName _flows_bind)
          (Core.termApplication $ Core.application
            (Core.termFunction $ Core.functionPrimitive $ encodedName _flows_sequence)
            (Core.termList $ Lists.map
              ("p" ~>
                "k" <~ Pairs.first (var "p") $
                "v" <~ Pairs.second (var "p") $
                Core.termApplication $ Core.application
                  (Core.termApplication $ Core.application
                    (Core.termFunction $ Core.functionPrimitive $ encodedName _flows_map)
                    (Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "k'") nothing $
                      Core.termPair $ pair (Core.termVariable $ wrap _Name $ string "k'") (var "v")))
                  (Core.termApplication $ Core.application (var "funTerm") (var "k")))
              (var "pairs"))))
        (Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "newPairs") nothing $
          Core.termApplication $ Core.application
            (Core.termFunction $ Core.functionPrimitive $ encodedName _flows_pure)
            (Core.termApplication $ Core.application
              (Core.termFunction $ Core.functionPrimitive $ encodedName _maps_fromList)
              (Core.termVariable $ wrap _Name $ string "newPairs")))]

-- | Interpreter-friendly mapList for List with Flow (traverse).
-- mapList funTerm listTerm: applies funTerm to each element, collecting results.
mapList_ :: TBinding (Term -> Term -> Flow s Term)
mapList_ = define "mapList" $
  doc "Interpreter-friendly mapList for List with Flow." $
  "funTerm" ~> "listTerm" ~>
  "elements" <<~ ExtractCore.list @@ var "listTerm" $
  -- Build: sequence (map funTerm elements)
  produce $ Core.termApplication $ Core.application
    (Core.termFunction $ Core.functionPrimitive $ encodedName _flows_sequence)
    (Core.termList $ Lists.map
      ("el" ~> Core.termApplication $ Core.application (var "funTerm") (var "el"))
      (var "elements"))

-- | Interpreter-friendly mapMaybe for Maybe with Flow (traverse).
-- mapMaybe funTerm maybeTerm: applies funTerm if Just, returns Nothing otherwise.
mapMaybe_ :: TBinding (Term -> Term -> Flow s Term)
mapMaybe_ = define "mapMaybe" $
  doc "Interpreter-friendly mapMaybe for Maybe with Flow." $
  "funTerm" ~> "maybeTerm" ~>
  cases _Term (var "maybeTerm")
    (Just (Monads.unexpected @@ string "optional value" @@ (ShowCore.term @@ var "maybeTerm"))) [
    _Term_maybe>>: "m" ~>
      -- Use ifElse with term-level lambdas to delay evaluation (same pattern as Maybes)
      produce $ Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termApplication $ Core.application
            (Core.termApplication $ Core.application
              (Core.termFunction $ Core.functionPrimitive $ encodedName _logic_ifElse)
              -- isNothing m
              (Core.termApplication $ Core.application
                (Core.termFunction $ Core.functionPrimitive $ encodedName _maybes_isNothing)
                (Core.termMaybe $ var "m")))
            -- Lambda returning pure Nothing (delayed)
            (Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "_") nothing $
              Core.termApplication $ Core.application
                (Core.termFunction $ Core.functionPrimitive $ encodedName _flows_pure)
                (Core.termMaybe nothing)))
          -- Lambda returning map Just (funTerm (fromJust m)) (delayed)
          (Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "_") nothing $
            Core.termApplication $ Core.application
              (Core.termApplication $ Core.application
                (Core.termFunction $ Core.functionPrimitive $ encodedName _flows_map)
                (Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "x") nothing $
                  Core.termMaybe $ just $ Core.termVariable $ wrap _Name $ string "x"))
              (Core.termApplication $ Core.application
                (var "funTerm")
                (Core.termApplication $ Core.application
                  (Core.termFunction $ Core.functionPrimitive $ encodedName _maybes_fromJust)
                  (Core.termMaybe $ var "m")))))
        Core.termUnit]

-- | Interpreter-friendly mapSet for Set with Flow (traverse).
-- mapSet funTerm setTerm: applies funTerm to each element, collecting results.
mapSet_ :: TBinding (Term -> Term -> Flow s Term)
mapSet_ = define "mapSet" $
  doc "Interpreter-friendly mapSet for Set with Flow." $
  "funTerm" ~> "setTerm" ~>
  "elements" <<~ ExtractCore.set @@ var "setTerm" $
  -- Build: map fromList (sequence (map funTerm (toList elements)))
  produce $ Core.termApplication $ Core.application
    (Core.termApplication $ Core.application
      (Core.termFunction $ Core.functionPrimitive $ encodedName _flows_map)
      (Core.termFunction $ Core.functionPrimitive $ encodedName _sets_fromList))
    (Core.termApplication $ Core.application
      (Core.termFunction $ Core.functionPrimitive $ encodedName _flows_sequence)
      (Core.termList $ Lists.map
        ("el" ~> Core.termApplication $ Core.application (var "funTerm") (var "el"))
        (Sets.toList $ var "elements")))

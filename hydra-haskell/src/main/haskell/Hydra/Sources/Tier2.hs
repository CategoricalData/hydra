{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2 where

import Hydra.Kernel
import Hydra.Sources.Compute
import Hydra.Sources.Graph
import Hydra.Sources.Mantle
import Hydra.Sources.Strip
import Hydra.Dsl.Base as Base
import Hydra.Dsl.Lib.Equality as Equality
import Hydra.Dsl.Lib.Flows as Flows
import Hydra.Dsl.Lib.Maps as Maps
import Hydra.Dsl.Lib.Io as Io
import Hydra.Dsl.Lib.Lists as Lists
import Hydra.Dsl.Lib.Optionals as Optionals
import Hydra.Dsl.Lib.Strings as Strings
import qualified Hydra.Dsl.Annotations as Ann
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import Prelude hiding ((++))
import qualified Data.Map as M
import qualified Data.Set as S


tier2Definition :: String -> Datum a -> Definition a
tier2Definition = definitionInModule hydraTier2Module

hydraTier2Module :: Module Kv
hydraTier2Module = Module (Namespace "hydra/tier2") elements [hydraGraphModule, hydraMantleModule, hydraComputeModule, hydraStripModule] $
    Just ("A module for miscellaneous tier-2 functions and constants.")
  where
   elements = [
     el getStateDef,
     el getTermTypeDef,
     el putStateDef,
     el requireElementTypeDef,
     el requireTermTypeDef,
     el unexpectedDef
     ]

getStateDef :: Definition (Flow s s)
getStateDef = tier2Definition "getState" $
  doc "Get the state of the current flow" $
  typed flowSS $
  wrap _Flow (lambda "s0" $ lambda "t0" $ (
    (lambda "v" $ lambda "s" $ lambda "t" $ (
      (matchOpt
        (Flows.flowState nothing (var "s") (var "t"))
        (constant (Flows.flowState (just $ var "s") (var "s") (var "t"))))
       @@ var "v"))
    @@ (Flows.flowStateValue @@ var "fs1") @@ (Flows.flowStateState @@ var "fs1") @@ (Flows.flowStateTrace @@ var "fs1"))
  `with` [
    "fs1">:
      typed (Types.apply (Types.apply (TypeVariable _FlowState) sT) unitT) $
      Flows.unFlow @@ (Flows.pure @@ unit) @@ var "s0" @@ var "t0"])

getTermTypeDef :: Definition (Term a -> Flow (Graph a) (Maybe (Type a)))
getTermTypeDef = tier2Definition "getTermType" $
  function termA (flowT graphA (TypeOptional typeA)) $
  doc "Get the annotated type of a given term, if any" $
  lambda "term" ((Flows.bind @@ (Flows.map @@ (project _Graph _Graph_annotations) @@ ref getStateDef) @@ var "annsToType")
  `with` [
    "annsToType">: lambda "anns" $ (project _AnnotationClass _AnnotationClass_termType @@ var "anns" @@ var "term")])

putStateDef :: Definition (s -> Flow s ())
putStateDef = tier2Definition "putState" $
  doc "Set the state of a flow" $
  function sT (flowT sT unitT) $
  lambda "cx" $ wrap _Flow $ lambda "s0" $ lambda "t0" (
    (Flows.flowState
      (Flows.flowStateValue @@ var "f1")
      (var "cx")
      (Flows.flowStateTrace @@ var "f1"))
    `with` [
      "f1">: Flows.unFlow @@ (Flows.pure @@ unit) @@ var "s0" @@ var "t0"])

requireElementTypeDef :: Definition (Element a -> Flow (Graph a) (Type a))
requireElementTypeDef = tier2Definition "requireElementType" $
  function elementA (flowT graphA typeA) $
  doc "Get the annotated type of a given element, or fail if it is missing" $
  lambda "el" ((Flows.bind @@ (ref getTermTypeDef @@ (project _Element _Element_data @@ var "el")) @@ var "withType")
    `with` [
      "withType">: matchOpt
       (Flows.fail @@ ("missing type annotation for element " ++ (unwrap _Name @@ (project _Element _Element_name @@ var "el"))))
       Flows.pure])

requireTermTypeDef :: Definition (Term a -> Flow (Graph a) (Type a))
requireTermTypeDef = tier2Definition "requireTermType" $
  function termA (flowT graphA typeA) $
  doc "Get the annotated type of a given term, or fail if it is missing" $
  lambda "term" ((Flows.bind @@ (ref getTermTypeDef @@ var "term") @@ var "withType")
    `with` [
      "withType">: matchOpt
       (Flows.fail @@ "missing type annotation")
       Flows.pure])

unexpectedDef :: Definition (String -> String -> Flow s x)
unexpectedDef = tier2Definition "unexpected" $
  function stringT (Types.function stringT (flowT sT xT)) $
  doc "Fail if an actual value does not match an expected value" $
  lambda "expected" $ lambda "actual" $ Flows.fail @@ ("expected " ++ var "expected" ++ " but found: " ++ var "actual")

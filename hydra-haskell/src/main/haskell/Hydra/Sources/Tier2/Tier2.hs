{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Tier2 where

-- Standard Tier-2 imports
import           Prelude hiding ((++))
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y
import           Hydra.Dsl.Base            as Base
import qualified Hydra.Dsl.Core            as Core
import qualified Hydra.Dsl.Graph           as Graph
import qualified Hydra.Dsl.Lib.Equality    as Equality
import qualified Hydra.Dsl.Lib.Flows       as Flows
import qualified Hydra.Dsl.Lib.Io          as Io
import qualified Hydra.Dsl.Lib.Lists       as Lists
import qualified Hydra.Dsl.Lib.Literals    as Literals
import qualified Hydra.Dsl.Lib.Logic       as Logic
import qualified Hydra.Dsl.Lib.Maps        as Maps
import qualified Hydra.Dsl.Lib.Math        as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Sets        as Sets
import           Hydra.Dsl.Lib.Strings     as Strings
import qualified Hydra.Dsl.Module          as Module
import qualified Hydra.Dsl.Terms           as Terms
import qualified Hydra.Dsl.Types           as Types
import           Hydra.Sources.Tier1.All


tier2Definition :: String -> TTerm a -> TElement a
tier2Definition = definitionInModule hydraTier2Module

hydraTier2Module :: Module
hydraTier2Module = Module (Namespace "hydra/tier2") elements
   [hydraGraphModule, hydraMantleModule, hydraComputeModule, hydraStripModule] tier0Modules $
    Just ("A module for miscellaneous tier-2 functions and constants.")
  where
    elements = [
      el elementsToGraphDef,
      el getStateDef,
      el getTermTypeDef,
      el putStateDef,
      el requireElementTypeDef,
      el requireTermTypeDef,
      el unexpectedDef]

elementsToGraphDef :: TElement (Graph -> Maybe Graph -> [Element] -> Graph)
elementsToGraphDef = tier2Definition "elementsToGraph" $
  function graphT (funT (optionalT graphT) (funT (TypeList elementT) graphT)) $
  lambda "parent" $ lambda "schema" $ lambda "elements" $
    Graph.graph
      (Maps.fromList @@ (Lists.map @@ var "toPair" @@ var "elements"))
      (Graph.graphEnvironment @@ var "parent")
      (Graph.graphTypes @@ var "parent")
      (Graph.graphBody @@ var "parent")
      (Graph.graphPrimitives @@ var "parent")
      (var "schema")
  `with` [
    "toPair" >: lambda "el" $ pair (Graph.elementName @@ var "el") (var "el")]

getStateDef :: TElement (Flow s s)
getStateDef = tier2Definition "getState" $
  doc "Get the state of the current flow" $
  typed flowSST $
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

getTermTypeDef :: TElement (Term -> Flow Graph (Maybe Type))
getTermTypeDef = tier2Definition "getTermType" $
  doc "Get the annotated type of a given term, if any" $
  function termT (optionalT typeT) $
  match _Term (Just nothing) [
    "annotated">: ref getTermTypeDef <.> project _AnnotatedTerm _AnnotatedTerm_subject,
    "typed">: lambda "tt" $ just (project _TypedTerm _TypedTerm_type @@ var "tt")]

putStateDef :: TElement (s -> Flow s ())
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

requireElementTypeDef :: TElement (Element -> Flow Graph Type)
requireElementTypeDef = tier2Definition "requireElementType" $
  doc "Get the annotated type of a given element, or fail if it is missing" $
  function elementT (flowT graphT typeT) $
  lambda "el" $ ((var "withType" @@ (ref getTermTypeDef @@ (project _Element _Element_data @@ var "el")))
    `with` [
      "withType">: matchOpt
       (Flows.fail @@ ("missing type annotation for element " ++ (unwrap _Name @@ (project _Element _Element_name @@ var "el"))))
       Flows.pure])

requireTermTypeDef :: TElement (Term -> Flow Graph Type)
requireTermTypeDef = tier2Definition "requireTermType" $
  doc "Get the annotated type of a given term, or fail if it is missing" $
  function termT (flowT graphT typeT) $
  (var "withType" <.> ref getTermTypeDef)
    `with` [
      "withType">: matchOpt
       (Flows.fail @@ "missing type annotation")
       Flows.pure]

unexpectedDef :: TElement (String -> String -> Flow s x)
unexpectedDef = tier2Definition "unexpected" $
  doc "Fail if an actual value does not match an expected value" $
  function stringT (funT stringT (flowT sT xT)) $
  lambda "expected" $ lambda "actual" $ Flows.fail @@ ("expected " ++ var "expected" ++ " but found: " ++ var "actual")

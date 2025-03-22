{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Errors where

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

import Hydra.Sources.Tier2.Variants


hydraErrorsModule :: Module
hydraErrorsModule = Module (Namespace "hydra.errors") elements
    [hydraVariantsModule]
    [hydraComputeModule, hydraMantleModule] $
    Just "Utilities for working with errors and flow state"
  where
   elements = [
      el getStateDef,
      el putStateDef,
      el traceSummaryDef,
      el unexpectedDef]

errorsDefinition :: String -> TTerm a -> TElement a
errorsDefinition = definitionInModule hydraErrorsModule

getStateDef :: TElement (Flow s s)
getStateDef = errorsDefinition "getState" $ -- Flow s s
  doc "Get the state of the current flow" $
  wrap _Flow $ lambda "s0" $ lambda "t0" $ lets [
    "fs1">: Flows.unFlow @@ (Flows.pure unit) @@ var "s0" @@ var "t0"] $ -- FlowState s ()
    (lambda "v" $ lambda "s" $ lambda "t" $ (
        (matchOpt
          (Flows.flowState nothing (var "s") (var "t"))
          (constant (Flows.flowState (just $ var "s") (var "s") (var "t"))))
         @@ var "v"))
      @@ (Flows.flowStateValue @@ var "fs1") @@ (Flows.flowStateState @@ var "fs1") @@ (Flows.flowStateTrace @@ var "fs1")

putStateDef :: TElement (s -> Flow s ())
putStateDef = errorsDefinition "putState" $
  doc "Set the state of a flow" $
  lambda "cx" $ wrap _Flow $ lambda "s0" $ lambda "t0" $ lets [
    "f1">: Flows.unFlow @@ (Flows.pure unit) @@ var "s0" @@ var "t0"] $
    Flows.flowState
      (Flows.flowStateValue @@ var "f1")
      (var "cx")
      (Flows.flowStateTrace @@ var "f1")

traceSummaryDef :: TElement (Trace -> String)
traceSummaryDef = errorsDefinition "traceSummary" $
  doc "Summarize a trace as a string" $
  lambda "t" $ lets [
    "messageLines">: (Lists.nub (Flows.traceMessages @@ var "t")),
    "keyvalLines">: Logic.ifElse (Maps.isEmpty (Flows.traceOther @@ var "t"))
      (list [])
      (Lists.cons ("key/value pairs: ")
        (Lists.map (var "toLine") (Maps.toList (Flows.traceOther @@ var "t")))),
    "toLine">:
      lambda "pair" $ "\t" ++ (Core.unName @@ (first @@ var "pair")) ++ ": " ++ (Io.showTerm (second @@ var "pair"))] $
    Strings.intercalate "\n" (Lists.concat2 (var "messageLines") (var "keyvalLines"))

unexpectedDef :: TElement (String -> String -> Flow s x)
unexpectedDef = errorsDefinition "unexpected" $
  doc "Fail if an actual value does not match an expected value" $
  lambda "expected" $ lambda "actual" $ Flows.fail ("expected " ++ var "expected" ++ " but found: " ++ var "actual")

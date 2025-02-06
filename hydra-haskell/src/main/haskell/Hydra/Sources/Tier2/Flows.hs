{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Flows where

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


flowsDefinition :: String -> TTerm a -> TElement a
flowsDefinition = definitionInModule hydraFlowsModule

hydraFlowsModule :: Module
hydraFlowsModule = Module (Namespace "hydra/flows") elements [] tier0Modules $
    Just ("Functions for working with flows (the Hydra state monad).")
  where
    elements = [
      el emptyTraceDef,
      el flowSucceedsDef,
      el fromFlowDef,
      el mutateTraceDef,
      el pushErrorDef,
      el warnDef,
      el withFlagDef,
      el withStateDef,
      el withTraceDef]

emptyTraceDef :: TElement Trace
emptyTraceDef = flowsDefinition "emptyTrace" $
  Flows.trace (list []) (list []) Maps.empty

flowSucceedsDef :: TElement (Flow s a -> Bool)
flowSucceedsDef = flowsDefinition "flowSucceeds" $
  doc "Check whether a flow succeeds" $
  function (Types.var "s") (Types.function flowSAT Types.boolean) $
  lambda "cx" $ lambda "f" $
    Optionals.isJust @@ (Flows.flowStateValue @@ (Flows.unFlow @@ var "f" @@ var "cx" @@ ref emptyTraceDef))

fromFlowDef :: TElement (a -> s -> Flow s a -> a)
fromFlowDef = flowsDefinition "fromFlow" $
  doc "Get the value of a flow, or a default value if the flow fails" $
  function (Types.var "a") (Types.function (Types.var "s") (Types.function flowSAT (Types.var "a"))) $
  lambda "def" $ lambda "cx" $ lambda "f" $
      matchOpt (var "def") (lambda "x" $ var "x")
        @@ (Flows.flowStateValue @@ (Flows.unFlow @@ var "f" @@ var "cx" @@ ref emptyTraceDef))

mutateTraceDef :: TElement ((Trace -> Either_ String Trace) -> (Trace -> Trace -> Trace) -> Flow s a -> Flow s a)
mutateTraceDef = flowsDefinition "mutateTrace" $
    functionN [
      Types.function traceT (eitherT Types.string traceT),
      Types.functionN [traceT, traceT, traceT],
      flowSAT,
      flowSAT] $
    lambda "mutate" $ lambda "restore" $ lambda "f" $ wrap _Flow (
      lambda "s0" $ lambda "t0" (
        ((match _Either Nothing [
            _Either_left>>: var "forLeft",
            _Either_right>>: var "forRight"])
          @@ (var "mutate" @@ var "t0"))
        `with` [
          "forLeft">:
            lambda "msg" $ Flows.flowState nothing (var "s0") (ref pushErrorDef @@ var "msg" @@ var "t0"),
          -- retain the updated state, but reset the trace after execution
          "forRight">:
            function traceT (flowStateT (Types.var "s") (Types.var "s")) $
            lambda "t1" ((Flows.flowState
                (Flows.flowStateValue @@ var "f2")
                (Flows.flowStateState @@ var "f2")
                (var "restore" @@ var "t0" @@ (Flows.flowStateTrace @@ var "f2")))
              `with` [
                 -- execute the internal flow after augmenting the trace
                 "f2">: Flows.unFlow @@ var "f" @@ var "s0" @@ var "t1"
              ])]))
  where
    eitherT l r = Types.applyN [TypeVariable _Either, l, r]

pushErrorDef :: TElement (String -> Trace -> Trace)
pushErrorDef = flowsDefinition "pushError" $
  doc "Push an error message" $
  functionN [Types.string, traceT, traceT] $
  lambda "msg" $ lambda "t" $ ((Flows.trace
      (Flows.traceStack @@ var "t")
      (Lists.cons @@ var "errorMsg" @@ (Flows.traceMessages @@ var "t"))
      (Flows.traceOther @@ var "t"))
    `with` [
      "errorMsg">: Strings.concat ["Error: ", var "msg", " (", (Strings.intercalate @@ " > " @@ (Lists.reverse @@ (Flows.traceStack @@ var "t"))), ")"]])

warnDef :: TElement (String -> Flow s a -> Flow s a)
warnDef = flowsDefinition "warn" $
  doc "Continue the current flow after adding a warning message" $
  functionN [Types.string, flowSAT, flowSAT] $
  lambda "msg" $ lambda "b" $ wrap _Flow $ lambda "s0" $ lambda "t0" (
    (Flows.flowState
      (Flows.flowStateValue @@ var "f1")
      (Flows.flowStateState @@ var "f1")
      (var "addMessage" @@ (Flows.flowStateTrace @@ var "f1")))
    `with` [
      "f1">: Flows.unFlow @@ var "b" @@ var "s0" @@ var "t0",
      "addMessage">: lambda "t" $ Flows.trace
        (Flows.traceStack @@ var "t")
        (Lists.cons @@ ("Warning: " ++ var "msg") @@ (Flows.traceMessages @@ var "t"))
        (Flows.traceOther @@ var "t")])

withFlagDef :: TElement (String -> Flow s a -> Flow s a)
withFlagDef = flowsDefinition "withFlag" $
  doc "Continue the current flow after setting a flag" $
  function nameT (Types.function flowSAT flowSAT) $
  lambda "flag" ((ref mutateTraceDef @@ var "mutate" @@ var "restore")
  `with` [
    "mutate">: lambda "t" $ inject _Either _Either_right $ (Flows.trace
      (Flows.traceStack @@ var "t")
      (Flows.traceMessages @@ var "t")
      (Maps.insert @@ var "flag" @@ (inject _Term _Term_literal $ inject _Literal _Literal_boolean $ boolean True) @@ (Flows.traceOther @@ var "t"))),
    "restore">: lambda "ignored" $ lambda "t1" $ Flows.trace
      (Flows.traceStack @@ var "t1")
      (Flows.traceMessages @@ var "t1")
      (Maps.remove @@ var "flag" @@ (Flows.traceOther @@ var "t1"))])

withStateDef :: TElement (s1 -> Flow s1 a -> Flow s2 a)
withStateDef = flowsDefinition "withState" $
  doc "Continue a flow using a given state" $
  function (Types.var "s1") (Types.function flowS1AT flowS2AT) $
  lambda "cx0" $ lambda "f" $
    wrap _Flow $ lambda "cx1" $ lambda "t1" (
      (Flows.flowState (Flows.flowStateValue @@ var "f1") (var "cx1") (Flows.flowStateTrace @@ var "f1"))
      `with` [
        "f1">:
          typed (Types.apply (Types.apply (TypeVariable _FlowState) (Types.var "s1")) (Types.var "a")) $
          Flows.unFlow @@ var "f" @@ var "cx0" @@ var "t1"])

withTraceDef :: TElement (String -> Flow s a -> Flow s a)
withTraceDef = flowsDefinition "withTrace" $
  doc "Continue the current flow after augmenting the trace" $
  functionN [Types.string, flowSAT, flowSAT] $
  lambda "msg" ((ref mutateTraceDef @@ var "mutate" @@ var "restore")
    `with` [
      -- augment the trace
      "mutate">: lambda "t" $ Logic.ifElse
        @@ (inject _Either _Either_left $ string "maximum trace depth exceeded. This may indicate an infinite loop")
        @@ (inject _Either _Either_right $ Flows.trace
          (Lists.cons @@ var "msg" @@ (Flows.traceStack @@ var "t"))
          (Flows.traceMessages @@ var "t")
          (Flows.traceOther @@ var "t"))
        @@ (Equality.gteInt32 @@ (Lists.length @@ (Flows.traceStack @@ var "t")) @@ ref maxTraceDepthDef),
      -- reset the trace stack after execution
      "restore">: lambda "t0" $ lambda "t1" $ Flows.trace
        (Flows.traceStack @@ var "t0")
        (Flows.traceMessages @@ var "t1")
        (Flows.traceOther @@ var "t1")])

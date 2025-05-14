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
import           Hydra.Sources.Tier1.All hiding (mapDef)

import Hydra.Sources.Libraries


flowsDefinition :: String -> TTerm a -> TElement a
flowsDefinition = definitionInModule hydraFlowsModule

hydraFlowsModule :: Module
hydraFlowsModule = Module (Namespace "hydra.flows") elements
    [hydraConstantsModule] [hydraMantleModule, hydraComputeModule] $
    Just ("Functions for working with flows (the Hydra state monad).")
  where
    elements = [
      el bindDef,
      el bind2Def,
      el bind3Def,
      el bind4Def,
      el emptyTraceDef,
      el failDef,
      el flowSucceedsDef,
      el fromFlowDef,
      el mapDef,
      el map2Def,
      el mutateTraceDef,
      el pureDef,
      el pushErrorDef,
      el warnDef,
      el withFlagDef,
      el withStateDef,
      el withTraceDef]

bindDef :: TElement (Flow s a -> (a -> Flow s b) -> Flow s b)
bindDef = flowsDefinition "bind" $
  lambdas ["l", "r"] $ lets [
    "q">: lambdas ["s0", "t0"] $ lets [
      "fs1">: Flows.unFlow @@ var "l" @@ var "s0" @@ var "t0"]
      $ (primitive _optionals_maybe
          @@ (Flows.flowState nothing (Flows.flowStateState @@ var "fs1") (Flows.flowStateTrace @@ var "fs1"))
          @@ (lambda "v" $ Flows.unFlow @@ (var "r" @@ var "v") @@ (Flows.flowStateState @@ var "fs1") @@ (Flows.flowStateTrace @@ var "fs1")))
        @@ (Flows.flowStateValue @@ var "fs1")]
    $ wrap _Flow $ var "q"

bind2Def :: TElement ((Flow s a) -> (Flow s b) -> (a -> b -> Flow s c) -> Flow s c)
bind2Def = flowsDefinition "bind2" $
  doc "Bind the results of two flows into another flow" $
  lambdas ["f1", "f2", "f"] $ ref bindDef @@ var "f1" @@
    (lambda "r1" $ ref bindDef @@ var "f2" @@
      (lambda "r2" $ var "f" @@ var "r1" @@ var "r2"))

bind3Def :: TElement ((Flow s a) -> (Flow s b) -> (Flow s c) -> (a -> b -> c -> Flow s d) -> Flow s d)
bind3Def = flowsDefinition "bind3" $
  doc "Bind the results of three flows into another flow" $
  lambdas ["f1", "f2", "f3", "f"] $ ref bindDef @@ var "f1" @@
    (lambda "r1" $ ref bindDef @@ var "f2" @@
      (lambda "r2" $ ref bindDef @@ var "f3" @@
        (lambda "r3" $ var "f" @@ var "r1" @@ var "r2" @@ var "r3")))

bind4Def :: TElement ((Flow s a) -> (Flow s b) -> (Flow s c) -> (a -> b -> c -> Flow s d) -> Flow s d)
bind4Def = flowsDefinition "bind4" $
  doc "Bind the results of four flows into another flow" $
  lambdas ["f1", "f2", "f3", "f4", "f"] $ ref bindDef @@ var "f1" @@
    (lambda "r1" $ ref bindDef @@ var "f2" @@
      (lambda "r2" $ ref bindDef @@ var "f3" @@
        (lambda "r3" $ ref bindDef @@ var "f4" @@
          (lambda "r4" $ var "f" @@ var "r1" @@ var "r2" @@ var "r3" @@ var "r4"))))

emptyTraceDef :: TElement Trace
emptyTraceDef = flowsDefinition "emptyTrace" $
  Flows.trace (list []) (list []) Maps.empty

failDef :: TElement (String -> Flow s a)
failDef = flowsDefinition "failInternal" $
  lambda "msg" $ wrap _Flow $ lambdas ["s", "t"] $
    Flows.flowState nothing (var "s") (ref pushErrorDef @@ var "msg" @@ var "t")

flowSucceedsDef :: TElement (Flow s a -> Bool)
flowSucceedsDef = flowsDefinition "flowSucceeds" $
  doc "Check whether a flow succeeds" $
  lambda "cx" $ lambda "f" $
    Optionals.isJust (Flows.flowStateValue @@ (Flows.unFlow @@ var "f" @@ var "cx" @@ ref emptyTraceDef))

fromFlowDef :: TElement (a -> s -> Flow s a -> a)
fromFlowDef = flowsDefinition "fromFlow" $
  doc "Get the value of a flow, or a default value if the flow fails" $
  lambda "def" $ lambda "cx" $ lambda "f" $ Optionals.maybe
    (var "def")
    (lambda "xmo" $ var "xmo")
    (Flows.flowStateValue @@ (Flows.unFlow @@ var "f" @@ var "cx" @@ ref emptyTraceDef))

mapDef :: TElement ((a -> b) -> Flow s a -> Flow s b)
mapDef = flowsDefinition "map" $
  doc "Map a function over a flow" $
  lambdas ["f", "f1"] $ wrap _Flow $ lambdas ["s0", "t0"] $ lets [
    "f2">: Flows.unFlow @@ var "f1" @@ var "s0" @@ var "t0"]
    $ Flows.flowState
      (Optionals.map (var "f") $ Flows.flowStateValue @@ var "f2")
      (Flows.flowStateState @@ var "f2")
      (Flows.flowStateTrace @@ var "f2")

map2Def :: TElement ((Flow s a) -> (Flow s b) -> (a -> b -> c) -> Flow s c)
map2Def = flowsDefinition "map2" $
  doc "Map a function over two flows" $
  lambdas ["f1", "f2", "f"] $ ref bindDef
    @@ var "f1"
    @@ (lambda "r1" $ ref mapDef
      @@ (lambda "r2" $ var "f" @@ var "r1" @@ var "r2")
      @@ var "f2")

mutateTraceDef :: TElement ((Trace -> Either_ String Trace) -> (Trace -> Trace -> Trace) -> Flow s a -> Flow s a)
mutateTraceDef = flowsDefinition "mutateTrace" $
    lambda "mutate" $ lambda "restore" $ lambda "f"
      $ wrap _Flow $ lambda "s0" $ lambda "t0" $ lets [
        "forLeft">:
          lambda "msg" $ Flows.flowState nothing (var "s0") (ref pushErrorDef @@ var "msg" @@ var "t0"),
        -- retain the updated state, but reset the trace after execution
        "forRight">:
          lambda "t1" $ lets [
            -- execute the internal flow after augmenting the trace
            "f2">: Flows.unFlow @@ var "f" @@ var "s0" @@ var "t1"]
            $ Flows.flowState
                (Flows.flowStateValue @@ var "f2")
                (Flows.flowStateState @@ var "f2")
                (var "restore" @@ var "t0" @@ (Flows.flowStateTrace @@ var "f2"))]
        $ (match _Either Nothing [
            _Either_left>>: var "forLeft",
            _Either_right>>: var "forRight"])
          @@ (var "mutate" @@ var "t0")
  where
    eitherT l r = Types.applyMany [TypeVariable _Either, l, r]

pureDef :: TElement (a -> Flow s a)
pureDef = flowsDefinition "pureInternal" $
  lambda "xp" $ wrap _Flow $ lambdas ["s", "t"] $ Flows.flowState (just $ var "xp") (var "s") (var "t")

pushErrorDef :: TElement (String -> Trace -> Trace)
pushErrorDef = flowsDefinition "pushError" $
  doc "Push an error message" $
  lambda "msg" $ lambda "t" $ lets [
    "errorMsg">: Strings.concat ["Error: ", var "msg", " (", (Strings.intercalate " > " (Lists.reverse (Flows.traceStack @@ var "t"))), ")"]]
    $ Flows.trace
        (Flows.traceStack @@ var "t")
        (Lists.cons (var "errorMsg") (Flows.traceMessages @@ var "t"))
        (Flows.traceOther @@ var "t")

warnDef :: TElement (String -> Flow s a -> Flow s a)
warnDef = flowsDefinition "warn" $
  doc "Continue the current flow after adding a warning message" $
  lambda "msg" $ lambda "b" $ wrap _Flow $ lambda "s0" $ lambda "t0" $ lets [
    "f1">: Flows.unFlow @@ var "b" @@ var "s0" @@ var "t0",
    "addMessage">: lambda "t" $ Flows.trace
      (Flows.traceStack @@ var "t")
      (Lists.cons ("Warning: " ++ var "msg") (Flows.traceMessages @@ var "t"))
      (Flows.traceOther @@ var "t")]
    $ Flows.flowState
        (Flows.flowStateValue @@ var "f1")
        (Flows.flowStateState @@ var "f1")
        (var "addMessage" @@ (Flows.flowStateTrace @@ var "f1"))

withFlagDef :: TElement (String -> Flow s a -> Flow s a)
withFlagDef = flowsDefinition "withFlag" $
  doc "Continue the current flow after setting a flag" $
  lambda "flag" $ lets [
    "mutate">: lambda "t" $ inject _Either _Either_right $ (Flows.trace
      (Flows.traceStack @@ var "t")
      (Flows.traceMessages @@ var "t")
      (Maps.insert (var "flag") (inject _Term _Term_literal $ inject _Literal _Literal_boolean $ boolean True) (Flows.traceOther @@ var "t"))),
    "restore">: lambda "ignored" $ lambda "t1" $ Flows.trace
      (Flows.traceStack @@ var "t1")
      (Flows.traceMessages @@ var "t1")
      (Maps.remove (var "flag") (Flows.traceOther @@ var "t1"))]
    $ ref mutateTraceDef @@ var "mutate" @@ var "restore"

withStateDef :: TElement (s1 -> Flow s1 a -> Flow s2 a)
withStateDef = flowsDefinition "withState" $
  doc "Continue a flow using a given state" $
  lambda "cx0" $ lambda "f" $
    wrap _Flow $ lambda "cx1" $ lambda "t1" $ lets [
      "f1">:
        Flows.unFlow @@ var "f" @@ var "cx0" @@ var "t1"]
      $ Flows.flowState
          (Flows.flowStateValue @@ var "f1")
          (var "cx1")
          (Flows.flowStateTrace @@ var "f1")

withTraceDef :: TElement (String -> Flow s a -> Flow s a)
withTraceDef = flowsDefinition "withTrace" $
  doc "Continue the current flow after augmenting the trace" $
  lambda "msg" $ lets [
    -- augment the trace
    "mutate">: lambda "t" $ Logic.ifElse (Equality.gteInt32 (Lists.length (Flows.traceStack @@ var "t")) $ ref maxTraceDepthDef)
      (inject _Either _Either_left $ string "maximum trace depth exceeded. This may indicate an infinite loop")
      (inject _Either _Either_right $ Flows.trace
        (Lists.cons (var "msg") (Flows.traceStack @@ var "t"))
        (Flows.traceMessages @@ var "t")
        (Flows.traceOther @@ var "t")),
    -- reset the trace stack after execution
    "restore">: lambda "t0" $ lambda "t1" $ Flows.trace
      (Flows.traceStack @@ var "t0")
      (Flows.traceMessages @@ var "t1")
      (Flows.traceOther @@ var "t1")]
    $ ref mutateTraceDef @@ var "mutate" @@ var "restore"

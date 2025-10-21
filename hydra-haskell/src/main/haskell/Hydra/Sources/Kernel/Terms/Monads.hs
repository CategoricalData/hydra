{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Monads where

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

import qualified Hydra.Sources.Kernel.Terms.Constants as Constants
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Variants as Variants

import Hydra.Mantle


module_ :: Module
module_ = Module (Namespace "hydra.monads") elements
    [Constants.module_, ShowCore.module_]
    kernelTypesModules $
    Just ("Functions for working with Hydra's 'flow' and other monads.")
  where
    elements = [
      el bindDef,
      el emptyTraceDef,
      el execDef,
      el failDef,
      el flowSucceedsDef,
      el fromFlowDef,
      el getStateDef,
      el mapDef,
      el modifyDef,
      el mutateTraceDef,
      el optionalToListDef,
      el pureDef,
      el pushErrorDef,
      el putStateDef,
      el traceSummaryDef,
      el unexpectedDef,
      el warnDef,
      el withFlagDef,
      el withStateDef,
      el withTraceDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

bindDef :: TBinding (Flow s a -> (a -> Flow s b) -> Flow s b)
bindDef = define "bind" $
  "l" ~> "r" ~>
  "q" <~ ("s0" ~> "t0" ~>
    "fs1" <~ Compute.unFlow (var "l") (var "s0") (var "t0") $
    Optionals.maybe
      (Compute.flowState
        nothing
        (Compute.flowStateState $ var "fs1")
        (Compute.flowStateTrace $ var "fs1"))
      ("v" ~> Compute.unFlow
        (var "r" @@ var "v")
        (Compute.flowStateState $ var "fs1")
        (Compute.flowStateTrace $ var "fs1"))
      (Compute.flowStateValue $ var "fs1")) $
  Compute.flow $ var "q"

emptyTraceDef :: TBinding Trace
emptyTraceDef = define "emptyTrace" $ Compute.trace (list []) (list []) Maps.empty

execDef :: TBinding (Flow s a -> s -> s)
execDef = define "exec" $
  "f" ~> "s0" ~>
  Compute.flowStateState (Compute.unFlow (var "f") (var "s0") (ref emptyTraceDef))

failDef :: TBinding (String -> Flow s a)
failDef = define "fail" $
  "msg" ~>
  Compute.flow (
    "s" ~> "t" ~>
    Compute.flowState nothing (var "s") (ref pushErrorDef @@ var "msg" @@ var "t"))

flowSucceedsDef :: TBinding (Flow s a -> Bool)
flowSucceedsDef = define "flowSucceeds" $
  doc "Check whether a flow succeeds" $
  "s" ~> "f" ~>
  Optionals.isJust (Compute.flowStateValue (Compute.unFlow (var "f") (var "s") (ref emptyTraceDef)))

fromFlowDef :: TBinding (a -> s -> Flow s a -> a)
fromFlowDef = define "fromFlow" $
  doc "Get the value of a flow, or a default value if the flow fails" $
  "def" ~> "cx" ~> "f" ~> Optionals.maybe
    (var "def")
    ("xmo" ~> var "xmo")
    (Compute.flowStateValue (Compute.unFlow (var "f") (var "cx") (ref emptyTraceDef)))

getStateDef :: TBinding (Flow s s)
getStateDef = define "getState" $
  doc "Get the state of the current flow" $
  Compute.flow (
    "s0" ~> "t0" ~>
    "fs1" <~ Compute.unFlow (ref pureDef @@ unit) (var "s0") (var "t0") $
    "v" <~ Compute.flowStateValue (var "fs1") $
    "s" <~ Compute.flowStateState (var "fs1") $
    "t" <~ Compute.flowStateTrace (var "fs1") $
    Optionals.maybe
      (Compute.flowState nothing (var "s") (var "t"))
      (constant (Compute.flowState (just (var "s")) (var "s") (var "t")))
      (var "v"))

mapDef :: TBinding ((a -> b) -> Flow s a -> Flow s b)
mapDef = define "map" $
  doc "Map a function over a flow" $
  "f" ~> "f1" ~>
  Compute.flow (
    "s0" ~> "t0" ~>
    "f2" <~ Compute.unFlow (var "f1") (var "s0") (var "t0") $
    Compute.flowState
      (Optionals.map (var "f") (Compute.flowStateValue (var "f2")))
      (Compute.flowStateState (var "f2"))
      (Compute.flowStateTrace (var "f2")))

modifyDef :: TBinding ((s -> s) -> Flow s ())
modifyDef = define "modify" $ lambda "f" $
  ref bindDef
    @@ (ref getStateDef)
    @@ (lambda "s" $ ref putStateDef @@ (var "f" @@ var "s"))

mutateTraceDef :: TBinding ((Trace -> Hydra.Mantle.Either String Trace) -> (Trace -> Trace -> Trace) -> Flow s a -> Flow s a)
mutateTraceDef = define "mutateTrace" $
  "mutate" ~> "restore" ~> "f" ~>
  "choose" <~ ("forLeft" ~> "forRight" ~> "e" ~> cases _Either (var "e") Nothing [
    _Either_left>>: "e" ~> var "forLeft" @@ var "e",
    _Either_right>>: "e" ~> var "forRight" @@ var "e"]) $
  Compute.flow (
    "s0" ~> "t0" ~>
    "forLeft" <~ ("msg" ~>
      Compute.flowState nothing (var "s0") (ref pushErrorDef @@ var "msg" @@ var "t0")) $
    "forRight" <~ ("t1" ~>
      "f2" <~ Compute.unFlow (var "f") (var "s0") (var "t1") $
      Compute.flowState
        (Compute.flowStateValue (var "f2"))
        (Compute.flowStateState (var "f2"))
        (var "restore" @@ var "t0" @@ (Compute.flowStateTrace (var "f2")))) $
    var "choose" @@ var "forLeft" @@ var "forRight" @@ (var "mutate" @@ var "t0"))

optionalToListDef :: TBinding (Maybe a -> [a])
optionalToListDef = define "optionalToList" $
  doc "Converts an optional value either to an empty list (if nothing) or a singleton list (if just)." $
  "mx" ~> Optionals.maybe (list []) (unaryFunction Lists.pure) (var "mx")

pureDef :: TBinding (a -> Flow s a)
pureDef = define "pure" $
  "xp" ~>
  Compute.flow (
    "s" ~> "t" ~> Compute.flowState (just (var "xp")) (var "s") (var "t"))

pushErrorDef :: TBinding (String -> Trace -> Trace)
pushErrorDef = define "pushError" $
  doc "Push an error message" $
  "msg" ~> "t" ~>
  "condenseRepeats" <~ (
    "ys" ~>
    "condenseGroup" <~ ("xs" ~>
      "x" <~ Lists.head (var "xs") $
      "n" <~ Lists.length (var "xs") $
      Logic.ifElse (Equality.equal (var "n") (int32 1))
        (var "x")
        (Strings.cat (list [var "x", " (x", Literals.showInt32 (var "n"), ")"]))) $
    Lists.map (var "condenseGroup") (Lists.group (var "ys" :: TTerm [String]))) $
  "errorMsg" <~ Strings.concat [
    "Error: ", var "msg", " (",
    (Strings.intercalate " > " (var "condenseRepeats" @@ (Lists.reverse (Compute.traceStack (var "t"))))),
    ")"] $
  Compute.trace
    (Compute.traceStack (var "t"))
    (Lists.cons (var "errorMsg") (Compute.traceMessages (var "t")))
    (Compute.traceOther (var "t"))

putStateDef :: TBinding (s -> Flow s ())
putStateDef = define "putState" $
  doc "Set the state of a flow" $
  "cx" ~> Compute.flow (
    "s0" ~> "t0" ~>
    "f1" <~ Compute.unFlow (ref pureDef @@ unit) (var "s0") (var "t0") $
    Compute.flowState
      (Compute.flowStateValue (var "f1"))
      (var "cx")
      (Compute.flowStateTrace (var "f1")))

traceSummaryDef :: TBinding (Trace -> String)
traceSummaryDef = define "traceSummary" $
  doc "Summarize a trace as a string" $
  "t" ~>
  "messageLines" <~ (Lists.nub (Compute.traceMessages $ var "t")) $
  "toLine" <~ ("pair" ~>
    "\t" ++ (Core.unName $ (first $ var "pair")) ++ ": " ++ (ref ShowCore.termDef @@ (second $ var "pair"))) $
  "keyvalLines" <~ Logic.ifElse (Maps.null (Compute.traceOther $ var "t"))
    (list [])
    (Lists.cons ("key/value pairs: ")
      (Lists.map (var "toLine") (Maps.toList (Compute.traceOther $ var "t")))) $
  Strings.intercalate "\n" (Lists.concat2 (var "messageLines") (var "keyvalLines"))

unexpectedDef :: TBinding (String -> String -> Flow s x)
unexpectedDef = define "unexpected" $
  doc "Fail if an actual value does not match an expected value" $
  "expected" ~> "actual" ~> ref failDef @@ ("expected " ++ var "expected" ++ " but found: " ++ var "actual")

warnDef :: TBinding (String -> Flow s a -> Flow s a)
warnDef = define "warn" $
  doc "Continue the current flow after adding a warning message" $
  "msg" ~> "b" ~> Compute.flow (
    "s0" ~> "t0" ~>
    "f1" <~ Compute.unFlow (var "b") (var "s0") (var "t0") $
    "addMessage" <~ ("t" ~> Compute.trace
      (Compute.traceStack (var "t"))
      (Lists.cons ("Warning: " ++ var "msg") (Compute.traceMessages (var "t")))
      (Compute.traceOther (var "t"))) $
    Compute.flowState
      (Compute.flowStateValue (var "f1"))
      (Compute.flowStateState (var "f1"))
      (var "addMessage" @@ (Compute.flowStateTrace (var "f1"))))

--withFlagDef :: TBinding (Name -> Flow s a -> Flow s a)
--withFlagDef = define "withFlag" $
--  doc "Continue the current flow after setting a flag" $
--  "flag" ~> "f" ~>
--  "mutate" <~ ("t" ~> Mantle.eitherRight (Compute.trace
--    (Compute.traceStack (var "t"))
--    (Compute.traceMessages (var "t"))
--    (Maps.insert (var "flag") (Core.termLiteral (Core.literalBoolean (boolean True))) (Compute.traceOther (var "t"))))) $
--  "restore" <~ ("ignored" ~> "t1" ~> Compute.trace
--    (Compute.traceStack (var "t1"))
--    (Compute.traceMessages (var "t1"))
--    (Maps.remove (var "flag") (Compute.traceOther (var "t1")))) $
--  ref mutateTraceDef @@ var "mutate" @@ var "restore" @@ var "f"

withFlagDef = define "withFlag" $
  doc "Continue the current flow after setting a flag" $
  "flag" ~> "f" ~>
  "mutate" <~ ("t" ~> Logic.ifElse
    (boolean False)
    (Mantle.eitherLeft (string "never happens"))  -- Forces the left type to String
    (Mantle.eitherRight (Compute.trace
      (Compute.traceStack (var "t"))
      (Compute.traceMessages (var "t"))
      (Maps.insert (var "flag") (Core.termLiteral (Core.literalBoolean (boolean True))) (Compute.traceOther (var "t")))))) $
  "restore" <~ ("ignored" ~> "t1" ~> Compute.trace
    (Compute.traceStack (var "t1"))
    (Compute.traceMessages (var "t1"))
    (Maps.remove (var "flag") (Compute.traceOther (var "t1")))) $
  ref mutateTraceDef @@ var "mutate" @@ var "restore" @@ var "f"

withStateDef :: TBinding (s1 -> Flow s1 a -> Flow s2 a)
withStateDef = define "withState" $
  doc "Continue a flow using a given state" $
  "cx0" ~> "f" ~>
  Compute.flow (
    "cx1" ~> "t1" ~>
    "f1" <~ Compute.unFlow (var "f") (var "cx0") (var "t1") $
    Compute.flowState
      (Compute.flowStateValue (var "f1"))
      (var "cx1")
      (Compute.flowStateTrace (var "f1")))

withTraceDef :: TBinding (String -> Flow s a -> Flow s a)
withTraceDef = define "withTrace" $
  doc "Continue the current flow after augmenting the trace" $
  "msg" ~> "f" ~>
  "mutate" <~ ("t" ~> Logic.ifElse (Equality.gte (Lists.length (Compute.traceStack (var "t"))) (ref Constants.maxTraceDepthDef))
    (Mantle.eitherLeft (string "maximum trace depth exceeded. This may indicate an infinite loop"))
    (Mantle.eitherRight (Compute.trace
      (Lists.cons (var "msg") (Compute.traceStack (var "t")))
      (Compute.traceMessages (var "t"))
      (Compute.traceOther (var "t"))))) $
  "restore" <~ ("t0" ~> "t1" ~> Compute.trace
    (Compute.traceStack (var "t0"))
    (Compute.traceMessages (var "t1"))
    (Compute.traceOther (var "t1"))) $
  ref mutateTraceDef @@ var "mutate" @@ var "restore" @@ var "f"

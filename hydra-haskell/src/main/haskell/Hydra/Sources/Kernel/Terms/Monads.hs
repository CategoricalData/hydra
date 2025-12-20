
module Hydra.Sources.Kernel.Terms.Monads where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  emptyTrace, flowSucceeds, fromFlow, getState, map, maybeToList, modify, mutateTrace,
  pushError, putState, traceSummary, unexpected, warn, withFlag, withState, withTrace)
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
import           Hydra.Dsl.Meta.Phantoms as Phantoms hiding (bind, exec, map)
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
import           Prelude hiding ((++), fail, map, pure)
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Constants as Constants
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore


ns :: Namespace
ns = Namespace "hydra.monads"

module_ :: Module
module_ = Module ns elements
    [Constants.ns, ShowCore.ns]
    kernelTypesNamespaces $
    Just ("Functions for working with Hydra's 'flow' and other monads.")
  where
    elements = [
      toBinding bind,
      toBinding emptyTrace,
      toBinding exec,
      toBinding fail,
      toBinding flowSucceeds,
      toBinding fromFlow,
      toBinding getState,
      toBinding map,
      toBinding modify,
      toBinding mutateTrace,
      toBinding maybeToList,
      toBinding pure,
      toBinding pushError,
      toBinding putState,
      toBinding traceSummary,
      toBinding unexpected,
      toBinding warn,
      toBinding withFlag,
      toBinding withState,
      toBinding withTrace]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

bind :: TBinding (Flow s a -> (a -> Flow s b) -> Flow s b)
bind = define "bind" $
  doc "Monadic bind for flows" $
  "l" ~> "r" ~>
  "q" <~ ("s0" ~> "t0" ~>
    "fs1" <~ Compute.unFlow (var "l") (var "s0") (var "t0") $
    Maybes.maybe
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

emptyTrace :: TBinding Trace
emptyTrace = define "emptyTrace" $
  doc "An empty trace with no stack, messages, or other attributes" $
  Compute.trace (list ([] :: [TTerm String])) (list ([] :: [TTerm String])) Maps.empty

exec :: TBinding (Flow s a -> s -> s)
exec = define "exec" $
  doc "Execute a flow and return the final state" $
  "f" ~> "s0" ~>
  Compute.flowStateState (Compute.unFlow (var "f") (var "s0") emptyTrace)

fail :: TBinding (String -> Flow s a)
fail = define "fail" $
  doc "Fail a flow with an error message" $
  "msg" ~>
  Compute.flow (
    "s" ~> "t" ~>
    Compute.flowState nothing (var "s") (pushError @@ var "msg" @@ var "t"))

flowSucceeds :: TBinding (Flow s a -> Bool)
flowSucceeds = define "flowSucceeds" $
  doc "Check whether a flow succeeds" $
  "s" ~> "f" ~>
  Maybes.isJust (Compute.flowStateValue (Compute.unFlow (var "f") (var "s") emptyTrace))

fromFlow :: TBinding (a -> s -> Flow s a -> a)
fromFlow = define "fromFlow" $
  doc "Get the value of a flow, or a default value if the flow fails" $
  "def" ~> "cx" ~> "f" ~> Maybes.maybe
    (var "def")
    ("xmo" ~> var "xmo")
    (Compute.flowStateValue (Compute.unFlow (var "f") (var "cx") emptyTrace))

getState :: TBinding (Flow s s)
getState = define "getState" $
  doc "Get the state of the current flow" $
  Compute.flow (
    "s0" ~> "t0" ~>
    "fs1" <~ Compute.unFlow (pure @@ unit) (var "s0") (var "t0") $
    "v" <~ Compute.flowStateValue (var "fs1") $
    "s" <~ Compute.flowStateState (var "fs1") $
    "t" <~ Compute.flowStateTrace (var "fs1") $
    Maybes.maybe
      (Compute.flowState nothing (var "s") (var "t"))
      (constant (Compute.flowState (just (var "s")) (var "s") (var "t")))
      (var "v"))

map :: TBinding ((a -> b) -> Flow s a -> Flow s b)
map = define "map" $
  doc "Map a function over a flow" $
  "f" ~> "f1" ~>
  Compute.flow (
    "s0" ~> "t0" ~>
    "f2" <~ Compute.unFlow (var "f1") (var "s0") (var "t0") $
    Compute.flowState
      (Maybes.map (var "f") (Compute.flowStateValue (var "f2")))
      (Compute.flowStateState (var "f2"))
      (Compute.flowStateTrace (var "f2")))

modify :: TBinding ((s -> s) -> Flow s ())
modify = define "modify" $
  doc "Modify the state of a flow using a given function" $
  lambda "f" $
  bind
    @@ (getState)
    @@ (lambda "s" $ putState @@ (var "f" @@ var "s"))

mutateTrace :: TBinding ((Trace -> Prelude.Either String Trace) -> (Trace -> Trace -> Trace) -> Flow s a -> Flow s a)
mutateTrace = define "mutateTrace" $
  doc "Temporarily mutate the trace for the duration of a flow" $
  "mutate" ~> "restore" ~> "f" ~>
  "choose" <~ ("forLeft" ~> "forRight" ~> "e" ~> Eithers.either_
    ("l" ~> var "forLeft" @@ var "l")
    ("r" ~> var "forRight" @@ var "r")
    (var "e")) $
  "flowFun" <~ ("s0" ~> "t0" ~>
    "forLeft" <~ ("msg" ~>
      Compute.flowState nothing (var "s0") (pushError @@ var "msg" @@ var "t0")) $
    "forRight" <~ ("t1" ~>
      "f2" <~ Compute.unFlow (var "f") (var "s0") (var "t1") $
      Compute.flowState
        (Compute.flowStateValue (var "f2"))
        (Compute.flowStateState (var "f2"))
        (var "restore" @@ var "t0" @@ (Compute.flowStateTrace (var "f2")))) $
    var "choose" @@ var "forLeft" @@ var "forRight" @@ (var "mutate" @@ var "t0")) $
  Compute.flow $ var "flowFun"

maybeToList :: TBinding (Maybe a -> [a])
maybeToList = define "maybeToList" $
  doc "Converts an optional value either to an empty list (if nothing) or a singleton list (if just)." $
  "mx" ~> Maybes.maybe (list ([] :: [TTerm a])) (unaryFunction Lists.pure) (var "mx")

pure :: TBinding (a -> Flow s a)
pure = define "pure" $
  doc "Lift a value into a flow" $
  "xp" ~>
  Compute.flow (
    "s" ~> "t" ~> Compute.flowState (just (var "xp")) (var "s") (var "t"))

pushError :: TBinding (String -> Trace -> Trace)
pushError = define "pushError" $
  doc "Push an error message" $
  "msg" ~> "t" ~>
  "condenseRepeats" <~ (
    "ys" ~>
    "condenseGroup" <~ ("xs" ~>
      "x" <~ Lists.head (var "xs") $
      "n" <~ Lists.length (var "xs") $
      Logic.ifElse (Equality.equal (var "n") (int32 1))
        (var "x")
        (Strings.cat (list [var "x", string " (x", Literals.showInt32 (var "n"), string ")"]))) $
    Lists.map (var "condenseGroup") (Lists.group (var "ys" :: TTerm [String]))) $
  "errorMsg" <~ Strings.concat [
    string "Error: ", var "msg", string " (",
    (Strings.intercalate (string " > ") (var "condenseRepeats" @@ (Lists.reverse (Compute.traceStack (var "t"))))),
    string ")"] $
  Compute.trace
    (Compute.traceStack (var "t"))
    (Lists.cons (var "errorMsg") (Compute.traceMessages (var "t")))
    (Compute.traceOther (var "t"))

putState :: TBinding (s -> Flow s ())
putState = define "putState" $
  doc "Set the state of a flow" $
  "cx" ~> Compute.flow (
    "s0" ~> "t0" ~>
    "f1" <~ Compute.unFlow (pure @@ unit) (var "s0") (var "t0") $
    Compute.flowState
      (Compute.flowStateValue (var "f1"))
      (var "cx")
      (Compute.flowStateTrace (var "f1")))

traceSummary :: TBinding (Trace -> String)
traceSummary = define "traceSummary" $
  doc "Summarize a trace as a string" $
  "t" ~>
  "messageLines" <~ (Lists.nub (Compute.traceMessages $ var "t")) $
  "toLine" <~ ("pair" ~>
    string "\t" ++ (Core.unName $ (Pairs.first $ var "pair")) ++ string ": " ++ (ShowCore.term @@ (Pairs.second $ var "pair"))) $
  "keyvalLines" <~ Logic.ifElse (Maps.null (Compute.traceOther $ var "t"))
    (list ([] :: [TTerm String]))
    (Lists.cons (string "key/value pairs: ")
      (Lists.map (var "toLine") (Maps.toList (Compute.traceOther $ var "t")))) $
  Strings.intercalate (string "\n") (Lists.concat2 (var "messageLines") (var "keyvalLines"))

unexpected :: TBinding (String -> String -> Flow s x)
unexpected = define "unexpected" $
  doc "Fail if an actual value does not match an expected value" $
  "expected" ~> "actual" ~> fail @@ (string "expected " ++ var "expected" ++ string " but found: " ++ var "actual")

warn :: TBinding (String -> Flow s a -> Flow s a)
warn = define "warn" $
  doc "Continue the current flow after adding a warning message" $
  "msg" ~> "b" ~> Compute.flow (
    "s0" ~> "t0" ~>
    "f1" <~ Compute.unFlow (var "b") (var "s0") (var "t0") $
    "addMessage" <~ ("t" ~> Compute.trace
      (Compute.traceStack (var "t"))
      (Lists.cons (string "Warning: " ++ var "msg") (Compute.traceMessages (var "t")))
      (Compute.traceOther (var "t"))) $
    Compute.flowState
      (Compute.flowStateValue (var "f1"))
      (Compute.flowStateState (var "f1"))
      (var "addMessage" @@ (Compute.flowStateTrace (var "f1"))))

withFlag = define "withFlag" $
  doc "Continue the current flow after setting a flag" $
  "flag" ~> "f" ~>
  "mutate" <~ ("t" ~> Logic.ifElse
    (boolean False)
    (left (string "never happens"))  -- Forces the left type to String
    (right (Compute.trace
      (Compute.traceStack (var "t"))
      (Compute.traceMessages (var "t"))
      (Maps.insert (var "flag") (Core.termLiteral (Core.literalBoolean (boolean True))) (Compute.traceOther (var "t")))))) $
  "restore" <~ ("ignored" ~> "t1" ~> Compute.trace
    (Compute.traceStack (var "t1"))
    (Compute.traceMessages (var "t1"))
    (Maps.delete (var "flag") (Compute.traceOther (var "t1")))) $
  mutateTrace @@ var "mutate" @@ var "restore" @@ var "f"

withState :: TBinding (s1 -> Flow s1 a -> Flow s2 a)
withState = define "withState" $
  doc "Continue a flow using a given state" $
  "cx0" ~> "f" ~>
  Compute.flow (
    "cx1" ~> "t1" ~>
    "f1" <~ Compute.unFlow (var "f") (var "cx0") (var "t1") $
    Compute.flowState
      (Compute.flowStateValue (var "f1"))
      (var "cx1")
      (Compute.flowStateTrace (var "f1")))

withTrace :: TBinding (String -> Flow s a -> Flow s a)
withTrace = define "withTrace" $
  doc "Continue the current flow after augmenting the trace" $
  "msg" ~> "f" ~>
  "mutate" <~ ("t" ~> Logic.ifElse (Equality.gte (Lists.length (Compute.traceStack (var "t"))) Constants.maxTraceDepth)
    (left (string "maximum trace depth exceeded. This may indicate an infinite loop"))
    (right (Compute.trace
      (Lists.cons (var "msg") (Compute.traceStack (var "t")))
      (Compute.traceMessages (var "t"))
      (Compute.traceOther (var "t"))))) $
  "restore" <~ ("t0" ~> "t1" ~> Compute.trace
    (Compute.traceStack (var "t0"))
    (Compute.traceMessages (var "t1"))
    (Compute.traceOther (var "t1"))) $
  mutateTrace @@ var "mutate" @@ var "restore" @@ var "f"

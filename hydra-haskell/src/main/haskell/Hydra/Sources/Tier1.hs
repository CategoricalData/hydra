{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier1 where

import Hydra.Kernel
import Hydra.Sources.Compute
import Hydra.Sources.Graph
import Hydra.Sources.Mantle
import Hydra.Sources.Strip
import Hydra.Dsl.Base as Base
import Hydra.Dsl.Lib.Equality as Equality
import Hydra.Dsl.Lib.Flows as Flows
import Hydra.Dsl.Lib.Maps as Maps
import Hydra.Dsl.Lib.Lists as Lists
import Hydra.Dsl.Lib.Optionals as Optionals
import Hydra.Dsl.Lib.Strings as Strings
import qualified Hydra.Dsl.Annotations as Ann
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import Prelude hiding ((++))
import qualified Data.Map as M
import qualified Data.Set as S


tier1Definition :: String -> Datum a -> Definition a
tier1Definition = definitionInModule hydraTier1Module

hydraTier1Module :: Module Kv
hydraTier1Module = Module (Namespace "hydra/tier1") elements [hydraGraphModule, hydraMantleModule, hydraComputeModule, hydraStripModule] $
    Just ("A module for miscellaneous tier-1 functions and constants.")
  where
   elements = [
     el unqualifyNameDef,
     -- Flows.hs
     el emptyTraceDef,
     el flowSucceedsDef,
     el fromFlowDef,
     el pushErrorDef
--     el unexpectedDef
     ]

unqualifyNameDef :: Definition (QualifiedName -> Name)
unqualifyNameDef = tier1Definition "unqualifyName" $
  doc "Convert a qualified name to a dot-separated name" $
  lambda "qname" $ (wrap _Name $ var "prefix" ++ (project _QualifiedName _QualifiedName_local @@ var "qname"))
    `with` [
      "prefix">: matchOpt (string "") (lambda "n" $ (unwrap _Namespace @@ var "n") ++ string ".")
        @@ (project _QualifiedName _QualifiedName_namespace @@ var "qname")]

-- Flows.hs

emptyTraceDef :: Definition Trace
emptyTraceDef = tier1Definition "emptyTrace" $
  record _Trace [
    _Trace_stack>>: list [],
    _Trace_messages>>: list [],
    _Trace_other>>: Maps.empty]

flowSucceedsDef :: Definition (Flow s a -> Bool)
flowSucceedsDef = tier1Definition "flowSucceeds" $
  doc "Check whether a flow succeeds" $
  function (Types.var "s") (Types.function flowSA Types.boolean) $
  lambda "cx" $ lambda "f" $
    Optionals.isJust @@ (Flows.flowStateValue @@ (Flows.unFlow @@ var "f" @@ var "cx" @@ ref emptyTraceDef))

fromFlowDef :: Definition (a -> s -> Flow s a -> a)
fromFlowDef = tier1Definition "fromFlow" $
  doc "Get the value of a flow, or a default value if the flow fails" $
  function (Types.var "a") (Types.function (Types.var "s") (Types.function flowSA (Types.var "a"))) $
  lambda "def" $ lambda "cx" $ lambda "f" $
      matchOpt (var "def") (lambda "x" $ var "x")
        @@ (Flows.flowStateValue @@ (Flows.unFlow @@ var "f" @@ var "cx" @@ ref emptyTraceDef))










--mutateTrace :: (Trace -> Either String Trace) -> (Trace -> Trace -> Trace) -> Flow s a -> Flow s a
--mutateTrace mutate restore f = Flow q
--  where
--    q s0 t0 = either forLeft forRight $ mutate t0
--      where
--        forLeft msg = FlowState Nothing s0 $ pushError msg t0
--        forRight t1 = FlowState v s1 $ restore t0 t2 -- retain the updated state, but reset the trace after execution
--          where
--            FlowState v s1 t2 = unFlow f s0 t1 -- execute the internal flow after augmenting the trace
--
--pushError :: String -> Trace -> Trace
--pushError msg t = t {traceMessages = errorMsg:(traceMessages t)}
--  where
--    errorMsg = "Error: " ++ msg ++ " (" ++ L.intercalate " > " (L.reverse $ traceStack t) ++ ")"

pushErrorDef :: Definition (String -> Trace -> Trace)
pushErrorDef = tier1Definition "pushError" $
  doc "Push an error message" $
  lambda "msg" $ lambda "t" $ ((Flows.trace
      (Flows.traceStack @@ var "t")
      (Lists.cons @@ var "errorMsg" @@ (Flows.traceMessages @@ var "t"))
      (Flows.traceOther @@ var "t"))
    `with` [
      "errorMsg">: Strings.concat ["Error: ", var "msg", " (", (Strings.intercalate @@ " > " @@ (Lists.reverse @@ (Flows.traceStack @@ var "t"))), ")"]])


--putState :: s -> Flow s ()
--putState cx = Flow q
--  where
--    q s0 t0 = FlowState v cx t1
--      where
--        FlowState v _ t1 = unFlow f s0 t0
--        f = pure ()
--
--traceSummary :: Trace -> String
--traceSummary t = L.intercalate "\n" (messageLines ++ keyvalLines)
--  where
--    messageLines = L.nub $ traceMessages t
--    keyvalLines = if M.null (traceOther t)
--        then []
--        else "key/value pairs:":(toLine <$> M.toList (traceOther t))
--      where
--        toLine (k, v) = "\t" ++ k ++ ": " ++ show v
--
--unexpected :: Show x => String -> x -> Flow s y
--unexpected cat obj = fail $ "expected " ++ cat ++ " but found: " ++ show obj

--unexpectedDef :: Definition (String -> x -> Flow s y)
--unexpectedDef = tier1Definition "unexpected" $
--  doc "Fail with a message indicating an unexpected value" $
--  function Types.string (Types.function (Types.var "x") flowSY) $
--  lambda "cat" $ lambda "obj" $
--    Flows.fail @@ (Strings.concat ["expected ", var "cat", " but found: ", (show @@ var "obj"))

--warn :: String -> Flow s a -> Flow s a
--warn msg b = Flow u'
--  where
--    u' s0 t0 = FlowState v s1 t2
--      where
--        FlowState v s1 t1 = unFlow b s0 t0
--        t2 = t1 {traceMessages = ("Warning: " ++ msg):(traceMessages t1)}
--
--withFlag :: String -> Flow s a -> Flow s a
--withFlag flag = mutateTrace mutate restore
--  where
--    mutate t = Right $ t {traceOther = M.insert flag (TermLiteral $ LiteralBoolean True) (traceOther t)}
--    restore _ t1 = t1 {traceOther = M.delete flag (traceOther t1)}
--
--withState :: s1 -> Flow s1 a -> Flow s2 a
--withState cx0 f = Flow q
--  where
--    q cx1 t1 = FlowState v cx1 t2
--      where
--        FlowState v _ t2 = unFlow f cx0 t1
--
--withTrace :: String -> Flow s a -> Flow s a
--withTrace msg = mutateTrace mutate restore
--  where
--    mutate t = if L.length (traceStack t) >= maxTraceDepth
--      then Left "maximum trace depth exceeded. This may indicate an infinite loop"
--      else Right $ t {traceStack = msg:(traceStack t)} -- augment the trace
--    restore t0 t1 = t1 {traceStack = traceStack t0} -- reset the trace stack after execution



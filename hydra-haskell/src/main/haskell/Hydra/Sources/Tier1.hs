{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier1 where

import Hydra.Kernel
import Hydra.Sources.Compute
import Hydra.Sources.Graph
import Hydra.Sources.Mantle
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
hydraTier1Module = Module (Namespace "hydra/tier1") elements [hydraGraphModule, hydraMantleModule, hydraComputeModule] $
    Just ("A module for all tier-1 functions and constants. "
      <> "These are generated functions and constants which DSL functions and the implementations of primitive functions are allowed to depend upon. "
      <> "Higher tiers of generated code may not be depended upon, as these tiers may themselves need to depend on DSL functions or primitive functions.")
  where
   elements = [
     el skipAnnotationsDef,
     el stripTermDef,
     el stripTypeDef,
     el unqualifyNameDef,
     -- Flows.hs
     el emptyTraceDef,
     el flowSucceedsDef,
     el fromFlowDef,
--     el getStateDef,
     el pushErrorDef
     ]

eqA = (M.fromList [(Name "a", S.fromList [TypeClassEquality])])
elementA = Types.apply (TypeVariable _Element) (Types.var "a") :: Type a
fieldA = Types.apply (TypeVariable _Field) (Types.var "a") :: Type a
fieldTypeA = Types.apply (TypeVariable _FieldType) (Types.var "a") :: Type a
flowGraphATypeA = Types.apply (Types.apply (TypeVariable _Flow) graphA) typeA :: Type a
flowSA = Types.apply (Types.apply (TypeVariable _Flow) (Types.var "s")) (Types.var "a") :: Type a
flowSS = Types.apply (Types.apply (TypeVariable _Flow) (Types.var "s")) (Types.var "s") :: Type a
flowSY = Types.apply (Types.apply (TypeVariable _Flow) (Types.var "s")) (Types.var "y") :: Type a
flowStateSS = Types.apply (Types.apply (TypeVariable _FlowState) (Types.var "s")) (Types.var "s") :: Type a
graphA = Types.apply (TypeVariable _Graph) (Types.var "a") :: Type a
termA = Types.apply (TypeVariable _Term) (Types.var "a") :: Type a
typeA = Types.apply (TypeVariable _Type) (Types.var "a") :: Type a

skipAnnotationsDef :: Definition ((a -> Maybe (Annotated a m)) -> a -> a)
skipAnnotationsDef = tier1Definition "skipAnnotations" $
  function getAnnType (Types.function (Types.var "x") (Types.var "x")) $
  lambda "getAnn" $ lambda "t" $
    (var "skip" @@ var "t") `with` [
      "skip">:
        function (Types.var "x") (Types.var "x") $
        lambda "t1" $
          (matchOpt
            (var "t1")
            (lambda "ann" $ var "skip" @@ (project _Annotated _Annotated_subject @@ var "ann")))
          @@ (var "getAnn" @@ var "t1")]
  where
    getAnnType = (Types.function
      (Types.var "x")
      (Types.optional $ Types.apply (Types.apply (TypeVariable _Annotated) (Types.var "x")) (Types.var "a")))

stripTermDef :: Definition (Term a -> Term a)
stripTermDef = tier1Definition "stripTerm" $
    doc "Strip all annotations from a term" $
    function termA termA $
      lambda "x" (ref skipAnnotationsDef @@ (match _Term (Just nothing) [
        Case _Term_annotated --> lambda "ann" (just $ var "ann")]) @@ var "x")

stripTypeDef :: Definition (Type a -> Type a)
stripTypeDef = tier1Definition "stripType" $
    doc "Strip all annotations from a type" $
    function typeA typeA $
      lambda "x" (ref skipAnnotationsDef @@ (match _Type (Just nothing) [
        Case _Type_annotated --> lambda "ann" (just $ var "ann")]) @@ var "x")
  where
    typeA = Types.apply (TypeVariable _Type) (Types.var "a")

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


--getState :: Flow s s
--getState = Flow q
--  where
--    q s0 t0 = case flowStateValue fs1 of
--        Nothing -> FlowState Nothing s1 t1
--        Just _ -> FlowState (Just s1) s1 t1
--      where
--        fs1 = unFlow (pure ()) s0 t0
--        s1 = flowStateState fs1
--        t1 = flowStateTrace fs1

--getStateDef :: Definition (Flow s s)
--getStateDef = tier1Definition "getState" $
--  doc "Get the state of the current flow" $
--  typed flowSS $
--  ((wrap _Flow $ var "q")
--  `with` [
--    "q">: function (Types.var "s") (Types.function (Types.var "x") flowStateSS) $
--      lambda "s0" $ lambda "t0" $ (
--        (matchOpt (Flows.flowState nothing (var "s1") (var "t1")) (constant (Flows.flowState (just $ var "s1") (var "s1") (var "t1")))
--          @@ (Flows.flowStateValue @@ var "fs1"))
--        `with` [
--          "fs1">:
--            typed (Types.apply (Types.apply (TypeVariable _FlowState) Types.unit) (Types.var "x")) $
--            Flows.unFlow @@ (Flows.pure @@ unit) @@ var "s0" @@ var "t0",
--          "s1">:
--            typed (Types.var "s") $
--            Flows.flowStateState @@ var "fs1",
--          "t1">:
--            typed (TypeVariable _Trace) $
--            Flows.flowStateTrace @@ var "fs1"
--        ])])

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
pushErrorDef = tier1Definition "pushErrorTmp" $
  doc "Push an error message" $
  lambda "msg" $ lambda "t" $ ((Flows.trace
      (Flows.traceStack @@ var "t")
      (Lists.cons @@ var "errorMsg" @@ (Flows.traceMessages @@ var "t"))
      (Flows.traceOther @@ var "t"))
    `with` [
--      "errorMsg">: "foo"])
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
--    Flows.fail @@ (Strings.concat ["expected ", var "cat", " but found: " ++ (show @@ var "obj"))

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



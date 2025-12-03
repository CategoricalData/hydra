-- | Test cases for hydra.monads functions
module Hydra.Sources.Test.Monads where

import Hydra.Kernel
import Hydra.Testing
import Hydra.Dsl.Meta.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Compute as Compute
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import Hydra.Dsl.Meta.Terms as MetaTerms
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads
import qualified Hydra.Sources.Kernel.Terms.Constants as Constants

import qualified Data.Map as M


module_ :: Module
module_ = Module (Namespace "hydra.test.monads") elements [Monads.module_] [] $
    Just "Test cases for hydra.monads functions"
  where
    elements = [el allTestsDef]

testTrace :: TTerm Term
testTrace = metaref Monads.emptyTraceDef

-- | Test cases for pure: lifts a value into a successful flow
pureTests :: TTerm TestGroup
pureTests = subgroup "pure" [
  test "integer" (int32 42),
  test "string" (string "hello")]
  where
    test testName val = evalCaseWithTags testName []
      (unFlowTerm @@ (metaref Monads.pureDef @@ val) @@ unit @@ testTrace)
      (flowStateTerm (optional $ just val) unit testTrace)

-- | Test cases for map: transforms the value inside a flow
mapTests :: TTerm TestGroup
mapTests = subgroup "map" [
  test "map negate" (primitive _math_negate) (int32 5) (int32 (-5)),
  test "map absolute" (primitive _math_abs) (int32 (-3)) (int32 3)]
  where
    test testName fn inVal outVal = evalCaseWithTags testName [tag_requiresInterp]
      (unFlowTerm @@ (metaref Monads.mapDef @@ fn @@ (metaref Monads.pureDef @@ inVal)) @@ unit @@ testTrace)
      (flowStateTerm (optional $ just outVal) unit testTrace)

-- | Test cases for bind: chains flow computations together
bindTests :: TTerm TestGroup
bindTests = subgroup "bind" [
  test "bind add" (primitive _math_add) (int32 10) (int32 5) (int32 15),
  test "bind multiply" (primitive _math_mul) (int32 3) (int32 4) (int32 12)]
  where
    test testName op x y result = evalCaseWithTags testName [tag_requiresInterp]
      (unFlowTerm @@ (metaref Monads.bindDef @@ (metaref Monads.pureDef @@ x) @@ (lambda "n" (metaref Monads.pureDef @@ (op @@ var "n" @@ y)))) @@ unit @@ testTrace)
      (flowStateTerm (optional $ just result) unit testTrace)

-- | Build a trace term at the meta level with custom stack, messages, and other
traceTerm :: TTerm Term -> TTerm Term -> TTerm Term -> TTerm Term
traceTerm stack messages other = recordLift _Trace [
  _Trace_stack >>: stack,
  _Trace_messages >>: messages,
  _Trace_other >>: other]

-- | Build an empty trace with custom messages
traceWithMessages :: [String] -> TTerm Term
traceWithMessages msgs = traceTerm
  (list [])
  (list $ fmap string msgs)
  (MetaTerms.map (Phantoms.map M.empty))

-- | Test cases for error trace ordering
-- Tests that withTrace annotations are properly recorded and errors include the trace context
errorTraceTests :: TTerm TestGroup
errorTraceTests = subgroup "error traces" [
  evalCaseWithTags "Error traces are in the right order" [tag_requiresInterp]
    -- Input: withTrace "one" $ withTrace "two" $ fail "oops"
    (unFlowTerm
      @@ (metaref Monads.withTraceDef @@ string "one"
          @@ (metaref Monads.withTraceDef @@ string "two"
              @@ (metaref Monads.failDef @@ string "oops")))
      @@ unit
      @@ testTrace)
    -- Output: FlowState Nothing () (Trace [] ["Error: oops (one > two)"] {})
    (flowStateTerm (optional nothing) unit (traceWithMessages ["Error: oops (one > two)"]))]

allTestsDef :: TBinding TestGroup
allTestsDef = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.monads functions" $
    supergroup "monads" [
      pureTests,
      mapTests,
      bindTests,
      errorTraceTests]

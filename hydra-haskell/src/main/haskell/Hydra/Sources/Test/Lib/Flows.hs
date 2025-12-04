-- | Test cases for hydra.lib.flows primitives
module Hydra.Sources.Test.Lib.Flows where

import Hydra.Kernel
import Hydra.Testing
import Hydra.Dsl.Meta.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Compute as Compute
import qualified Hydra.Dsl.Meta.Core as Core
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import Hydra.Dsl.Meta.Terms as MetaTerms
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads
import qualified Data.Map as M


module_ :: Module
module_ = Module (Namespace "hydra.test.lib.flows") elements [Monads.module_] [] $
    Just "Test cases for hydra.lib.flows primitives"
  where
    elements = [el allTestsDef]

testTrace :: TTerm Term
testTrace = metaref Monads.emptyTraceDef

-- Test groups for hydra.lib.flows primitives

-- | Test cases for flows.bind: chains flow computations together
flowsBind :: TTerm TestGroup
flowsBind = subgroup "bind" [
  test "bind add" (primitive _math_add) (int32 5) (int32 5) (int32 10),
  test "bind multiply" (primitive _math_mul) (int32 3) (int32 4) (int32 12)]
  where
    test testName op x y result = evalCaseWithTags testName [tag_requiresInterp]
      (unFlowTerm @@ (metaref Monads.bindDef
        @@ (metaref Monads.pureDef @@ x)
        @@ (lambda "n" (metaref Monads.pureDef @@ (op @@ var "n" @@ y)))) @@ unit @@ testTrace)
      (flowStateTerm (optional $ just result) unit testTrace)

-- | Test cases for flows.fail: creates a failing flow
flowsFail :: TTerm TestGroup
flowsFail = subgroup "fail" [
  test "fail with message"]
  where
    test testName = evalCaseWithTags testName []
      (unFlowTerm @@ (metaref Monads.failDef @@ MetaTerms.string "test error message") @@ unit @@ testTrace)
      (flowStateTerm (optional nothing) unit (traceWithMessages ["Error: test error message ()"]))

-- | Build an empty trace with custom messages
traceWithMessages :: [String] -> TTerm Term
traceWithMessages msgs = traceTerm
  (list [])
  (list $ fmap string msgs)
  (MetaTerms.map (Phantoms.map M.empty))

-- | Test cases for flows.map: transforms the value inside a flow
flowsMap :: TTerm TestGroup
flowsMap = subgroup "map" [
  test "map negate" (primitive _math_negate) (int32 5) (int32 (-5)),
  test "map abs" (primitive _math_abs) (int32 (-3)) (int32 3)]
  where
    test testName fn inVal outVal = evalCaseWithTags testName [tag_requiresInterp]
      (unFlowTerm @@ (metaref Monads.mapDef @@ fn @@ (metaref Monads.pureDef @@ inVal)) @@ unit @@ testTrace)
      (flowStateTerm (optional $ just outVal) unit testTrace)

-- | Test cases for flows.pure: lifts a value into a successful flow
flowsPure :: TTerm TestGroup
flowsPure = subgroup "pure" [
  test "pure integer" (int32 42),
  test "pure zero" (int32 0),
  test "pure negative" (int32 (-5)),
  test "pure string" (string "hello")]
  where
    test testName val = evalCaseWithTags testName []
      (unFlowTerm @@ (metaref Monads.pureDef @@ val) @@ unit @@ testTrace)
      (flowStateTerm (optional $ just val) unit testTrace)

allTestsDef :: TBinding TestGroup
allTestsDef = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.flows primitives" $
    supergroup "hydra.lib.flows primitives" [
      flowsBind,
      flowsFail,
      flowsMap,
      flowsPure]

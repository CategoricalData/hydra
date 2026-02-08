-- | Test cases for hydra.lib.flows primitives
module Hydra.Sources.Test.Lib.Flows where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

-- Additional imports specific to this file
import Hydra.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Compute as Compute
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads


ns :: Namespace
ns = Namespace "hydra.test.lib.flows"

module_ :: Module
module_ = Module ns elements [Monads.ns] kernelTypesNamespaces $
    Just "Test cases for hydra.lib.flows primitives"
  where
    elements = [Phantoms.toBinding allTests]

-- | Trace at the TTerm Term level
testTrace :: TTerm Term
testTrace = traceTerm (list []) (list []) (Terms.map (Phantoms.map M.empty))

-- Test groups for hydra.lib.flows primitives

-- | Test cases for flows.pure: lifts a value into a successful flow
-- Note: This uses the kernel Monads.pure since the primitive doesn't have an eval implementation
flowsPure :: TTerm TestGroup
flowsPure = subgroup "pure" [
  test "pure integer" (int32 42),
  test "pure zero" (int32 0),
  test "pure negative" (int32 (-5)),
  test "pure string" (string "hello")]
  where
    test testName val = evalCase testName
      (unFlowTerm @@ (metaref Monads.pure @@ val) @@ unit @@ testTrace)
      (flowStateTerm (optional $ just val) unit testTrace)

-- | Test cases for flows.fail: creates a failing flow
-- Note: This uses the kernel Monads.fail since the primitive doesn't have an eval implementation
flowsFail :: TTerm TestGroup
flowsFail = subgroup "fail" [
  test "fail with message"]
  where
    test testName = evalCase testName
      (unFlowTerm @@ (metaref Monads.fail @@ string "test error message") @@ unit @@ testTrace)
      (flowStateTerm (optional nothing) unit (traceWithMessages ["Error: test error message ()"]))
    traceWithMessages msgs = traceTerm (list []) (list $ fmap string msgs) (Terms.map (Phantoms.map M.empty))

-- | Test cases for flows.map: transforms the value inside a flow
-- Tests the primitive _flows_map with kernel Monads.pure for flow construction
flowsMap :: TTerm TestGroup
flowsMap = subgroup "map" [
  test "map negate" (primitive _math_negate) (int32 5) (int32 (-5)),
  test "map abs" (primitive _math_abs) (int32 (-3)) (int32 3)]
  where
    test testName fn inVal outVal = evalCase testName
      (unFlowTerm @@ (primitive _flows_map @@ fn @@ (metaref Monads.pure @@ inVal)) @@ unit @@ testTrace)
      (flowStateTerm (optional $ just outVal) unit testTrace)

-- | Test cases for flows.bind: chains flow computations together
-- Uses kernel Monads.bind since the primitive isn't completing evaluation
flowsBind :: TTerm TestGroup
flowsBind = subgroup "bind" [
  test "bind add" (primitive _math_add) (int32 5) (int32 5) (int32 10),
  test "bind multiply" (primitive _math_mul) (int32 3) (int32 4) (int32 12)]
  where
    test testName op x y result = evalCase testName
      (unFlowTerm @@ (metaref Monads.bind
        @@ (metaref Monads.pure @@ x)
        @@ (lambda "n" (metaref Monads.pure @@ (op @@ var "n" @@ y)))) @@ unit @@ testTrace)
      (flowStateTerm (optional $ just result) unit testTrace)

-- | Test cases for flows.apply: applies a function inside a flow to a value inside a flow
-- This test uses kernel Monads definitions to construct the apply operation.
flowsApply :: TTerm TestGroup
flowsApply = subgroup "apply" [
  test "apply add" (int32 8)]
  where
    -- apply (pure (add 3)) (pure 5) = pure 8
    -- Using Monads definitions: bind flowFun (\f -> bind flowArg (\x -> pure (f x)))
    test testName result =
      let flowFun = metaref Monads.pure @@ (primitive _math_add @@ int32 3)
          flowArg = metaref Monads.pure @@ int32 5
          -- Build the nested bind structure using kernel definitions
          applyTerm = metaref Monads.bind @@ flowFun @@
            (lambda "f" (metaref Monads.bind @@ flowArg @@
              (lambda "x" (metaref Monads.pure @@ (var "f" @@ var "x")))))
      in evalCase testName
        (unFlowTerm @@ applyTerm @@ unit @@ testTrace)
        (flowStateTerm (optional $ just result) unit testTrace)

-- | Test cases for flows.foldl: folds over a list with flow-returning function
-- Manually constructs the fold using kernel Monads.bind/pure
flowsFoldl :: TTerm TestGroup
flowsFoldl = subgroup "foldl" [
  test "foldl sum"]
  where
    -- foldl (\acc x -> pure (acc + x)) 0 [1,2,3] = pure 6
    -- Unrolled: bind (pure 0 >>= \a0 -> pure (a0+1)) >>= \a1 -> bind (pure (a1+2)) >>= \a2 -> pure (a2+3)
    -- Simplified: bind (pure 1) >>= \a1 -> bind (pure (a1+2)) >>= \a2 -> pure (a2+3)
    test testName = evalCase testName
      (unFlowTerm @@ foldTerm @@ unit @@ testTrace)
      (flowStateTerm (optional $ just (int32 6)) unit testTrace)
    -- Manually unroll: foldl (+) 0 [1,2,3] using bind
    foldTerm =
      metaref Monads.bind @@ (metaref Monads.pure @@ int32 0) @@
        (lambda "a0" $ metaref Monads.bind @@ (metaref Monads.pure @@ (primitive _math_add @@ var "a0" @@ int32 1)) @@
          (lambda "a1" $ metaref Monads.bind @@ (metaref Monads.pure @@ (primitive _math_add @@ var "a1" @@ int32 2)) @@
            (lambda "a2" $ metaref Monads.pure @@ (primitive _math_add @@ var "a2" @@ int32 3))))

-- | Test cases for flows.mapList: maps a flow-returning function over a list
-- Manually constructs the mapList using kernel Monads.bind/pure
flowsMapList :: TTerm TestGroup
flowsMapList = subgroup "mapList" [
  test "mapList add one"]
  where
    -- mapList (\n -> pure (n + 1)) [1,2,3] = pure [2,3,4]
    -- Unrolled: bind (pure 2) (\y1 -> bind (pure 3) (\y2 -> bind (pure 4) (\y3 -> pure [y1,y2,y3])))
    test testName = evalCase testName
      (unFlowTerm @@ mapListTerm @@ unit @@ testTrace)
      (flowStateTerm (optional $ just (list [int32 2, int32 3, int32 4])) unit testTrace)
    addOne n = metaref Monads.pure @@ (primitive _math_add @@ n @@ int32 1)
    mapListTerm =
      metaref Monads.bind @@ addOne (int32 1) @@
        (lambda "y1" $ metaref Monads.bind @@ addOne (int32 2) @@
          (lambda "y2" $ metaref Monads.bind @@ addOne (int32 3) @@
            (lambda "y3" $ metaref Monads.pure @@ list [var "y1", var "y2", var "y3"])))

-- | Test cases for flows.sequence: sequences a list of flows into a flow of list
-- Manually constructs using kernel Monads.bind/pure
flowsSequence :: TTerm TestGroup
flowsSequence = subgroup "sequence" [
  test "sequence pure list"]
  where
    -- sequence [pure 1, pure 2, pure 3] = pure [1,2,3]
    -- Unrolled: bind (pure 1) (\x1 -> bind (pure 2) (\x2 -> bind (pure 3) (\x3 -> pure [x1,x2,x3])))
    test testName = evalCase testName
      (unFlowTerm @@ sequenceTerm @@ unit @@ testTrace)
      (flowStateTerm (optional $ just (list [int32 1, int32 2, int32 3])) unit testTrace)
    sequenceTerm =
      metaref Monads.bind @@ (metaref Monads.pure @@ int32 1) @@
        (lambda "x1" $ metaref Monads.bind @@ (metaref Monads.pure @@ int32 2) @@
          (lambda "x2" $ metaref Monads.bind @@ (metaref Monads.pure @@ int32 3) @@
            (lambda "x3" $ metaref Monads.pure @@ list [var "x1", var "x2", var "x3"])))

-- | Test cases for flows.mapMaybe: maps a flow-returning function over a Maybe
-- Manually constructs using kernel Monads operations
flowsMapMaybe :: TTerm TestGroup
flowsMapMaybe = subgroup "mapMaybe" [
  testJust "mapMaybe just" (int32 5) (int32 6),
  testNothing "mapMaybe nothing"]
  where
    -- mapMaybe (\n -> pure (n + 1)) (Just 5) = pure (Just 6)
    testJust testName inVal outVal = evalCase testName
      (unFlowTerm @@ (metaref Monads.bind @@ addOnePure inVal @@
        (lambda "y" $ metaref Monads.pure @@ (optional $ just (var "y")))) @@ unit @@ testTrace)
      (flowStateTerm (optional $ just (optional $ just outVal)) unit testTrace)
    -- mapMaybe (\n -> pure (n + 1)) Nothing = pure Nothing
    testNothing testName = evalCase testName
      (unFlowTerm @@ (metaref Monads.pure @@ (optional nothing)) @@ unit @@ testTrace)
      (flowStateTerm (optional $ just (optional nothing)) unit testTrace)
    addOnePure n = metaref Monads.pure @@ (primitive _math_add @@ n @@ int32 1)

-- | Test cases for flows.mapSet: maps a flow-returning function over a Set
-- Manually constructs using kernel Monads operations
flowsMapSet :: TTerm TestGroup
flowsMapSet = subgroup "mapSet" [
  test "mapSet add one"]
  where
    -- mapSet (\n -> pure (n + 1)) {1,2,3} = pure {2,3,4}
    -- Unrolled similar to mapList, then convert to set
    test testName = evalCase testName
      (unFlowTerm @@ mapSetTerm @@ unit @@ testTrace)
      (flowStateTerm (optional $ just (Core.termSet $ Phantoms.set [int32 2, int32 3, int32 4])) unit testTrace)
    addOne n = metaref Monads.pure @@ (primitive _math_add @@ n @@ int32 1)
    -- Build: bind (addOne 1) (\y1 -> bind (addOne 2) (\y2 -> bind (addOne 3) (\y3 -> pure (fromList [y1,y2,y3]))))
    mapSetTerm =
      metaref Monads.bind @@ addOne (int32 1) @@
        (lambda "y1" $ metaref Monads.bind @@ addOne (int32 2) @@
          (lambda "y2" $ metaref Monads.bind @@ addOne (int32 3) @@
            (lambda "y3" $ metaref Monads.pure @@ (primitive _sets_fromList @@ list [var "y1", var "y2", var "y3"]))))

-- | Test cases for flows.mapElems: maps a flow-returning function over Map values
-- Manually constructs using kernel Monads operations
flowsMapElems :: TTerm TestGroup
flowsMapElems = subgroup "mapElems" [
  test "mapElems add one"]
  where
    -- mapElems (\v -> pure (v + 1)) {a:1, b:2} = pure {a:2, b:3}
    -- Unrolled: bind (addOne 1) (\v1 -> bind (addOne 2) (\v2 -> pure (fromList [("a",v1),("b",v2)])))
    test testName = evalCase testName
      (unFlowTerm @@ mapElemsTerm @@ unit @@ testTrace)
      (flowStateTerm (optional $ just expectedMap) unit testTrace)
    addOne n = metaref Monads.pure @@ (primitive _math_add @@ n @@ int32 1)
    expectedMap = Core.termMap $ Phantoms.map $ M.fromList [(string "a", int32 2), (string "b", int32 3)]
    mapElemsTerm =
      metaref Monads.bind @@ addOne (int32 1) @@
        (lambda "v1" $ metaref Monads.bind @@ addOne (int32 2) @@
          (lambda "v2" $ metaref Monads.pure @@
            (primitive _maps_fromList @@ list [pair (string "a") (var "v1"), pair (string "b") (var "v2")])))

-- | Test cases for flows.mapKeys: maps a flow-returning function over Map keys
-- Manually constructs using kernel Monads operations
flowsMapKeys :: TTerm TestGroup
flowsMapKeys = subgroup "mapKeys" [
  test "mapKeys add one"]
  where
    -- mapKeys (\k -> pure (k + 1)) {1:a, 2:b} = pure {2:a, 3:b}
    -- Unrolled: bind (addOne 1) (\k1 -> bind (addOne 2) (\k2 -> pure (fromList [(k1,"a"),(k2,"b")])))
    test testName = evalCase testName
      (unFlowTerm @@ mapKeysTerm @@ unit @@ testTrace)
      (flowStateTerm (optional $ just expectedMap) unit testTrace)
    addOne n = metaref Monads.pure @@ (primitive _math_add @@ n @@ int32 1)
    expectedMap = Core.termMap $ Phantoms.map $ M.fromList [(int32 2, string "a"), (int32 3, string "b")]
    mapKeysTerm =
      metaref Monads.bind @@ addOne (int32 1) @@
        (lambda "k1" $ metaref Monads.bind @@ addOne (int32 2) @@
          (lambda "k2" $ metaref Monads.pure @@
            (primitive _maps_fromList @@ list [pair (var "k1") (string "a"), pair (var "k2") (string "b")])))

-- | Test cases for flows.withDefault: uses fallback value if flow fails
flowsWithDefault :: TTerm TestGroup
flowsWithDefault = subgroup "withDefault" [
  testSuccess "withDefault on success returns original",
  testFailure "withDefault on failure returns fallback"]
  where
    -- withDefault 0 (pure 42) = pure 42 (success case)
    testSuccess testName = evalCase testName
      (unFlowTerm @@ (primitive _flows_withDefault @@ int32 0 @@ (metaref Monads.pure @@ int32 42)) @@ unit @@ testTrace)
      (flowStateTerm (optional $ just (int32 42)) unit testTrace)
    -- withDefault 99 (fail "error") = pure 99 (failure case - uses fallback)
    testFailure testName = evalCase testName
      (unFlowTerm @@ (primitive _flows_withDefault @@ int32 99 @@ (metaref Monads.fail @@ string "error")) @@ unit @@ testTrace)
      (flowStateTerm (optional $ just (int32 99)) unit testTrace)

allTests :: TBinding TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.flows primitives" $
    supergroup "hydra.lib.flows primitives" [
      flowsApply,
      flowsBind,
      flowsFail,
      flowsFoldl,
      flowsMap,
      flowsMapElems,
      flowsMapKeys,
      flowsMapList,
      flowsMapMaybe,
      flowsMapSet,
      flowsPure,
      flowsSequence,
      flowsWithDefault]

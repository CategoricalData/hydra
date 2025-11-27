module Hydra.Sources.Test.Lib.Flows where

import Hydra.Kernel
import Hydra.Testing
import Hydra.Dsl.Meta.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Core as Core
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import Hydra.Dsl.Meta.Terms as MetaTerms
import qualified Hydra.Sources.Kernel.Types.All as KernelTypes
import qualified Hydra.Sources.Kernel.Types.Testing as TestingTypes
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Data.Map as M


module_ :: Module
module_ = Module (Namespace "hydra.test.lib.flows") elements [] [] $
    Just "Test cases for hydra.lib.flows primitives"
  where
    elements = [el allTestsDef]

-- Test groups for hydra.lib.flows primitives

flowsApply :: TTerm TestGroup
flowsApply = subgroup "apply" [
  test "apply addition function" 5 10,
  test "apply multiplication function" 3 15]
  where
    test name x result = primCase name _flows_apply [
        primitive _flows_pure @@ lambda "n" (primitive _math_add @@ var "n" @@ int32 x),
        primitive _flows_pure @@ int32 x]
      (primitive _flows_pure @@ int32 result)

flowsBind :: TTerm TestGroup
flowsBind = subgroup "bind" [
  test "bind single computation" 5 10,
  test "bind chained computations" 3 12]
  where
    test name x result = primCase name _flows_bind [
        primitive _flows_pure @@ int32 x,
        lambda "n" (primitive _flows_pure @@ (primitive _math_add @@ var "n" @@ int32 x))]
      (primitive _flows_pure @@ int32 result)

flowsFail :: TTerm TestGroup
flowsFail = subgroup "fail" [
  test "fail with message"]
  where
    test name = primCase name _flows_fail [
        MetaTerms.string "test error message"]
      (primitive _flows_fail @@ MetaTerms.string "test error message")

flowsFoldl :: TTerm TestGroup
flowsFoldl = subgroup "foldl" [
  test "fold sum" [1, 2, 3] 0 6,
  test "fold product" [2, 3, 4] 1 24,
  test "fold empty list" [] 10 10]
  where
    test name xs init result = primCase name _flows_foldl [
        lambda "acc" (lambda "x" (primitive _flows_pure @@ (primitive _math_add @@ var "acc" @@ var "x"))),
        int32 init,
        list (Prelude.map int32 xs)]
      (primitive _flows_pure @@ int32 result)

flowsMap :: TTerm TestGroup
flowsMap = subgroup "map" [
  test "map add 5" 5 10,
  test "map add 0" 0 0]
  where
    test name x result = primCase name _flows_map [
        lambda "n" (primitive _math_add @@ var "n" @@ int32 x),
        primitive _flows_pure @@ int32 x]
      (primitive _flows_pure @@ int32 result)

flowsMapElems :: TTerm TestGroup
flowsMapElems = subgroup "mapElems" [
  test "map double values" [(1, 2), (3, 4)] [(1, 4), (3, 8)],
  test "map empty map" [] []]
  where
    test name input expected = primCase name _flows_mapElems [
        lambda "x" (primitive _flows_pure @@ (primitive _math_mul @@ var "x" @@ int32 2)),
        Core.termMap (Phantoms.map $ M.fromList $ Prelude.map (\(k, v) -> (int32 k, int32 v)) input)]
      (primitive _flows_pure @@ (Core.termMap $ Phantoms.map $ M.fromList $ Prelude.map (\(k, v) -> (int32 k, int32 v)) expected))

flowsMapKeys :: TTerm TestGroup
flowsMapKeys = subgroup "mapKeys" [
  test "map double keys" [(1, 2), (3, 4)] [(2, 2), (6, 4)],
  test "map empty map" [] []]
  where
    test name input expected = primCase name _flows_mapKeys [
        lambda "x" (primitive _flows_pure @@ (primitive _math_mul @@ var "x" @@ int32 2)),
        Core.termMap (Phantoms.map $ M.fromList $ Prelude.map (\(k, v) -> (int32 k, int32 v)) input)]
      (primitive _flows_pure @@ (Core.termMap $ Phantoms.map $ M.fromList $ Prelude.map (\(k, v) -> (int32 k, int32 v)) expected))

flowsMapList :: TTerm TestGroup
flowsMapList = subgroup "mapList" [
  test "map double list" [1, 2, 3] [2, 4, 6],
  test "map empty list" [] []]
  where
    test name input expected = primCase name _flows_mapList [
        lambda "x" (primitive _flows_pure @@ (primitive _math_mul @@ var "x" @@ int32 2)),
        list (Prelude.map int32 input)]
      (primitive _flows_pure @@ (list $ Prelude.map int32 expected))

flowsMapMaybe :: TTerm TestGroup
flowsMapMaybe = subgroup "mapMaybe" [
  test "map over just" 5 (Just 10),
  test "map over nothing" 5 Nothing]
  where
    test name x result = primCase name _flows_mapMaybe [
        lambda "n" (primitive _flows_pure @@ (primitive _math_add @@ var "n" @@ int32 x)),
        case result of
          Nothing -> Core.termMaybe nothing
          Just r -> Core.termMaybe (just (int32 x))]
      (primitive _flows_pure @@ case result of
        Nothing -> Core.termMaybe nothing
        Just r -> Core.termMaybe (just (int32 r)))

flowsMapSet :: TTerm TestGroup
flowsMapSet = subgroup "mapSet" [
  test "map double set" [1, 2, 3] [2, 4, 6],
  test "map empty set" [] []]
  where
    test name input expected = primCase name _flows_mapSet [
        lambda "x" (primitive _flows_pure @@ (primitive _math_mul @@ var "x" @@ int32 2)),
        Core.termSet (Phantoms.set $ Prelude.map int32 input)]
      (primitive _flows_pure @@ (Core.termSet $ Phantoms.set $ Prelude.map int32 expected))

flowsPure :: TTerm TestGroup
flowsPure = subgroup "pure" [
  test "pure integer" 42,
  test "pure zero" 0,
  test "pure negative" (-5)]
  where
    test name x = primCase name _flows_pure [int32 x]
      (primitive _flows_pure @@ int32 x)

flowsSequence :: TTerm TestGroup
flowsSequence = subgroup "sequence" [
  test "sequence list of flows" [1, 2, 3],
  test "sequence empty list" []]
  where
    test name xs = primCase name _flows_sequence [
        list (Prelude.map (\x -> primitive _flows_pure @@ int32 x) xs)]
      (primitive _flows_pure @@ (list $ Prelude.map int32 xs))

allTestsDef :: TBinding TestGroup
allTestsDef = definitionInModule module_ "allTests" $
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
      flowsSequence]

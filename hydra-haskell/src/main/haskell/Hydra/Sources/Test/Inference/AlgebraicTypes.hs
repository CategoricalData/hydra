module Hydra.Sources.Test.Inference.AlgebraicTypes where

-- Standard imports for kernel tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing as Testing
import Hydra.Dsl.Meta.Terms as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core as Core
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Meta.Types as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List as L
import qualified Data.Map  as M


ns :: Namespace
ns = Namespace "hydra.test.inference.algebraicTypes"

module_ :: Module
module_ = Module ns elements
    [TestGraph.ns]
    kernelTypesNamespaces
    (Just "Inference tests for algebraic data types")
  where
    elements = [
      Phantoms.toBinding allTests,
      Phantoms.toBinding testGroupForEithers,
      Phantoms.toBinding testGroupForFolds,
      Phantoms.toBinding testGroupForLists,
      Phantoms.toBinding testGroupForMaps,
      Phantoms.toBinding testGroupForOptionals,
      Phantoms.toBinding testGroupForPairs,
      Phantoms.toBinding testGroupForSets]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

allTests :: TBinding TestGroup
allTests = define "allTests" $
  Phantoms.doc "Algebraic data type tests" $
  supergroup "Algebraic terms" [
    testGroupForEithers,
    testGroupForFolds,
    testGroupForLists,
    testGroupForMaps,
    testGroupForOptionals,
    testGroupForPairs,
    testGroupForSets]

testGroupForEithers :: TBinding TestGroup
testGroupForEithers = define "testGroupForEithers" $
  supergroup "Either terms" [
    subgroup "Left values" [
      expectMono 1 []
        (list [left $ string "error", right $ int32 42])
        (T.list $ T.either_ T.string T.int32),
      expectPoly 2 []
        (left $ string "error")
        ["t0"] (T.either_ T.string (T.var "t0"))],

    subgroup "Right values" [
      expectMono 1 []
        (list [right $ int32 42, left $ string "error"])
        (T.list $ T.either_ T.string T.int32),
      expectPoly 2 []
        (right $ int32 42)
        ["t0"] (T.either_ (T.var "t0") T.int32)],

    subgroup "Polymorphic either values" [
      expectPoly 1 []
        (left $ list [])
        ["t0", "t1"] (T.either_ (T.list $ T.var "t0") (T.var "t1")),
      expectPoly 2 []
        (right $ list [])
        ["t0", "t1"] (T.either_ (T.var "t0") (T.list $ T.var "t1"))],

    subgroup "Nested either values" [
      expectMono 1 []
        (list [left $ left $ int32 1, left $ right $ string "nested", right $ true])
        (T.list $ T.either_ (T.either_ T.int32 T.string) T.boolean),
      expectMono 2 []
        (list [right $ left $ int32 42, right $ right $ true, left $ string "foo"])
        (T.list $ T.either_ T.string (T.either_ T.int32 T.boolean))],

    subgroup "Either in lambda" [
      expectPoly 1 []
        (lambda "x" (left $ var "x"))
        ["t0", "t1"] (T.function (T.var "t0") (T.either_ (T.var "t0") (T.var "t1"))),
      expectPoly 2 []
        (lambda "x" (right $ var "x"))
        ["t0", "t1"] (T.function (T.var "t0") (T.either_ (T.var "t1") (T.var "t0")))],

    subgroup "Either in data structures" [
      expectMono 1 []
        (list [left $ string "error", right $ int32 42])
        (T.list $ T.either_ T.string T.int32),
      expectPoly 2 []
        (pair (list [left $ string "error", right $ int32 42]) (list []))
        ["t0"] (T.pair (T.list $ T.either_ T.string T.int32) (T.list $ T.var "t0"))]]

testGroupForFolds :: TBinding TestGroup
testGroupForFolds = define "testGroupForFolds" $
  supergroup "Eliminations" [
    subgroup "List eliminations (folds)" [
      expectMono 1 [tag_disabledForMinimalInference]
        foldAdd
        (T.functionMany [T.int32, T.list T.int32, T.int32]),
      expectMono 2 [tag_disabledForMinimalInference]
        (foldAdd @@ int32 0)
        (T.function (T.list T.int32) T.int32),
      expectMono 3 [tag_disabledForMinimalInference]
        (foldAdd @@ int32 0 @@ (list (int32 <$> [1, 2, 3, 4, 5])))
        T.int32],

    subgroup "Optional eliminations" [
      expectMono 1 [tag_disabledForMinimalInference]
        (primitive _maybes_maybe @@ (int32 42) @@ (primitive _math_negate))
        (T.function (T.optional T.int32) T.int32),
      expectMono 2 [tag_disabledForMinimalInference]
        (primitive _maybes_maybe @@ (int32 42) @@ (primitive _math_negate) @@ (optional (just $ int32 137)))
        T.int32,
      expectMono 3 [tag_disabledForMinimalInference]
        (primitive _maybes_maybe @@ (int32 42) @@ (primitive _math_negate) @@ optional nothing)
        T.int32,
      expectPoly 4 [tag_disabledForMinimalInference]
        (lambda "x" $ primitive _maybes_maybe @@ (var "x") @@ (primitive _maybes_pure) @@ var "x")
        ["t0"] (T.function (T.optional $ T.var "t0") (T.optional $ T.var "t0")),
      expectPoly 5 [tag_disabledForMinimalInference]
        (primitive _maybes_maybe @@ (list []) @@ (lambda "x" $ list [var "x"]))
        ["t0"] (T.function (T.optional $ T.var "t0") (T.list $ T.var "t0"))]]
  where
    foldAdd = primitive _lists_foldl @@ primitive _math_add

testGroupForLists :: TBinding TestGroup
testGroupForLists = define "testGroupForLists" $
  supergroup "List terms" [
    subgroup "List of strings" [
      expectMono 1 []
        (list [string "foo", string "bar"])
        (T.list T.string)],
    subgroup "List of lists of strings" [
      expectMono 1 []
        (list [list [string "foo"], list []])
        (T.list $ T.list T.string)],
    subgroup "Empty list" [
      expectPoly 1 []
        (list [])
        ["t0"] (T.list $ T.var "t0")],
    subgroup "List containing an empty list" [
      expectPoly 1 []
        (list [list []])
        ["t0"] (T.list $ T.list $ T.var "t0")],
    subgroup "Lambda producing a polymorphic list" [
      expectPoly 1 []
        (lambda "x" (list [var "x"]))
        ["t0"] (T.function (T.var "t0") (T.list $ T.var "t0"))],
    subgroup "Lambda producing a list of integers" [
      expectMono 1 []
        (lambda "x" (list [var "x", int32 42]))
        (T.function T.int32 $ T.list T.int32)],
    subgroup "List with repeated variables" [
      expectMono 1 []
        (lambda "x" (list [var "x", string "foo", var "x"]))
        (T.function T.string (T.list T.string))]]

testGroupForMaps :: TBinding TestGroup
testGroupForMaps = define "testGroupForMaps" $
  subgroup "Map terms" [
    expectMono 1 [tag_disabledForMinimalInference]
      (mapTerm [
        (string "firstName", string "Arthur"),
        (string "lastName", string "Dent")])
      (T.map T.string T.string),
    expectPoly 2 [tag_disabledForMinimalInference]
      (mapTerm [])
      ["t0", "t1"] (T.map (T.var "t0") (T.var "t1")),
    expectPoly 3 [tag_disabledForMinimalInference]
      (lambdas ["x", "y"] $ mapTerm
        [(var "x", float64 0.1), (var "y", float64 0.2)])
      ["t0"] (T.function (T.var "t0") (T.function (T.var "t0") (T.map (T.var "t0") T.float64)))]

testGroupForOptionals :: TBinding TestGroup
testGroupForOptionals = define "testGroupForOptionals" $
  subgroup "Optional terms" [
    expectMono 1 [tag_disabledForMinimalInference]
      (optional $ just $ int32 42)
      (T.optional T.int32),
    expectPoly 2 [tag_disabledForMinimalInference]
      (optional nothing)
      ["t0"] (T.optional $ T.var "t0")]

testGroupForPairs :: TBinding TestGroup
testGroupForPairs = define "testGroupForPairs" $
  supergroup "Pair terms" [
    subgroup "Monotyped pairs" [
      expectMono 1 [tag_disabledForMinimalInference]
        (pair (string "foo") (int32 42))
        (T.pair T.string T.int32),
      expectMono 2 [tag_disabledForMinimalInference]
        (pair (string "foo") (list [float32 42.0, float32 137.0]))
        (T.pair T.string (T.list T.float32))],

    subgroup "Polytyped pairs" [
      expectPoly 1 [tag_disabledForMinimalInference]
        (pair (list []) (string "foo"))
        ["t0"] (T.pair (T.list $ T.var "t0") T.string),
      expectPoly 2 [tag_disabledForMinimalInference]
        (pair (list []) (list []))
        ["t0", "t1"] (T.pair (T.list $ T.var "t0") (T.list $ T.var "t1"))],

    subgroup "Nested pairs" [
      expectMono 1 [tag_disabledForMinimalInference]
        (pair (pair (int32 1) (string "nested")) true)
        (T.pair (T.pair T.int32 T.string) T.boolean),
      expectMono 2 [tag_disabledForMinimalInference]
        (pair (string "foo") (pair (int32 42) (list [float32 42.0])))
        (T.pair T.string (T.pair T.int32 (T.list T.float32)))],

    subgroup "Pairs in lambda" [
      expectPoly 1 [tag_disabledForMinimalInference]
        (lambda "x" (pair (var "x") (string "constant")))
        ["t0"] (T.function (T.var "t0") (T.pair (T.var "t0") T.string)),
      expectPoly 2 [tag_disabledForMinimalInference]
        (lambda "p" (pair (var "p") (var "p")))
        ["t0"] (T.function (T.var "t0") (T.pair (T.var "t0") (T.var "t0")))],

    subgroup "Pairs in data structures" [
      expectMono 1 [tag_disabledForMinimalInference]
        (list [pair (string "a") (int32 1), pair (string "b") (int32 2)])
        (T.list $ T.pair T.string T.int32),
      expectPoly 2 [tag_disabledForMinimalInference]
        (list [pair (list []) (string "foo")])
        ["t0"] (T.list $ T.pair (T.list $ T.var "t0") T.string)],

    subgroup "Additional cases" [
      expectMono 1 [tag_disabledForMinimalInference]
        (pair (int32 42) (string "foo"))
        (T.pair T.int32 T.string),
      expectPoly 2 [tag_disabledForMinimalInference]
        (pair (list []) (string "foo"))
        ["t0"] (T.pair (T.list $ T.var "t0") T.string),
      expectPoly 3 [tag_disabledForMinimalInference]
        (pair (list []) (list []))
        ["t0", "t1"] (T.pair (T.list $ T.var "t0") (T.list $ T.var "t1"))]]

testGroupForSets :: TBinding TestGroup
testGroupForSets = define "testGroupForSets" $
  subgroup "Set terms" [
    expectMono 1 [tag_disabledForMinimalInference]
      (set [true])
      (T.set T.boolean),
    expectPoly 2 [tag_disabledForMinimalInference]
      (set [set []])
      ["t0"] (T.set $ T.set $ T.var "t0")]

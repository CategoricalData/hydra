module Hydra.Sources.Test.Inference.Classes where

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


ns :: Namespace
ns = Namespace "hydra.test.inference.classes"

module_ :: Module
module_ = Module ns elements
    [TestGraph.ns]
    kernelTypesNamespaces
    (Just "Inference tests for type class constraints (ordering and equality)")
  where
    elements = [
      Phantoms.toBinding allTests,
      Phantoms.toBinding testGroupForMonomorphicConstraints,
      Phantoms.toBinding testGroupForPrimitiveReferences,
      Phantoms.toBinding testGroupForPartialApplication,
      Phantoms.toBinding testGroupForLetBindings,
      Phantoms.toBinding testGroupForComposition,
      Phantoms.toBinding testGroupForNestedContainers]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

allTests :: TBinding TestGroup
allTests = define "allTests" $
  Phantoms.doc "Type class constraint inference tests" $
  supergroup "Type classes" [
    testGroupForMonomorphicConstraints,
    testGroupForPrimitiveReferences,
    testGroupForPartialApplication,
    testGroupForLetBindings,
    testGroupForComposition,
    testGroupForNestedContainers]

-- | When all type variables are instantiated to concrete types, constraints vanish.
testGroupForMonomorphicConstraints :: TBinding TestGroup
testGroupForMonomorphicConstraints = define "testGroupForMonomorphicConstraints" $
  supergroup "Monomorphic (constraints vanish)" [

    subgroup "Map operations with concrete types" [
      -- maps.fromList [("a", 1)] => Map String Int32
      expectMono 1 []
        (primitive _maps_fromList @@ list [pair (string "a") (int32 1)])
        (T.map T.string T.int32),
      -- maps.lookup "k" (maps.singleton "k" 42) => Optional Int32
      expectMono 2 []
        (primitive _maps_lookup @@ string "k" @@ (primitive _maps_singleton @@ string "k" @@ int32 42))
        (T.optional T.int32),
      -- maps.insert "k" 42 maps.empty => Map String Int32
      expectMono 3 []
        (primitive _maps_insert @@ string "k" @@ int32 42 @@ primitive _maps_empty)
        (T.map T.string T.int32)],

    subgroup "Set operations with concrete types" [
      -- sets.fromList [1, 2, 3] => Set Int32
      expectMono 1 []
        (primitive _sets_fromList @@ list [int32 1, int32 2, int32 3])
        (T.set T.int32),
      -- sets.member 42 (sets.singleton 42) => Boolean
      expectMono 2 []
        (primitive _sets_member @@ int32 42 @@ (primitive _sets_singleton @@ int32 42))
        T.boolean],

    subgroup "Equality operations with concrete types" [
      -- equality.equal 1 2 => Boolean
      expectMono 1 []
        (primitive _equality_equal @@ int32 1 @@ int32 2)
        T.boolean,
      -- equality.compare "a" "b" => forall t0. t0
      expectPoly 2 []
        (primitive _equality_compare @@ string "a" @@ string "b")
        ["t0"] (T.var "t0")],

    subgroup "List operations with concrete types" [
      -- lists.sort [3, 1, 2] => [Int32]
      expectMono 1 []
        (primitive _lists_sort @@ list [int32 3, int32 1, int32 2])
        (T.list T.int32)]]

-- | Bare primitive references should retain constraints on type variables.
testGroupForPrimitiveReferences :: TBinding TestGroup
testGroupForPrimitiveReferences = define "testGroupForPrimitiveReferences" $
  supergroup "Primitive references with constraints" [

    subgroup "Map primitives (ordering on key type)" [
      -- maps.fromList => forall k v. Ord k => [(k, v)] -> Map k v
      expectPolyConstrained 1 []
        (primitive _maps_fromList)
        ["t0", "t1"] [("t0", ["ordering"])]
        (T.function (T.list $ T.pair (T.var "t0") (T.var "t1")) (T.map (T.var "t0") (T.var "t1"))),
      -- maps.lookup => forall k v. Ord k => k -> Map k v -> Optional v
      expectPolyConstrained 2 []
        (primitive _maps_lookup)
        ["t0", "t1"] [("t0", ["ordering"])]
        (T.functionMany [T.var "t0", T.map (T.var "t0") (T.var "t1"), T.optional (T.var "t1")]),
      -- maps.insert => forall k v. Ord k => k -> v -> Map k v -> Map k v
      expectPolyConstrained 3 []
        (primitive _maps_insert)
        ["t0", "t1"] [("t0", ["ordering"])]
        (T.functionMany [T.var "t0", T.var "t1", T.map (T.var "t0") (T.var "t1"), T.map (T.var "t0") (T.var "t1")]),
      -- maps.map => forall k v1 v2. Ord k => (v1 -> v2) -> Map k v1 -> Map k v2
      expectPolyConstrained 4 []
        (primitive _maps_map)
        ["t0", "t1", "t2"] [("t2", ["ordering"])]
        (T.functionMany [T.function (T.var "t0") (T.var "t1"), T.map (T.var "t2") (T.var "t0"), T.map (T.var "t2") (T.var "t1")]),
      -- maps.empty => forall k v. Ord k => Map k v
      expectPolyConstrained 5 []
        (primitive _maps_empty)
        ["t0", "t1"] [("t0", ["ordering"])]
        (T.map (T.var "t0") (T.var "t1"))],

    subgroup "Set primitives (ordering on element type)" [
      -- sets.fromList => forall x. Ord x => [x] -> Set x
      expectPolyConstrained 1 []
        (primitive _sets_fromList)
        ["t0"] [("t0", ["ordering"])]
        (T.function (T.list $ T.var "t0") (T.set $ T.var "t0")),
      -- sets.member => forall x. Ord x => x -> Set x -> Boolean
      expectPolyConstrained 2 []
        (primitive _sets_member)
        ["t0"] [("t0", ["ordering"])]
        (T.functionMany [T.var "t0", T.set (T.var "t0"), T.boolean]),
      -- sets.insert => forall x. Ord x => x -> Set x -> Set x
      expectPolyConstrained 3 []
        (primitive _sets_insert)
        ["t0"] [("t0", ["ordering"])]
        (T.functionMany [T.var "t0", T.set (T.var "t0"), T.set (T.var "t0")]),
      -- sets.map => forall a b. (Ord a, Ord b) => (a -> b) -> Set a -> Set b
      expectPolyConstrained 4 []
        (primitive _sets_map)
        ["t0", "t1"] [("t0", ["ordering"]), ("t1", ["ordering"])]
        (T.functionMany [T.function (T.var "t0") (T.var "t1"), T.set (T.var "t0"), T.set (T.var "t1")])],

    subgroup "Equality primitives" [
      -- equality.equal => forall x. Eq x => x -> x -> Boolean
      expectPolyConstrained 1 []
        (primitive _equality_equal)
        ["t0"] [("t0", ["equality"])]
        (T.functionMany [T.var "t0", T.var "t0", T.boolean]),
      -- equality.compare => forall x y. Ord x => x -> x -> y
      expectPolyConstrained 2 []
        (primitive _equality_compare)
        ["t0", "t1"] [("t0", ["ordering"])]
        (T.functionMany [T.var "t0", T.var "t0", T.var "t1"])],

    subgroup "List primitives with constraints" [
      -- lists.sort => forall x. Ord x => [x] -> [x]
      expectPolyConstrained 1 []
        (primitive _lists_sort)
        ["t0"] [("t0", ["ordering"])]
        (T.function (T.list $ T.var "t0") (T.list $ T.var "t0")),
      -- lists.nub => forall x. Eq x => [x] -> [x]
      expectPolyConstrained 2 []
        (primitive _lists_nub)
        ["t0"] [("t0", ["equality"])]
        (T.function (T.list $ T.var "t0") (T.list $ T.var "t0")),
      -- lists.elem => forall x. Eq x => x -> [x] -> Boolean
      expectPolyConstrained 3 []
        (primitive _lists_elem)
        ["t0"] [("t0", ["equality"])]
        (T.functionMany [T.var "t0", T.list (T.var "t0"), T.boolean])]]

-- | Partial application where the constrained variable is not yet fixed.
testGroupForPartialApplication :: TBinding TestGroup
testGroupForPartialApplication = define "testGroupForPartialApplication" $
  supergroup "Partial application preserving constraints" [

    subgroup "Map partial application" [
      -- \k -> maps.lookup k => forall k v. Ord k => k -> Map k v -> Optional v
      expectPolyConstrained 1 []
        (lambda "k" $ primitive _maps_lookup @@ var "k")
        ["t0", "t1"] [("t0", ["ordering"])]
        (T.functionMany [T.var "t0", T.map (T.var "t0") (T.var "t1"), T.optional (T.var "t1")]),
      -- \k -> \v -> maps.singleton k v => forall k v. Ord k => k -> v -> Map k v
      expectPolyConstrained 2 []
        (lambda "k" $ lambda "v" $ primitive _maps_singleton @@ var "k" @@ var "v")
        ["t0", "t1"] [("t0", ["ordering"])]
        (T.functionMany [T.var "t0", T.var "t1", T.map (T.var "t0") (T.var "t1")])],

    subgroup "Set partial application" [
      -- \x -> sets.member x => forall x. Ord x => x -> Set x -> Boolean
      expectPolyConstrained 1 []
        (lambda "x" $ primitive _sets_member @@ var "x")
        ["t0"] [("t0", ["ordering"])]
        (T.functionMany [T.var "t0", T.set (T.var "t0"), T.boolean])],

    subgroup "Equality partial application" [
      -- \x -> \y -> equality.equal x y => forall x. Eq x => x -> x -> Boolean
      expectPolyConstrained 1 []
        (lambda "x" $ lambda "y" $ primitive _equality_equal @@ var "x" @@ var "y")
        ["t0"] [("t0", ["equality"])]
        (T.functionMany [T.var "t0", T.var "t0", T.boolean])],

    subgroup "Partial application fixing the constrained variable" [
      -- \v -> maps.singleton "key" v => forall v. String -> v -> Map String v (no constraint)
      expectPoly 1 []
        (lambda "v" $ primitive _maps_singleton @@ string "key" @@ var "v")
        ["t0"] (T.function (T.var "t0") (T.map T.string (T.var "t0")))]]

-- | Constraints should propagate through let-bound generalizations.
testGroupForLetBindings :: TBinding TestGroup
testGroupForLetBindings = define "testGroupForLetBindings" $
  supergroup "Let binding constraint propagation" [

    subgroup "Simple let-bound wrappers" [
      -- let lookup = \k -> \m -> maps.lookup k m in lookup
      expectPolyConstrained 1 []
        (lets [
          "lookup">: lambda "k" $ lambda "m" $ primitive _maps_lookup @@ var "k" @@ var "m"]
          $ var "lookup")
        ["t0", "t1"] [("t0", ["ordering"])]
        (T.functionMany [T.var "t0", T.map (T.var "t0") (T.var "t1"), T.optional (T.var "t1")]),
      -- let member = \x -> \s -> sets.member x s in member
      expectPolyConstrained 2 []
        (lets [
          "member">: lambda "x" $ lambda "s" $ primitive _sets_member @@ var "x" @@ var "s"]
          $ var "member")
        ["t0"] [("t0", ["ordering"])]
        (T.functionMany [T.var "t0", T.set (T.var "t0"), T.boolean]),
      -- let fromList = maps.fromList in fromList
      expectPolyConstrained 3 []
        (lets [
          "fromList">: primitive _maps_fromList]
          $ var "fromList")
        ["t0", "t1"] [("t0", ["ordering"])]
        (T.function (T.list $ T.pair (T.var "t0") (T.var "t1")) (T.map (T.var "t0") (T.var "t1")))],

    subgroup "Let-bound with partial instantiation" [
      -- let f = \m -> maps.map math.negate m in f  => Ord k => Map k Int32 -> Map k Int32
      expectPolyConstrained 1 []
        (lets [
          "f">: lambda "m" $ primitive _maps_map @@ primitive _math_negate @@ var "m"]
          $ var "f")
        ["t0"] [("t0", ["ordering"])]
        (T.function (T.map (T.var "t0") T.int32) (T.map (T.var "t0") T.int32)),
      -- let f = \xs -> sets.fromList xs in f  => Ord x => [x] -> Set x
      expectPolyConstrained 2 []
        (lets [
          "f">: lambda "xs" $ primitive _sets_fromList @@ var "xs"]
          $ var "f")
        ["t0"] [("t0", ["ordering"])]
        (T.function (T.list $ T.var "t0") (T.set $ T.var "t0"))],

    subgroup "Multiple uses of a constrained let binding" [
      -- let f = maps.fromList in pair (f [("a", 1)]) (f [(true, "x")])
      -- Both uses instantiate f, constraints vanish at concrete types
      expectMono 1 []
        (lets [
          "f">: primitive _maps_fromList]
          $ pair (var "f" @@ list [pair (string "a") (int32 1)])
                 (var "f" @@ list [pair true (string "x")]))
        (T.pair (T.map T.string T.int32) (T.map T.boolean T.string))]]

-- | Constraint propagation through function composition.
testGroupForComposition :: TBinding TestGroup
testGroupForComposition = define "testGroupForComposition" $
  supergroup "Composition and constraint merging" [

    subgroup "Composing constrained primitives" [
      -- \xs -> maps.fromList (lists.map (\x -> pair x x) xs)
      -- Key type needs Ord (from maps.fromList)
      expectPolyConstrained 1 []
        (lambda "xs" $ primitive _maps_fromList @@ (primitive _lists_map @@ (lambda "x" $ pair (var "x") (var "x")) @@ var "xs"))
        ["t0"] [("t0", ["ordering"])]
        (T.function (T.list $ T.var "t0") (T.map (T.var "t0") (T.var "t0"))),
      -- \f -> \xs -> sets.fromList (lists.map f xs)
      -- Result element type needs Ord (from sets.fromList)
      expectPolyConstrained 2 []
        (lambda "f" $ lambda "xs" $ primitive _sets_fromList @@ (primitive _lists_map @@ var "f" @@ var "xs"))
        ["t0", "t1"] [("t1", ["ordering"])]
        (T.functionMany [T.function (T.var "t0") (T.var "t1"), T.list (T.var "t0"), T.set (T.var "t1")])],

    subgroup "Composing map and sort" [
      -- \m -> maps.map lists.sort m
      -- Value type needs Ord (from lists.sort), key type needs Ord (from maps.map)
      expectPolyConstrained 1 []
        (lambda "m" $ primitive _maps_map @@ primitive _lists_sort @@ var "m")
        ["t0", "t1"] [("t0", ["ordering"]), ("t1", ["ordering"])]
        (T.function (T.map (T.var "t0") (T.list $ T.var "t1")) (T.map (T.var "t0") (T.list $ T.var "t1")))]]

-- | Nested container types where constraints apply at multiple levels.
testGroupForNestedContainers :: TBinding TestGroup
testGroupForNestedContainers = define "testGroupForNestedContainers" $
  supergroup "Nested containers" [

    subgroup "Maps of sets" [
      -- \m -> maps.map sets.fromList m
      -- Map key needs Ord, set element needs Ord
      expectPolyConstrained 1 []
        (lambda "m" $ primitive _maps_map @@ primitive _sets_fromList @@ var "m")
        ["t0", "t1"] [("t0", ["ordering"]), ("t1", ["ordering"])]
        (T.function (T.map (T.var "t0") (T.list $ T.var "t1")) (T.map (T.var "t0") (T.set $ T.var "t1")))],

    subgroup "Sets of sets" [
      -- \xss -> sets.map sets.fromList xss
      -- Outer: Set (List x) needs Ord on (List x); inner: sets.fromList needs Ord on x
      -- Both source and target element types need Ord
      expectPolyConstrained 1 []
        (lambda "xss" $ primitive _sets_map @@ primitive _sets_fromList @@ var "xss")
        ["t0"] [("t0", ["ordering"])]
        (T.function (T.set $ T.list $ T.var "t0") (T.set $ T.set $ T.var "t0"))],

    subgroup "Map from sorted list" [
      -- \xs -> maps.fromList (lists.map (\x -> pair x (sets.singleton x)) xs)
      -- Key needs Ord (maps.fromList), element needs Ord (sets.singleton) — same variable
      expectPolyConstrained 1 []
        (lambda "xs" $ primitive _maps_fromList @@
          (primitive _lists_map @@ (lambda "x" $ pair (var "x") (primitive _sets_singleton @@ var "x")) @@ var "xs"))
        ["t0"] [("t0", ["ordering"])]
        (T.function (T.list $ T.var "t0") (T.map (T.var "t0") (T.set $ T.var "t0")))]]

module Hydra.Sources.Tier3.Test.Inference.Simple (simpleTermsTests) where

import Hydra.Kernel
import Hydra.Testing
import qualified Hydra.Dsl.Base as Base
import qualified Hydra.Dsl.Core as Core
import Hydra.Dsl.Testing as Testing
import Hydra.Dsl.ShorthandTypes
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Lib.Maps as Maps
import qualified Hydra.Dsl.Types as Types
import Hydra.Sources.Tier3.Test.TestGraph
import Hydra.Dsl.TTerms as TTerms
import qualified Hydra.Dsl.TTypes as T
import Hydra.Sources.Tier3.Test.Inference.Fundamentals

import qualified Data.Map as M
import Prelude hiding (map, product, sum)


simpleTermsTests :: TTerm TestGroup
simpleTermsTests = supergroup "Simple terms" [
  testGroupForApplicationTerms,
  testGroupForFunctionTerms,
  testGroupForIndividualTerms,
  testGroupForLetTerms,
  testGroupForListTerms,
  testGroupForPrimitiveTerms,
  testGroupForProductTerms,
  testGroupForSumTerms]

testGroupForApplicationTerms :: TTerm TestGroup
testGroupForApplicationTerms = subgroup "Application terms" [
    expectMono 1 []
      ((lambda "x" $ var "x") @@ string "foo")
      T.string,
    expectMono 2 []
      (lambda "x" $ primitive _math_sub @@ (primitive _math_add @@ var "x" @@ var "x") @@ int32 1)
      (T.function T.int32 T.int32)]

testGroupForFunctionTerms :: TTerm TestGroup
testGroupForFunctionTerms = supergroup "Function terms" [

    subgroup "Lambdas" [
      expectPoly 1 []
        (lambda "x" $ var "x")
        ["t0"] (T.function (T.var "t0") (T.var "t0")),
      expectPoly 2 []
        (lambda "x" $ int16 137)
        ["t0"] (T.function (T.var "t0") T.int16)],

    subgroup "List eliminations" [
      expectMono 1 [tag_disabledForMinimalInference]
        foldAdd
        (T.functionN [T.int32, T.list T.int32, T.int32]),
      expectMono 2 [tag_disabledForMinimalInference]
        (apply foldAdd $ int32 0)
        (T.function (T.list T.int32) T.int32),
      expectMono 3 [tag_disabledForMinimalInference]
        (apply (apply foldAdd $ int32 0) (list (int32 <$> [1, 2, 3, 4, 5])))
        T.int32],

    subgroup "Optional eliminations" [
      expectMono 1 [tag_disabledForMinimalInference]
        (matchOpt (int32 42) (primitive _math_neg))
        (T.function (T.optional T.int32) T.int32),
      expectMono 2 [tag_disabledForMinimalInference]
        (matchOpt (int32 42) (primitive _math_neg) @@ optional (just $ int32 137))
        T.int32,
      expectMono 3 [tag_disabledForMinimalInference]
        (matchOpt (int32 42) (primitive _math_neg) @@ optional nothing)
        T.int32,
      expectPoly 4 [tag_disabledForMinimalInference]
        (lambda "x" $ matchOpt (var "x") (primitive _optionals_pure) @@ var "x")
        ["t0"] (T.function (T.optional $ T.var "t0") (T.optional $ T.var "t0")),
      expectPoly 5 [tag_disabledForMinimalInference]
        (matchOpt (list []) (lambda "x" $ list [var "x"]))
        ["t0"] (T.function (T.optional $ T.var "t0") (T.list $ T.var "t0"))],

   subgroup "Tuple projections" [
     expectPoly 1 [tag_disabledForMinimalInference]
       (untuple 2 0)
       ["t0", "t1"] (T.function (T.product [T.var "t0", T.var "t1"]) (T.var "t0")),
     expectMono 2 [tag_disabledForMinimalInference]
       (untuple 2 1 @@ pair (int32 42) (string "foo"))
       T.string,
     expectPoly 3 [tag_disabledForMinimalInference]
       (lambda "x" $ untuple 1 0 @@ tuple [var "x"])
       ["t0"] (T.function (T.var "t0") (T.var "t0")),
     expectPoly 4 [tag_disabledForMinimalInference]
       (lambda "x" $ untuple 3 2 @@ tuple [var "x", var "x", int32 42])
       ["t0"] (T.function (T.var "t0") T.int32)]]

  where
    foldAdd = fold $ primitive _math_add

testGroupForIndividualTerms :: TTerm TestGroup
testGroupForIndividualTerms = supergroup "Individual terms" [

    subgroup "Literal values" [
      expectMono 1 []
        (int32 42)
        T.int32,
      expectMono 2 []
        (string "foo")
        T.string,
      expectMono 3 []
        false
        T.boolean,
      expectMono 4 []
        (float64 42.0)
        T.float64],

    subgroup "Let terms" [
      expectPoly 1 []
        (let1 "x" (float32 42.0) $ lambdas ["y", "z"] $ var "x")
        ["t0", "t1"] (T.function (T.var "t0") (T.function (T.var "t1") T.float32)),
      -- Example from https://www.cs.cornell.edu/courses/cs6110/2017sp/lectures/lec23.pdf
      expectMono 2 []
        (lets [
            "square">: lambda "z" $ primitive _math_mul @@ var "z" @@ var "z"] $
          lambdas ["f", "x", "y"] $ primitive _logic_ifElse
              @@ (var "f" @@ (var "square" @@ var "x") @@ var "y")
              @@ (var "f" @@ var "x" @@ (var "f" @@ var "x" @@ var "y"))
              @@ (var "f" @@ var "x" @@ var "y"))
        (T.functionN [
          T.functionN [T.int32, T.boolean, T.boolean], T.int32, T.boolean, T.boolean])],

    subgroup "Optionals" [
      expectMono 1 [tag_disabledForMinimalInference]
        (optional $ just $ int32 42)
        (T.optional T.int32),
      expectPoly 2 [tag_disabledForMinimalInference]
        (optional nothing)
        ["t0"] (T.optional $ T.var "t0")],

    subgroup "Products" [
      expectMono 1 []
        (product [])
        (T.product []),
      expectMono 2 []
        (pair (int32 42) (string "foo"))
        (T.product [T.int32, T.string])],

    subgroup "Sets" [
      expectMono 1 [tag_disabledForMinimalInference]
        (set [true])
        (T.set T.boolean),
      expectPoly 2 [tag_disabledForMinimalInference]
        (set [set []])
        ["t0"] (T.set $ T.set $ T.var "t0")],

    subgroup "Maps" [
      expectMono 1 [tag_disabledForMinimalInference]
        (mapTermCheat [
          (Terms.string "firstName", Terms.string "Arthur"),
          (Terms.string "lastName", Terms.string "Dent")])
        (T.map T.string T.string),
      expectPoly 2 [tag_disabledForMinimalInference]
        (mapTerm Maps.empty)
        ["t0", "t1"] (T.map (T.var "t0") (T.var "t1")),
      expectPoly 3 [tag_disabledForMinimalInference]
        (lambdas ["x", "y"] $ mapTermCheat
          [(Terms.var "x", Terms.float64 0.1), (Terms.var "y", Terms.float64 0.2)])
        ["t0"] (T.function (T.var "t0") (T.function (T.var "t0") (T.map (T.var "t0") T.float64)))]]

--     -- TODO: add a case for a recursive nominal type -- e.g. MyList := () + (int, Mylist)
--     subgroup "Nominal (newtype) terms" [
--       expectMono []
--         testDataArthur
--         (T.wrap "Person")
--       expectMono []
--         (lambda "x" (record [
--           Field "firstName" $ var "x",
--           Field "lastName" $ var "x",
--           Field "age" $ int32 42]))
--         (T.function T.string testTypePerson)

testGroupForLetTerms :: TTerm TestGroup
testGroupForLetTerms = supergroup "Let terms" [

    subgroup "Empty let" [
      expectMono 1 []
        (lets [] $ int32 42)
        T.int32],

    subgroup "Trivial let" [
      expectMono 2 []
        (lets [
            "foo">: int32 42]
          $ var "foo")
        T.int32]]

testGroupForListTerms :: TTerm TestGroup
testGroupForListTerms = supergroup "List terms" [

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
    subgroup "Lambda producing a list of integers" [
      expectMono 1 []
        (lambda "x" (list [var "x", int32 42]))
        (T.function T.int32 $ T.list T.int32)],
    subgroup "List with bound variables" [
      expectMono 1 []
        (lambda "x" (list [var "x", string "foo", var "x"]))
        (T.function T.string (T.list T.string))]]

testGroupForPrimitiveTerms :: TTerm TestGroup
testGroupForPrimitiveTerms = supergroup "Primitive terms" [

    subgroup "Monomorphic primitive functions" [
      expectMono 1 []
        (primitive $ Name "hydra.lib.strings.length")
        (T.function T.string T.int32),
      expectMono 2 []
        (primitive _math_sub)
        (T.function T.int32 (T.function T.int32 T.int32))],
    subgroup "Polymorphic primitive functions" [
      expectPoly 1 []
        (lambda "els" (apply (primitive _lists_length) (apply (primitive _lists_concat) $ var "els")))
        ["t0"] (T.function (T.list $ T.list $ T.var "t0") T.int32)]]

testGroupForProductTerms :: TTerm TestGroup
testGroupForProductTerms = supergroup "Product terms" [

    subgroup "Empty product" [
      expectMono 1 []
        (product [])
        (T.product [])],
    subgroup "Non-empty monotyped products" [
      expectMono 1 []
        (product [string "foo", int32 42])
        (T.product [T.string, T.int32]),
      expectMono 2 []
        (product [string "foo", list [float32 42.0, float32 137.0]])
        (T.product [T.string, T.list T.float32])],
    subgroup "Polytyped products" [
      expectPoly 1 []
        (product [list [], string "foo"])
        ["t0"] (T.product [T.list $ T.var "t0", T.string])]]

testGroupForSumTerms :: TTerm TestGroup
testGroupForSumTerms = supergroup "Sum terms" [

    subgroup "Singleton sum terms" [
      expectMono 1 [tag_disabledForMinimalInference]
        (sum 0 1 $ string "foo")
        (T.sum [T.string]),
      expectPoly 2 [tag_disabledForMinimalInference]
        (sum 0 1 $ list [])
        ["t0"] (T.sum [T.list $ T.var "t0"])],
    subgroup "Non-singleton sum terms" [
      expectPoly 1 [tag_disabledForMinimalInference]
        (sum 0 2 $ string "foo")
        ["t0"] (T.sum [T.string, T.var "t0"]),
      expectPoly 2 [tag_disabledForMinimalInference]
        (sum 1 2 $ string "foo")
        ["t0"] (T.sum [T.var "t0", T.string])]]

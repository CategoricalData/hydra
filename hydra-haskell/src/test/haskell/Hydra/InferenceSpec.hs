-- Additional inference tests, adding to those in the generated test suite

{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.InferenceSpec.spec
-}

module Hydra.InferenceSpec where

import Hydra.Kernel
import Hydra.TestUtils
import Hydra.Staging.TestGraph
import Hydra.Tools.Monads
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


spec :: H.Spec
spec = do
  checkTypeOf
  checkTypeOfFailsOnUntypedTerms

----------------------------------------

checkTypeOf :: H.SpecWith ()
checkTypeOf = H.describe "typeOf" $ do
  H.describe "Literals" $ do
    H.describe "Integers" $ do
      expectTypeOf "#1"
        (Terms.int32 42)
        Types.int32
      expectTypeOf "#2"
        (Terms.bigint 42)
        Types.bigint
    H.describe "Strings" $ do
      expectTypeOf "#3"
        (Terms.string "foo")
        Types.string

  H.describe "Lists" $ do
    H.describe "Lists of literals" $ do
      expectTypeOf "#1"
        (Terms.list [Terms.int32 1, Terms.int32 2])
        (Types.list Types.int32)
    H.describe "Empty lists" $ do
      expectTypeOf "#1"
        (Terms.list [])
        (Types.forAll "t0" $ Types.list $ Types.var "t0")
      expectTypeOf "#2"
        (Terms.pair (Terms.list []) (Terms.list []))
        (Types.forAlls ["t1", "t0"] $ Types.pair (Types.list $ Types.var "t0") (Types.list $ Types.var "t1"))

  H.describe "Algebraic types" $ do
    H.describe "Products" $ do
      H.describe "Monomorphic products" $ do
        expectTypeOf "#1"
          (Terms.tuple [])
          (Types.product [])
        expectTypeOf "#2"
          (Terms.tuple [Terms.int32 42])
          (Types.product [Types.int32])
        expectTypeOf "#3"
          (Terms.tuple [Terms.int32 42, Terms.string "foo"])
          (Types.product [Types.int32, Types.string])
      H.describe "Polymorphic products" $ do
        expectTypeOf "#1"
          (Terms.lambda "x" $ Terms.tuple [Terms.var "x", Terms.string "foo"])
          (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.product [Types.var "t0", Types.string]))

  H.describe "Nominally-typed terms" $ do
    H.describe "Records" $ do
      H.describe "Monomorphic records" $ do
        expectTypeOf "#1"
          (Terms.record testTypeLatLonName [
            Terms.field "lat" (Terms.float32 19.5429),
            Terms.field "lon" (Terms.float32 (0-155.6659))])
          (Types.var "LatLon")
        expectTypeOf "#2"
          (Terms.lambda "x" (Terms.record testTypeLatLonName [
            Terms.field "lat" (Terms.float32 19.5429),
            Terms.field "lon" (Terms.var "x")]))
          (Types.function Types.float32 (Types.var "LatLon"))
      H.describe "Polymorphic records" $ do
        expectTypeOf "#1"
          (Terms.record testTypeLatLonPolyName [
            Terms.field "lat" (Terms.float32 19.5429),
            Terms.field "lon" (Terms.float32 (0-155.6659))])
          (Types.apply (Types.var "LatLonPoly") Types.float32)
        expectTypeOf "#2"
          (Terms.record testTypeLatLonPolyName [
            Terms.field "lat" (Terms.int64 195429),
            Terms.field "lon" (Terms.int64 (0-1556659))])
          (Types.apply (Types.var "LatLonPoly") Types.int64)

checkTypeOfFailsOnUntypedTerms :: H.SpecWith ()
checkTypeOfFailsOnUntypedTerms = H.describe "Fail on untyped terms" $ do
  H.describe "Untyped lambdas" $ do
    withDefaults typeOfShouldFail "#1"
      (Terms.lambda "x" (Terms.record testTypeLatLonName [
        Terms.field "lat" (Terms.float32 19.5429),
        Terms.field "lon" (Terms.var "x")]))

expectTypeOf :: String -> Term -> Type -> H.SpecWith ()
expectTypeOf desc term typ = H.it desc $ withDefaults expectTypeOfResult desc term typ

typeOfShouldFail :: String -> M.Map Name Type -> Term -> H.SpecWith ()
typeOfShouldFail desc types term = H.it desc $ shouldFail $ do
  cx <- graphToInferenceContext testGraph
  typeOf cx S.empty types term

withDefaults :: (String -> M.Map Name Type -> Term -> x) -> String -> Term -> x
withDefaults f desc = f desc M.empty

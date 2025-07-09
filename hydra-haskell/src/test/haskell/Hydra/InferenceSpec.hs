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
  checkFailTypeOfOnUntypedTerms

----------------------------------------

checkFailTypeOfOnUntypedTerms :: H.SpecWith ()
checkFailTypeOfOnUntypedTerms = H.describe "Fail on untyped (pre-inference) terms" $ do
  H.describe "Untyped lambdas" $ do
    withDefaults typeOfShouldFail "#1"
      (Terms.lambda "x" (Terms.record testTypeLatLonName [
        Terms.field "lat" (Terms.float32 19.5429),
        Terms.field "lon" (Terms.var "x")]))

----------------------------------------

checkTypeOf :: H.SpecWith ()
checkTypeOf = H.describe "typeOf" $ do
  checkTypeOfAnnotatedTerms
  checkTypeOfApplications
  checkTypeOfFunctions
  checkTypeOfLetTerms
  checkTypeOfLists
  checkTypeOfLiterals
  checkTypeOfMaps
  checkTypeOfOptionals
  checkTypeOfProducts
  checkTypeOfRecords
  checkTypeOfSets
  checkTypeOfSums
  checkTypeOfTypeAbstractions
  checkTypeOfTypeApplications
  checkTypeOfUnions
  checkTypeOfUnit
  checkTypeOfVariables
  checkTypeOfWrappedTerms

checkTypeOfAnnotatedTerms :: H.SpecWith ()
checkTypeOfAnnotatedTerms = H.describe "Annotated terms" $ do
  return ()  -- TODO: implement

checkTypeOfApplications :: H.SpecWith ()
checkTypeOfApplications = H.describe "Applications" $ do
  return ()  -- TODO: implement

checkTypeOfFunctions :: H.SpecWith ()
checkTypeOfFunctions = H.describe "Functions" $ do
  return ()  -- TODO: implement

checkTypeOfLetTerms :: H.SpecWith ()
checkTypeOfLetTerms = H.describe "Let terms" $ do
  return ()  -- TODO: implement

checkTypeOfLists :: H.SpecWith ()
checkTypeOfLists = H.describe "Lists" $ do
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

checkTypeOfLiterals :: H.SpecWith ()
checkTypeOfLiterals = H.describe "Literals" $ do
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

checkTypeOfMaps :: H.SpecWith ()
checkTypeOfMaps = H.describe "Maps" $ do
  return ()  -- TODO: implement

checkTypeOfOptionals :: H.SpecWith ()
checkTypeOfOptionals = H.describe "Optionals" $ do
  return ()  -- TODO: implement

checkTypeOfProducts :: H.SpecWith ()
checkTypeOfProducts = H.describe "Products" $ do
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
    expectTypeOf "#4"
      (Terms.tuple [Terms.int32 1, Terms.int32 2, Terms.int32 3])
      (Types.product [Types.int32, Types.int32, Types.int32])
    expectTypeOf "#5"
      (Terms.tuple [Terms.unit, Terms.string "test", Terms.bigint 100])
      (Types.product [Types.unit, Types.string, Types.bigint])
  H.describe "Polymorphic products" $ do
    expectTypeOf "#1"
      (Terms.lambda "x" $ Terms.tuple [Terms.var "x", Terms.string "foo"])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.product [Types.var "t0", Types.string]))
    expectTypeOf "#2"
      (Terms.lambda "x" $ Terms.lambda "y" $ Terms.tuple [Terms.var "x", Terms.var "y"])
      (Types.forAlls ["t1", "t0"] $ Types.function (Types.var "t0") (Types.function (Types.var "t1") (Types.product [Types.var "t0", Types.var "t1"])))
    expectTypeOf "#3"
      (Terms.lambda "x" $ Terms.tuple [Terms.var "x", Terms.var "x"])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.product [Types.var "t0", Types.var "t0"]))
  H.describe "Nested products" $ do
    expectTypeOf "#1"
      (Terms.tuple [Terms.tuple [Terms.int32 1], Terms.string "foo"])
      (Types.product [Types.product [Types.int32], Types.string])
    expectTypeOf "#2"
      (Terms.lambda "x" $ Terms.tuple [Terms.tuple [Terms.var "x"], Terms.tuple [Terms.string "test"]])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.product [Types.product [Types.var "t0"], Types.product [Types.string]]))

checkTypeOfRecords :: H.SpecWith ()
checkTypeOfRecords = H.describe "Records" $ do
  H.describe "Monomorphic records" $ do
    expectTypeOf "#1"
      (Terms.record testTypeLatLonName [
        Terms.field "lat" (Terms.float32 19.5429),
        Terms.field "lon" (Terms.float32 (0-155.6659))])
      (Types.var "LatLon")
    expectTypeOf "#2"
      (Terms.lambda "x" $ Terms.record testTypeLatLonName [
        Terms.field "lat" (Terms.float32 19.5429),
        Terms.field "lon" (Terms.var "x")])
      (Types.function Types.float32 (Types.var "LatLon"))
    expectTypeOf "#3"
      (Terms.record testTypePersonName [
        Terms.field "firstName" (Terms.string "Alice"),
        Terms.field "lastName" (Terms.string "Smith"),
        Terms.field "age" (Terms.int32 30)])
      (Types.var "Person")
    expectTypeOf "#4"
      (Terms.record testTypeUnitName [])
      (Types.var "Unit")
    expectTypeOf "#5"
      (Terms.lambda "name" $ Terms.lambda "age" $ Terms.record testTypePersonName [
        Terms.field "firstName" (Terms.var "name"),
        Terms.field "lastName" (Terms.string "Doe"),
        Terms.field "age" (Terms.var "age")])
      (Types.function Types.string (Types.function Types.int32 (Types.var "Person")))

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
    expectTypeOf "#3"
      (Terms.lambda "x" $ Terms.record testTypeLatLonPolyName [
        Terms.field "lat" (Terms.var "x"),
        Terms.field "lon" (Terms.var "x")])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.apply (Types.var "LatLonPoly") (Types.var "t0")))
    expectTypeOf "#4"
      (Terms.record testTypeBuddyListAName [
        Terms.field "head" (Terms.string "first"),
        Terms.field "tail" (Terms.optional Nothing)])
      (Types.apply (Types.var "BuddyListA") Types.string)
    expectTypeOf "#5"
      (Terms.lambda "x" $ Terms.record testTypeBuddyListAName [
        Terms.field "head" (Terms.var "x"),
        Terms.field "tail" (Terms.optional Nothing)])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.apply (Types.var "BuddyListA") (Types.var "t0")))

  H.describe "Records in complex contexts" $ do
    expectTypeOf "#1"
      (Terms.tuple [
        Terms.record testTypePersonName [
          Terms.field "firstName" (Terms.string "Bob"),
          Terms.field "lastName" (Terms.string "Jones"),
          Terms.field "age" (Terms.int32 25)],
        Terms.record testTypeLatLonName [
          Terms.field "lat" (Terms.float32 1.0),
          Terms.field "lon" (Terms.float32 2.0)]])
      (Types.product [Types.var "Person", Types.var "LatLon"])
    expectTypeOf "#2"
      (Terms.tuple [
        Terms.record testTypeLatLonPolyName [
          Terms.field "lat" (Terms.int32 1),
          Terms.field "lon" (Terms.int32 2)],
        Terms.record testTypeBuddyListAName [
          Terms.field "head" (Terms.string "test"),
          Terms.field "tail" (Terms.optional Nothing)]])
      (Types.product [
        Types.apply (Types.var "LatLonPoly") Types.int32,
        Types.apply (Types.var "BuddyListA") Types.string])
    expectTypeOf "#3"
      (Terms.record testTypeIntListName [
        Terms.field "head" (Terms.int32 42),
        Terms.field "tail" (Terms.optional $ Just $
          Terms.record testTypeIntListName [
            Terms.field "head" (Terms.int32 43),
            Terms.field "tail" (Terms.optional Nothing)])])
      (Types.var "IntList")

checkTypeOfSets :: H.SpecWith ()
checkTypeOfSets = H.describe "Sets" $ do
  return ()  -- TODO: implement

checkTypeOfSums :: H.SpecWith ()
checkTypeOfSums = H.describe "Sums" $ do
  return ()  -- TODO: implement

checkTypeOfTypeAbstractions :: H.SpecWith ()
checkTypeOfTypeAbstractions = H.describe "Type abstractions" $ do
  return ()  -- TODO: implement

checkTypeOfTypeApplications :: H.SpecWith ()
checkTypeOfTypeApplications = H.describe "Type applications" $ do
  return ()  -- TODO: implement

checkTypeOfUnions :: H.SpecWith ()
checkTypeOfUnions = H.describe "Unions" $ do
  return ()  -- TODO: implement

checkTypeOfUnit :: H.SpecWith ()
checkTypeOfUnit = H.describe "Unit" $ do
  H.describe "Unit term" $ do
    expectTypeOf "#1"
      Terms.unit
      Types.unit
  H.describe "Unit term in polymorphic context" $ do
    expectTypeOf "#1"
      (Terms.lambda "x" Terms.unit)
      (Types.forAll "t0" $ Types.function (Types.var "t0") Types.unit)
    expectTypeOf "#2"
      (Terms.tuple [Terms.unit, Terms.string "foo"])
      (Types.product [Types.unit, Types.string])

checkTypeOfVariables :: H.SpecWith ()
checkTypeOfVariables = H.describe "Variables" $ do
  return ()  -- TODO: implement
  
checkTypeOfWrappedTerms :: H.SpecWith ()
checkTypeOfWrappedTerms = H.describe "Wrapped terms" $ do
  return ()  -- TODO: implement
  
----------------------------------------

expectTypeOf :: String -> Term -> Type -> H.SpecWith ()
expectTypeOf desc term typ = H.it desc $ withDefaults expectTypeOfResult desc term typ

typeOfShouldFail :: String -> M.Map Name Type -> Term -> H.SpecWith ()
typeOfShouldFail desc types term = H.it desc $ shouldFail $ do
  cx <- graphToInferenceContext testGraph
  typeOf cx S.empty types term

withDefaults :: (String -> M.Map Name Type -> Term -> x) -> String -> Term -> x
withDefaults f desc = f desc M.empty

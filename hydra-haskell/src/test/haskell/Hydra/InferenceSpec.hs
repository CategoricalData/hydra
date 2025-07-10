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
import           Hydra.Dsl.Terms as Terms
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
    withDefaults typeOfShouldFail "untyped var in record"
      (lambda "x" (record testTypeLatLonName [
        field "lat" (float32 19.5429),
        field "lon" (var "x")]))

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
  H.describe "Simple let bindings" $ do
    expectTypeOf "single binding"
      (lets ["x">: int32 42] $
            var "x")
      Types.int32
    expectTypeOf "multiple bindings"
      (lets ["x">: int32 42,
             "y">: string "hello"] $
            tuple [var "x", var "y"])
      (Types.product [Types.int32, Types.string])
    expectTypeOf "binding shadowing"
      (lets ["x">: int32 1] $
       lets ["x">: string "shadow"] $
            var "x")
      Types.string

  H.describe "Recursive bindings" $ do
    expectTypeOf "simple arithmetic recursion"
      (lets ["double">: lambda "n" $ primitive _math_add @@ var "n" @@ var "n"] $
            var "double" @@ int32 5)
      Types.int32

  H.describe "Mutual recursion" $ do
    expectTypeOf "mutually recursive data"
      (lets ["listA">: record testTypeBuddyListAName [
               field "head" (int32 1),
               field "tail" (optional $ Just $ var "listB")],
             "listB">: record testTypeBuddyListBName [
               field "head" (int32 2),
               field "tail" (optional Nothing)]] $
            var "listA")
      (Types.apply (Types.var "BuddyListA") Types.int32)
    expectTypeOf "(monomorphic) mutually recursive functions"
      (lets ["f">: lambda "x" $ var "g" @@ var "x",
             "g">: lambda "y" $ primitive _math_add @@ var "y" @@ int32 1] $
            var "f" @@ int32 5)
      Types.int32

  H.describe "Nested let terms" $ do
    expectTypeOf "monomorphic nesting"
      (lets ["x">: int32 1] $
       lets ["y">: primitive _math_add @@ var "x" @@ int32 2] $
       lets ["z">: primitive _math_mul @@ var "y" @@ int32 3] $
            var "z")
      Types.int32
    expectTypeOf "polymorphic nesting"
      (lets ["id">: lambda "x" $ var "x"] $
       lets ["apply">: lambda "f" $ lambda "x" $ var "f" @@ var "x"] $
            var "apply" @@ var "id" @@ string "test")
      Types.string
    expectTypeOf "variable capture avoidance"
      (lets ["x">: int32 1] $
            lambda "x" $ lets ["y">: var "x"] $
                              var "y")
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.var "t0"))

  H.describe "Let with complex expressions" $ do
    expectTypeOf "let in record"
      (record testTypePersonName [
        field "firstName" (lets ["first">: string "John",
                                "middle">: string "Q"] $
                               primitive _strings_cat2 @@ var "first" @@ var "middle"),
        field "lastName" (string "Doe"),
        field "age" (int32 30)])
      (Types.var "Person")
    expectTypeOf "let in function application"
      (lets ["x">: int32 5,
             "y">: int32 3] $
            primitive _math_add @@ var "x" @@ var "y")
      Types.int32
    expectTypeOf "polymorphic let binding"
      (lets ["id">: lambda "x" $ var "x"] $
            tuple [var "id" @@ int32 42, var "id" @@ string "hello"])
      (Types.product [Types.int32, Types.string])
    expectTypeOf "composition"
      (lets ["compose">: lambda "f" $ lambda "g" $ lambda "x" $ var "f" @@ (var "g" @@ var "x"),
             "add1">: lambda "n" $ primitive _math_add @@ var "n" @@ int32 1,
             "double">: lambda "n" $ primitive _math_mul @@ var "n" @@ int32 2] $
            (var "compose" @@ var "add1" @@ var "double") @@ int32 5)
      Types.int32

checkTypeOfLists :: H.SpecWith ()
checkTypeOfLists = H.describe "Lists" $ do
  H.describe "Lists of literals" $ do
    expectTypeOf "int list"
      (list [int32 1, int32 2])
      (Types.list Types.int32)
    expectTypeOf "string list"
      (list [string "hello", string "world"])
      (Types.list Types.string)
    expectTypeOf "single element list"
      (list [bigint 42])
      (Types.list Types.bigint)
    expectTypeOf "mixed numeric types"
      (list [float32 1.0, float32 2.5, float32 3.14])
      (Types.list Types.float32)
  H.describe "Empty lists" $ do
    expectTypeOf "empty list"
      (list [])
      (Types.forAll "t0" $ Types.list $ Types.var "t0")
    expectTypeOf "pair of empty lists"
      (pair (list []) (list []))
      (Types.forAlls ["t1", "t0"] $ Types.pair (Types.list $ Types.var "t0") (Types.list $ Types.var "t1"))
    expectTypeOf "empty list in tuple"
      (tuple [list [], string "context"])
      (Types.forAll "t0" $ Types.product [Types.list $ Types.var "t0", Types.string])
  H.describe "Polymorphic lists" $ do
    expectTypeOf "list from lambda"
      (lambda "x" $ list [var "x"])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.list $ Types.var "t0"))
    expectTypeOf "list with repeated var"
      (lambda "x" $ list [var "x", var "x"])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.list $ Types.var "t0"))
    expectTypeOf "list from two lambdas"
      (lambda "x" $ lambda "y" $ list [var "x", var "y"])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.function (Types.var "t0") (Types.list $ Types.var "t0")))
  H.describe "Nested lists" $ do
    expectTypeOf "list of lists"
      (list [list [int32 1], list [int32 2, int32 3]])
      (Types.list $ Types.list Types.int32)
    expectTypeOf "empty nested lists"
      (list [list [], list []])
      (Types.forAll "t0" $ Types.list $ Types.list $ Types.var "t0")
    expectTypeOf "nested polymorphic"
      (lambda "x" $ list [list [var "x"]])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.list $ Types.list $ Types.var "t0"))
  H.describe "Lists in complex contexts" $ do
    expectTypeOf "multiple lists in tuple"
      (tuple [
        list [int32 1, int32 2],
        list [string "a", string "b"]])
      (Types.product [Types.list Types.int32, Types.list Types.string])

checkTypeOfLiterals :: H.SpecWith ()
checkTypeOfLiterals = H.describe "Literals" $ do
  H.describe "Boolean literals" $ do
    expectTypeOf "true"
      (boolean True)
      Types.boolean
    expectTypeOf "false"
      (boolean False)
      Types.boolean

  H.describe "String literals" $ do
    expectTypeOf "simple string"
      (string "hello")
      Types.string
    expectTypeOf "empty string"
      (string "")
      Types.string
    expectTypeOf "unicode string"
      (string "café")
      Types.string

  H.describe "Integer literals" $ do
    expectTypeOf "bigint"
      (bigint 42)
      Types.bigint
    expectTypeOf "int8"
      (int8 127)
      Types.int8
    expectTypeOf "int16"
      (int16 32767)
      Types.int16
    expectTypeOf "int32"
      (int32 2147483647)
      Types.int32
    expectTypeOf "int64"
      (int64 9223372036854775807)
      Types.int64
    expectTypeOf "uint8"
      (uint8 255)
      Types.uint8
    expectTypeOf "uint16"
      (uint16 65535)
      Types.uint16
    expectTypeOf "uint32"
      (uint32 4294967295)
      Types.uint32
    expectTypeOf "uint64"
      (uint64 18446744073709551615)
      Types.uint64

  H.describe "Float literals" $ do
    expectTypeOf "bigfloat"
      (bigfloat 3.14159)
      Types.bigfloat
    expectTypeOf "float32"
      (float32 2.71828)
      Types.float32
    expectTypeOf "float64"
      (float64 1.41421)
      Types.float64

  H.describe "Binary literals" $ do
    expectTypeOf "binary"
      (binary "SGVsbG8gV29ybGQ=")  -- "Hello World" in base64
      Types.binary

  H.describe "Literals in complex contexts" $ do
    expectTypeOf "literals in tuple"
      (tuple [boolean True, string "test", int32 42, float32 3.14])
      (Types.product [Types.boolean, Types.string, Types.int32, Types.float32])
    expectTypeOf "literals in list"
      (list [string "one", string "two", string "three"])
      (Types.list Types.string)
    expectTypeOf "literals in record"
      (record testTypePersonName [
        field "firstName" (string "Alice"),
        field "lastName" (string "Smith"),
        field "age" (int32 30)])
      (Types.var "Person")
    expectTypeOf "literals in let binding"
      (lets ["x">: int32 100,
             "y">: string "hello",
             "z">: boolean True] $
            tuple [var "x", var "y", var "z"])
      (Types.product [Types.int32, Types.string, Types.boolean])

checkTypeOfMaps :: H.SpecWith ()
checkTypeOfMaps = H.describe "Maps" $ do
  H.describe "Monomorphic maps" $ do
    expectTypeOf "empty map"
      (Terms.map M.empty)
      (Types.forAlls ["t1", "t0"] $ Types.map (Types.var "t0") (Types.var "t1"))
    expectTypeOf "int to string map"
      (Terms.map $ M.fromList [(int32 1, string "one"),
                               (int32 2, string "two")])
      (Types.map Types.int32 Types.string)
    expectTypeOf "string to int map"
      (Terms.map $ M.fromList [(string "a", int32 1),
                               (string "b", int32 2)])
      (Types.map Types.string Types.int32)
    expectTypeOf "single entry map"
      (Terms.map $ M.singleton (bigint 42) (boolean True))
      (Types.map Types.bigint Types.boolean)

  H.describe "Polymorphic maps" $ do
    expectTypeOf "map from lambda keys"
      (lambda "k" $ Terms.map $ M.singleton (var "k") (string "value"))
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.map (Types.var "t0") Types.string))
    expectTypeOf "map from lambda values"
      (lambda "v" $ Terms.map $ M.singleton (string "key") (var "v"))
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.map Types.string (Types.var "t0")))
    expectTypeOf "map from lambda both"
      (lambda "k" $ lambda "v" $ Terms.map $ M.singleton (var "k") (var "v"))
      (Types.forAlls ["t1", "t0"] $ Types.function (Types.var "t0") (Types.function (Types.var "t1") (Types.map (Types.var "t0") (Types.var "t1"))))
    expectTypeOf "map with repeated variables"
      (lambda "x" $ Terms.map $ M.singleton (var "x") (var "x"))
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.map (Types.var "t0") (Types.var "t0")))

  H.describe "Maps in complex contexts" $ do
    expectTypeOf "map in tuple"
      (tuple [Terms.map $ M.singleton (int32 1) (string "one"),
              string "context"])
      (Types.product [Types.map Types.int32 Types.string, Types.string])
    expectTypeOf "nested maps"
      (Terms.map $ M.singleton (string "outer") (Terms.map $ M.singleton (int32 1) (boolean True)))
      (Types.map Types.string (Types.map Types.int32 Types.boolean))
    expectTypeOf "map in let binding"
      (lets ["lookup">: Terms.map $ M.fromList [(string "key1", int32 100),
                                                (string "key2", int32 200)]] $
            var "lookup")
      (Types.map Types.string Types.int32)

  H.describe "Maps with complex types" $ do
    expectTypeOf "map of records"
      (Terms.map $ M.singleton (string "person1")
                     (record testTypePersonName [
                       field "firstName" (string "Alice"),
                       field "lastName" (string "Smith"),
                       field "age" (int32 25)]))
      (Types.map Types.string (Types.var "Person"))
    expectTypeOf "map of lists"
      (Terms.map $ M.fromList [(int32 1, list [string "a", string "b"]),
                               (int32 2, list [string "c", string "d"])])
      (Types.map Types.int32 (Types.list Types.string))
    expectTypeOf "map of tuples"
      (Terms.map $ M.singleton (string "coords") (tuple [int32 10, int32 20]))
      (Types.map Types.string (Types.product [Types.int32, Types.int32]))

checkTypeOfOptionals :: H.SpecWith ()
checkTypeOfOptionals = H.describe "Optionals" $ do
  return ()  -- TODO: implement

checkTypeOfProducts :: H.SpecWith ()
checkTypeOfProducts = H.describe "Products" $ do
  H.describe "Monomorphic products" $ do
    expectTypeOf "empty tuple"
      (tuple [])
      (Types.product [])
    expectTypeOf "singleton tuple"
      (tuple [int32 42])
      (Types.product [Types.int32])
    expectTypeOf "pair tuple"
      (tuple [int32 42, string "foo"])
      (Types.product [Types.int32, Types.string])
    expectTypeOf "triple tuple"
      (tuple [int32 1, int32 2, int32 3])
      (Types.product [Types.int32, Types.int32, Types.int32])
    expectTypeOf "mixed types"
      (tuple [unit, string "test", bigint 100])
      (Types.product [Types.unit, Types.string, Types.bigint])
  H.describe "Polymorphic products" $ do
    expectTypeOf "lambda with var"
      (lambda "x" $ tuple [var "x", string "foo"])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.product [Types.var "t0", Types.string]))
    expectTypeOf "two variables"
      (lambda "x" $ lambda "y" $ tuple [var "x", var "y"])
      (Types.forAlls ["t1", "t0"] $ Types.function (Types.var "t0") (Types.function (Types.var "t1") (Types.product [Types.var "t0", Types.var "t1"])))
    expectTypeOf "repeated variable"
      (lambda "x" $ tuple [var "x", var "x"])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.product [Types.var "t0", Types.var "t0"]))
  H.describe "Nested products" $ do
    expectTypeOf "tuple in tuple"
      (tuple [tuple [int32 1], string "foo"])
      (Types.product [Types.product [Types.int32], Types.string])
    expectTypeOf "nested polymorphic"
      (lambda "x" $ tuple [tuple [var "x"], tuple [string "test"]])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.product [Types.product [Types.var "t0"], Types.product [Types.string]]))

checkTypeOfRecords :: H.SpecWith ()
checkTypeOfRecords = H.describe "Records" $ do
  H.describe "Monomorphic records" $ do
    expectTypeOf "latlon record"
      (record testTypeLatLonName [
        field "lat" (float32 19.5429),
        field "lon" (float32 (0-155.6659))])
      (Types.var "LatLon")
    expectTypeOf "latlon with variable"
      (lambda "x" $ record testTypeLatLonName [
        field "lat" (float32 19.5429),
        field "lon" (var "x")])
      (Types.function Types.float32 (Types.var "LatLon"))
    expectTypeOf "person record"
      (record testTypePersonName [
        field "firstName" (string "Alice"),
        field "lastName" (string "Smith"),
        field "age" (int32 30)])
      (Types.var "Person")
    expectTypeOf "empty record"
      (record testTypeUnitName [])
      (Types.var "Unit")
    expectTypeOf "person with variables"
      (lambda "name" $ lambda "age" $ record testTypePersonName [
        field "firstName" (var "name"),
        field "lastName" (string "Doe"),
        field "age" (var "age")])
      (Types.function Types.string (Types.function Types.int32 (Types.var "Person")))

  H.describe "Polymorphic records" $ do
    expectTypeOf "latlon poly float"
      (record testTypeLatLonPolyName [
        field "lat" (float32 19.5429),
        field "lon" (float32 (0-155.6659))])
      (Types.apply (Types.var "LatLonPoly") Types.float32)
    expectTypeOf "latlon poly int64"
      (record testTypeLatLonPolyName [
        field "lat" (int64 195429),
        field "lon" (int64 (0-1556659))])
      (Types.apply (Types.var "LatLonPoly") Types.int64)
    expectTypeOf "latlon poly variable"
      (lambda "x" $ record testTypeLatLonPolyName [
        field "lat" (var "x"),
        field "lon" (var "x")])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.apply (Types.var "LatLonPoly") (Types.var "t0")))
    expectTypeOf "buddylist string"
      (record testTypeBuddyListAName [
        field "head" (string "first"),
        field "tail" (optional Nothing)])
      (Types.apply (Types.var "BuddyListA") Types.string)
    expectTypeOf "buddylist variable"
      (lambda "x" $ record testTypeBuddyListAName [
        field "head" (var "x"),
        field "tail" (optional Nothing)])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.apply (Types.var "BuddyListA") (Types.var "t0")))

  H.describe "Records in complex contexts" $ do
    expectTypeOf "records in tuple"
      (tuple [
        record testTypePersonName [
          field "firstName" (string "Bob"),
          field "lastName" (string "Jones"),
          field "age" (int32 25)],
        record testTypeLatLonName [
          field "lat" (float32 1.0),
          field "lon" (float32 2.0)]])
      (Types.product [Types.var "Person", Types.var "LatLon"])
    expectTypeOf "poly records in tuple"
      (tuple [
        record testTypeLatLonPolyName [
          field "lat" (int32 1),
          field "lon" (int32 2)],
        record testTypeBuddyListAName [
          field "head" (string "test"),
          field "tail" (optional Nothing)]])
      (Types.product [
        Types.apply (Types.var "LatLonPoly") Types.int32,
        Types.apply (Types.var "BuddyListA") Types.string])
    expectTypeOf "recursive record"
      (record testTypeIntListName [
        field "head" (int32 42),
        field "tail" (optional $ Just $
          record testTypeIntListName [
            field "head" (int32 43),
            field "tail" (optional Nothing)])])
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
    expectTypeOf "unit literal"
      unit
      Types.unit
  H.describe "Unit term in polymorphic context" $ do
    expectTypeOf "unit from lambda"
      (lambda "x" unit)
      (Types.forAll "t0" $ Types.function (Types.var "t0") Types.unit)
    expectTypeOf "unit in tuple"
      (tuple [unit, string "foo"])
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

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
  checkTypeOfUnions
  checkTypeOfUnit
  checkTypeOfVariables
  checkTypeOfWrappedTerms

checkTypeOfAnnotatedTerms :: H.SpecWith ()
checkTypeOfAnnotatedTerms = H.describe "Annotated terms" $ do
  H.describe "Top-level annotations" $ do
    expectTypeOf "annotated literal"
      (annotated (int32 42) M.empty)
      Types.int32
    expectTypeOf "annotated list"
      (annotated (list [string "a", string "b"]) M.empty)
      (Types.list Types.string)
    expectTypeOf "annotated record"
      (annotated (record testTypePersonName [
        field "firstName" (string "John"),
        field "lastName" (string "Doe"),
        field "age" (int32 25)]) M.empty)
      (Types.var "Person")
    expectTypeOf "annotated lambda"
      (annotated (lambda "x" $ var "x") M.empty)
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.var "t0"))

  H.describe "Nested annotations" $ do
    expectTypeOf "annotation within annotation"
      (annotated (annotated (int32 100) M.empty) M.empty)
      Types.int32
    expectTypeOf "annotated terms in tuple"
      (tuple [annotated (int32 1) M.empty,
              annotated (string "hello") M.empty])
      (Types.product [Types.int32, Types.string])
    expectTypeOf "annotated term in function application"
      (annotated (lambda "x" $ var "x") M.empty @@ annotated (int32 42) M.empty)
      Types.int32

  H.describe "Annotations in complex contexts" $ do
    expectTypeOf "annotated let binding"
      (lets ["x">: annotated (int32 5) M.empty,
             "y">: annotated (string "world") M.empty] $
            annotated (tuple [var "x", var "y"]) M.empty)
      (Types.product [Types.int32, Types.string])
    expectTypeOf "annotated record fields"
      (record testTypePersonName [
        field "firstName" (annotated (string "Alice") M.empty),
        field "lastName" (annotated (string "Smith") M.empty),
        field "age" (annotated (int32 30) M.empty)])
      (Types.var "Person")
    expectTypeOf "annotated function in application"
      (lets ["add">: annotated (primitive _math_add) M.empty] $
            var "add" @@ annotated (int32 10) M.empty @@ annotated (int32 20) M.empty)
      Types.int32

checkTypeOfApplications :: H.SpecWith ()
checkTypeOfApplications = H.describe "Applications" $ do
  H.describe "Simple function applications" $ do
    expectTypeOf "identity application"
      (lambda "x" (var "x") @@ int32 42)
      Types.int32
    expectTypeOf "primitive application"
      (primitive _math_add @@ int32 10 @@ int32 20)
      Types.int32
    expectTypeOf "string concatenation"
      (primitive _strings_cat2 @@ string "hello" @@ string "world")
      Types.string

  H.describe "Partial applications" $ do
    expectTypeOf "partially applied add"
      (primitive _math_add @@ int32 5)
      (Types.function Types.int32 Types.int32)
    expectTypeOf "partially applied string cat"
      (primitive _strings_cat2 @@ string "prefix")
      (Types.function Types.string Types.string)

  H.describe "Higher-order applications" $ do
    expectTypeOf "apply function to function"
      (lets ["apply">: lambda "f" $ lambda "x" $ var "f" @@ var "x",
             "double">: lambda "n" $ primitive _math_mul @@ var "n" @@ int32 2] $
            var "apply" @@ var "double" @@ int32 5)
      Types.int32
    expectTypeOf "function composition"
      (lets ["compose">: lambda "f" $ lambda "g" $ lambda "x" $ var "f" @@ (var "g" @@ var "x"),
             "add1">: lambda "n" $ primitive _math_add @@ var "n" @@ int32 1,
             "mul2">: lambda "n" $ primitive _math_mul @@ var "n" @@ int32 2] $
            var "compose" @@ var "add1" @@ var "mul2" @@ int32 3)
      Types.int32

  H.describe "Polymorphic applications" $ do
    expectTypeOf "polymorphic identity"
      (lets ["id">: lambda "x" $ var "x"] $
            tuple [var "id" @@ int32 42, var "id" @@ string "hello"])
      (Types.product [Types.int32, Types.string])
    expectTypeOf "polymorphic const"
      (lets ["const">: lambda "x" $ lambda "y" $ var "x"] $
            var "const" @@ string "keep" @@ int32 999)
      Types.string
    expectTypeOf "polymorphic flip"
      (lets ["flip">: lambda "f" $ lambda "x" $ lambda "y" $ var "f" @@ var "y" @@ var "x"] $
            var "flip" @@ primitive _strings_cat2 @@ string "world" @@ string "hello")
      Types.string

  H.describe "Applications in complex contexts" $ do
    expectTypeOf "application in tuple"
      (tuple [primitive _math_add @@ int32 1 @@ int32 2,
              primitive _strings_cat2 @@ string "a" @@ string "b"])
      (Types.product [Types.int32, Types.string])
    expectTypeOf "application in record"
      (record testTypePersonName [
        field "firstName" (primitive _strings_cat2 @@ string "John" @@ string "ny"),
        field "lastName" (string "Doe"),
        field "age" (primitive _math_add @@ int32 20 @@ int32 5)])
      (Types.var "Person")
    expectTypeOf "application in let binding"
      (lets ["result">: primitive _math_mul @@ int32 6 @@ int32 7] $
            var "result")
      Types.int32
    expectTypeOf "nested applications"
      (primitive _math_add @@ (primitive _math_mul @@ int32 3 @@ int32 4) @@ (primitive _math_add @@ int32 1 @@ int32 2))
      Types.int32

  H.describe "Applications with complex arguments" $ do
    expectTypeOf "application with record argument"
      (lets ["getName">: lambda "person" $ project testTypePersonName (Name "firstName") @@ var "person"] $
            var "getName" @@ record testTypePersonName [
              field "firstName" (string "Alice"),
              field "lastName" (string "Smith"),
              field "age" (int32 25)])
      Types.string
    expectTypeOf "application with list argument"
      (lets ["head">: lambda "xs" $ primitive _lists_head @@ var "xs"] $
            var "head" @@ list [string "first", string "second"])
      Types.string

checkTypeOfEliminations :: H.SpecWith ()
checkTypeOfEliminations = H.describe "Eliminations" $ do
  checkTypeOfProductEliminations
  checkTypeOfRecordEliminations
  checkTypeOfUnionEliminations
--  checkTypeOfWrapEliminations TODO: restore me

checkTypeOfFunctions :: H.SpecWith ()
checkTypeOfFunctions = H.describe "Functions" $ do
  checkTypeOfEliminations
  checkTypeOfLambdas
  checkTypeOfPrimitives

checkTypeOfLambdas :: H.SpecWith ()
checkTypeOfLambdas = H.describe "Lambdas" $ do
  H.describe "Simple lambdas" $ do
    expectTypeOf "identity function"
      (lambda "x" $ var "x")
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.var "t0"))
    expectTypeOf "constant function"
      (lambda "x" $ int32 42)
      (Types.forAll "t0" $ Types.function (Types.var "t0") Types.int32)

  H.describe "Multi-parameter lambdas" $ do
    expectTypeOf "two parameters"
      (lambda "x" $ lambda "y" $ var "x")
      (Types.forAlls ["t1", "t0"] $ Types.function (Types.var "t0") (Types.function (Types.var "t1") (Types.var "t0")))
    expectTypeOf "three parameters"
      (lambda "x" $ lambda "y" $ lambda "z" $ var "y")
      (Types.forAlls ["t2", "t1", "t0"] $ Types.function (Types.var "t0") (Types.function (Types.var "t1") (Types.function (Types.var "t2") (Types.var "t1"))))
    expectTypeOf "parameter reuse"
      (lambda "x" $ lambda "y" $ tuple [var "x", var "x", var "y"])
      (Types.forAlls ["t1", "t0"] $ Types.function (Types.var "t0") (Types.function (Types.var "t1") (Types.product [Types.var "t0", Types.var "t0", Types.var "t1"])))

  H.describe "Lambdas with operations" $ do
    expectTypeOf "lambda with primitive"
      (lambda "x" $ primitive _math_add @@ var "x" @@ int32 1)
      (Types.function Types.int32 Types.int32)
    expectTypeOf "lambda with application"
      (lambda "f" $ lambda "x" $ var "f" @@ var "x")
      (Types.forAlls ["t1", "t0"] $ Types.function (Types.function (Types.var "t0") (Types.var "t1")) (Types.function (Types.var "t0") (Types.var "t1")))
    expectTypeOf "lambda with construction"
      (lambda "x" $ lambda "y" $ tuple [var "x", var "y"])
      (Types.forAlls ["t1", "t0"] $ Types.function (Types.var "t0") (Types.function (Types.var "t1") (Types.product [Types.var "t0", Types.var "t1"])))

  H.describe "Nested lambdas" $ do
    expectTypeOf "lambda returning lambda"
      (lambda "x" $ lambda "y" $ lambda "z" $ var "x")
      (Types.forAlls ["t2", "t1", "t0"] $ Types.function (Types.var "t0") (Types.function (Types.var "t1") (Types.function (Types.var "t2") (Types.var "t0"))))
    expectTypeOf "lambda with let binding"
      (lambda "x" $ lets ["y">: var "x"] $ var "y")
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.var "t0"))
    expectTypeOf "lambda with inner lambda"
      (lambda "outer" $ lets ["inner">: lambda "x" $ var "x"] $ var "inner" @@ var "outer")
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.var "t0"))

  H.describe "Lambdas in complex contexts" $ do
    expectTypeOf "lambda in tuple"
      (tuple [lambda "x" $ var "x", int32 42])
      (Types.forAll "t0" $ Types.product [Types.function (Types.var "t0") (Types.var "t0"), Types.int32])
    expectTypeOf "lambda in list"
      (list [lambda "x" $ primitive _math_add @@ var "x" @@ int32 1,
             lambda "y" $ primitive _math_mul @@ var "y" @@ int32 2])
      (Types.list $ Types.function Types.int32 Types.int32)
    expectTypeOf "lambda in record"
      (lambda "name" $ record testTypePersonName [
        field "firstName" (var "name"),
        field "lastName" (string "Doe"),
        field "age" (int32 30)])
      (Types.function Types.string (Types.var "Person"))

  H.describe "Higher-order lambdas" $ do
    expectTypeOf "function composition"
      (lambda "f" $ lambda "g" $ lambda "x" $ var "f" @@ (var "g" @@ var "x"))
      (Types.forAlls ["t2", "t1", "t0"] $ Types.function
        (Types.function (Types.var "t0") (Types.var "t1"))
        (Types.function
          (Types.function (Types.var "t2") (Types.var "t0"))
          (Types.function (Types.var "t2") (Types.var "t1"))))
    expectTypeOf "function application"
      (lambda "f" $ lambda "x" $ var "f" @@ var "x")
      (Types.forAlls ["t1", "t0"] $ Types.function (Types.function (Types.var "t0") (Types.var "t1")) (Types.function (Types.var "t0") (Types.var "t1")))
    expectTypeOf "curried function"
      (lambda "x" $ lambda "y" $ lambda "z" $ primitive _logic_ifElse @@ var "x" @@ var "y" @@ var "z")
      (Types.forAll "t0" $ Types.function Types.boolean (Types.function (Types.var "t0") (Types.function (Types.var "t0") (Types.var "t0"))))

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
  H.describe "Monomorphic optionals" $ do
    expectTypeOf "nothing"
      (optional Nothing)
      (Types.forAll "t0" $ Types.optional $ Types.var "t0")
    expectTypeOf "just int"
      (optional $ Just $ int32 42)
      (Types.optional Types.int32)
    expectTypeOf "just string"
      (optional $ Just $ string "hello")
      (Types.optional Types.string)
    expectTypeOf "just boolean"
      (optional $ Just $ boolean True)
      (Types.optional Types.boolean)

  H.describe "Polymorphic optionals" $ do
    expectTypeOf "optional from lambda"
      (lambda "x" $ optional $ Just $ var "x")
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.optional $ Types.var "t0"))
    expectTypeOf "nothing from lambda"
      (lambda "x" $ optional Nothing)
      (Types.forAlls ["t1", "t0"] $ Types.function (Types.var "t0") (Types.optional $ Types.var "t1"))
    expectTypeOf "conditional optional"
      (lambda "x" $ lambda "flag" $
        primitive _logic_ifElse @@ var "flag" @@
          (optional $ Just $ var "x") @@
          (optional Nothing))
      (Types.forAlls ["t0"] $ Types.function (Types.var "t0") (Types.function Types.boolean (Types.optional $ Types.var "t0")))

  H.describe "Optionals in complex contexts" $ do
    expectTypeOf "optional in tuple"
      (tuple [optional $ Just $ int32 100, string "context"])
      (Types.product [Types.optional Types.int32, Types.string])
    expectTypeOf "optional in record"
      (record testTypeBuddyListAName [
        field "head" (string "first"),
        field "tail" (optional $ Just $ record testTypeBuddyListBName [
          field "head" (string "second"),
          field "tail" (optional Nothing)])])
      (Types.apply (Types.var "BuddyListA") Types.string)
    expectTypeOf "optional in let binding"
      (lets ["maybeValue">: optional $ Just $ int32 42] $
            var "maybeValue")
      (Types.optional Types.int32)

  H.describe "Nested optionals" $ do
    expectTypeOf "optional of optional"
      (optional $ Just $ optional $ Just $ string "nested")
      (Types.optional $ Types.optional Types.string)
    expectTypeOf "optional of list"
      (optional $ Just $ list [int32 1, int32 2, int32 3])
      (Types.optional $ Types.list Types.int32)
    expectTypeOf "list of optionals"
      (list [optional $ Just $ string "a", optional Nothing, optional $ Just $ string "b"])
      (Types.list $ Types.optional Types.string)

  H.describe "Optionals with complex types" $ do
    expectTypeOf "optional record"
      (optional $ Just $ record testTypePersonName [
        field "firstName" (string "Alice"),
        field "lastName" (string "Smith"),
        field "age" (int32 30)])
      (Types.optional $ Types.var "Person")
    expectTypeOf "optional tuple"
      (optional $ Just $ tuple [int32 10, string "test"])
      (Types.optional $ Types.product [Types.int32, Types.string])
    expectTypeOf "optional map"
      (optional $ Just $ Terms.map $ M.singleton (string "key") (int32 42))
      (Types.optional $ Types.map Types.string Types.int32)

checkTypeOfPrimitives :: H.SpecWith ()
checkTypeOfPrimitives = H.describe "Primitives" $ do
  H.describe "Nullary primitives" $ do
    expectTypeOf "empty map"
      (primitive _maps_empty)
      (Types.forAlls ["t1", "t0"] $ Types.map (Types.var "t1") (Types.var "t0"))
    expectTypeOf "empty set"
      (primitive _sets_empty)
      (Types.forAll "t0" $ Types.set $ Types.var "t0")

  H.describe "Unary primitives" $ do
    expectTypeOf "lists head"
      (primitive _lists_head)
      (Types.forAll "t0" $ Types.function (Types.list $ Types.var "t0") (Types.var "t0"))
    expectTypeOf "math neg"
      (primitive _math_neg)
      (Types.function Types.int32 Types.int32)
    expectTypeOf "logic not"
      (primitive _logic_not)
      (Types.function Types.boolean Types.boolean)

  H.describe "Binary primitives" $ do
    expectTypeOf "math add"
      (primitive _math_add)
      (Types.function Types.int32 (Types.function Types.int32 Types.int32))
    expectTypeOf "lists cons"
      (primitive _lists_cons)
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.function (Types.list $ Types.var "t0") (Types.list $ Types.var "t0")))
    expectTypeOf "maps insert"
      (primitive _maps_insert)
      (Types.forAlls ["t1", "t0"] $ Types.function
        (Types.var "t1")
        (Types.function (Types.var "t0") (Types.function (Types.map (Types.var "t1") (Types.var "t0")) (Types.map (Types.var "t1") (Types.var "t0")))))

  H.describe "Ternary primitives" $ do
    expectTypeOf "logic ifElse"
      (primitive _logic_ifElse)
      (Types.forAll "t0" $ Types.function Types.boolean (Types.function (Types.var "t0") (Types.function (Types.var "t0") (Types.var "t0"))))
    expectTypeOf "lists foldl"
      (primitive _lists_foldl)
      (Types.forAlls ["t1", "t0"] $ Types.function
        (Types.function (Types.var "t1") (Types.function (Types.var "t0") (Types.var "t1")))
        (Types.function (Types.var "t1") (Types.function (Types.list $ Types.var "t0") (Types.var "t1"))))

  H.describe "Monomorphic vs polymorphic" $ do
    expectTypeOf "monomorphic math"
      (primitive _math_add)
      (Types.function Types.int32 (Types.function Types.int32 Types.int32))
    expectTypeOf "polymorphic identity"
      (primitive _equality_identity)
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.var "t0"))
    expectTypeOf "polymorphic map"
      (primitive _lists_map)
      (Types.forAlls ["t1", "t0"] $ Types.function
        (Types.function (Types.var "t1") (Types.var "t0"))
        (Types.function (Types.list $ Types.var "t1") (Types.list $ Types.var "t0")))

  H.describe "Higher-order primitives" $ do
    expectTypeOf "lists map function"
      (primitive _lists_map @@ (lambda "x" $ primitive _math_add @@ var "x" @@ int32 1))
      (Types.function (Types.list Types.int32) (Types.list Types.int32))
    expectTypeOf "lists filter"
      (primitive _lists_filter)
      (Types.forAll "t0" $ Types.function (Types.function (Types.var "t0") Types.boolean) (Types.function (Types.list $ Types.var "t0") (Types.list $ Types.var "t0")))
    expectTypeOf "optionals maybe"
      (primitive _optionals_maybe)
      (Types.forAlls ["t1", "t0"] $ Types.function (Types.var "t1") (Types.function (Types.function (Types.var "t0") (Types.var "t1")) (Types.function (Types.optional $ Types.var "t0") (Types.var "t1"))))

  H.describe "Primitives in complex contexts" $ do
    expectTypeOf "primitive composition"
      (lets ["double">: lambda "x" $ primitive _math_mul @@ var "x" @@ int32 2,
             "increment">: lambda "x" $ primitive _math_add @@ var "x" @@ int32 1] $
            primitive _lists_map @@ var "double" @@ (primitive _lists_map @@ var "increment" @@ list [int32 1, int32 2, int32 3]))
      (Types.list Types.int32)
    expectTypeOf "nested higher-order"
      (primitive _lists_map @@ (primitive _lists_map @@ (primitive _math_add @@ int32 1)) @@
       list [list [int32 1, int32 2], list [int32 3, int32 4]])
      (Types.list $ Types.list Types.int32)

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

checkTypeOfProductEliminations :: H.SpecWith ()
checkTypeOfProductEliminations = H.describe "Product eliminations" $ do
  H.describe "Simple tuple projections" $ do
    expectTypeOf "projection from pair"
      (untuple 2 0 @@ pair (int32 42) (string "hello"))
      Types.int32
    expectTypeOf "second projection from pair"
      (untuple 2 1 @@ pair (int32 42) (string "hello"))
      Types.string
    expectTypeOf "projection from triple"
      (untuple 3 1 @@ triple (int32 1) (string "middle") (boolean True))
      Types.string
    expectTypeOf "first element of triple"
      (untuple 3 0 @@ triple (boolean False) (int32 100) (string "last"))
      Types.boolean
    expectTypeOf "last element of triple"
      (untuple 3 2 @@ triple (boolean False) (int32 100) (string "last"))
      Types.string

  H.describe "Polymorphic tuple projections" $ do
    expectTypeOf "projection function"
      (untuple 2 0)
      (Types.forAlls ["t1", "t0"] $ Types.function
        (Types.product [Types.var "t0", Types.var "t1"])
        (Types.var "t0"))
    expectTypeOf "second projection function"
      (untuple 2 1)
      (Types.forAlls ["t1", "t0"] $ Types.function
        (Types.product [Types.var "t0", Types.var "t1"])
        (Types.var "t1"))
    expectTypeOf "triple projection function"
      (untuple 3 1)
      (Types.forAlls ["t2", "t1", "t0"] $ Types.function
        (Types.product [Types.var "t0", Types.var "t1", Types.var "t2"])
        (Types.var "t1"))
    expectTypeOf "projection from lambda"
      (lambda "pair" $ untuple 2 0 @@ var "pair")
      (Types.forAlls ["t1", "t0"] $ Types.function
        (Types.product [Types.var "t0", Types.var "t1"])
        (Types.var "t0"))

  H.describe "Projections with variables" $ do
    expectTypeOf "projection with variable tuple"
      (lambda "x" $ lambda "y" $ untuple 2 0 @@ pair (var "x") (var "y"))
      (Types.forAlls ["t1", "t0"] $ Types.function (Types.var "t0")
        (Types.function (Types.var "t1") (Types.var "t0")))
    expectTypeOf "projection preserves polymorphism"
      (lambda "pair" $ pair (untuple 2 0 @@ var "pair") (untuple 2 1 @@ var "pair"))
      (Types.forAlls ["t1", "t0"] $ Types.function
        (Types.product [Types.var "t0", Types.var "t1"])
        (Types.product [Types.var "t0", Types.var "t1"]))
    expectTypeOf "nested projection"
      (lambda "nested" $ untuple 2 0 @@ (untuple 2 1 @@ var "nested"))
      (Types.forAlls ["t2", "t1", "t0"] $ Types.function
        (Types.product [Types.var "t0", Types.product [Types.var "t1", Types.var "t2"]])
        (Types.var "t1"))

  H.describe "Projections in complex contexts" $ do
    expectTypeOf "projection in let binding"
      (lets ["pair">: pair (int32 10) (string "test")] $
            untuple 2 0 @@ var "pair")
      Types.int32
    expectTypeOf "projection in tuple"
      (pair (untuple 2 0 @@ pair (int32 1) (string "a"))
            (untuple 2 1 @@ pair (int32 2) (string "b")))
      (Types.product [Types.int32, Types.string])
    expectTypeOf "projection in list"
      (list [untuple 2 0 @@ pair (int32 1) (string "a"),
             untuple 2 0 @@ pair (int32 2) (string "b")])
      (Types.list Types.int32)

  H.describe "Higher-order projections" $ do
    expectTypeOf "map projection over list"
      (primitive _lists_map @@ (untuple 2 0) @@
       list [pair (int32 1) (string "a"), pair (int32 2) (string "b")])
      (Types.list Types.int32)
    expectTypeOf "projection composition"
      (lets ["getFirst">: untuple 2 0,
             "getSecond">: untuple 2 1] $
            lambda "pair" $ pair (var "getFirst" @@ var "pair") (var "getSecond" @@ var "pair"))
      (Types.forAlls ["t1", "t0"] $ Types.function
        (Types.product [Types.var "t0", Types.var "t1"])
        (Types.product [Types.var "t0", Types.var "t1"]))
    expectTypeOf "partial application of projection"
      (primitive _lists_map @@ (untuple 2 1))
      (Types.forAlls ["t1", "t0"] $ Types.function
        (Types.list (Types.product [Types.var "t0", Types.var "t1"]))
        (Types.list (Types.var "t1")))

  H.describe "Projections with mixed types" $ do
    expectTypeOf "projection from mixed tuple"
      (untuple 4 2 @@ tuple4 (int32 1) (string "test") (boolean True) (float32 3.14))
      Types.boolean
    expectTypeOf "projection chain"
      (lets ["quadruple">: tuple4 (int32 1) (string "test") (boolean True) (float32 3.14)] $
            tuple4 (untuple 4 0 @@ var "quadruple")
                   (untuple 4 1 @@ var "quadruple")
                   (untuple 4 2 @@ var "quadruple")
                   (untuple 4 3 @@ var "quadruple"))
      (Types.product [Types.int32, Types.string, Types.boolean, Types.float32])
    expectTypeOf "projection with function result"
      (untuple 2 1 @@ pair (int32 42) (lambda "x" $ var "x"))
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.var "t0"))

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

checkTypeOfRecordEliminations :: H.SpecWith ()
checkTypeOfRecordEliminations = H.describe "Record eliminations" $ do
  return ()  -- TODO: implement

checkTypeOfSets :: H.SpecWith ()
checkTypeOfSets = H.describe "Sets" $ do
  H.describe "Monomorphic sets" $ do
    expectTypeOf "empty set"
      (Terms.set S.empty)
      (Types.forAll "t0" $ Types.set $ Types.var "t0")
    expectTypeOf "int set"
      (Terms.set $ S.fromList [int32 1, int32 2, int32 3])
      (Types.set Types.int32)
    expectTypeOf "string set"
      (Terms.set $ S.fromList [string "apple", string "banana", string "cherry"])
      (Types.set Types.string)
    expectTypeOf "single element set"
      (Terms.set $ S.singleton $ boolean True)
      (Types.set Types.boolean)

  H.describe "Polymorphic sets" $ do
    expectTypeOf "set from lambda"
      (lambda "x" $ Terms.set $ S.singleton $ var "x")
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.set $ Types.var "t0"))
    expectTypeOf "set with repeated variable"
      (lambda "x" $ Terms.set $ S.fromList [var "x", var "x"])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.set $ Types.var "t0"))
    expectTypeOf "set from two variables"
      (lambda "x" $ lambda "y" $ Terms.set $ S.fromList [var "x", var "y"])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.function (Types.var "t0") (Types.set $ Types.var "t0")))

  H.describe "Sets in complex contexts" $ do
    expectTypeOf "set in tuple"
      (tuple [Terms.set $ S.fromList [int32 1, int32 2], string "context"])
      (Types.product [Types.set Types.int32, Types.string])
    expectTypeOf "set in let binding"
      (lets ["numbers">: Terms.set $ S.fromList [int32 10, int32 20, int32 30]] $
            var "numbers")
      (Types.set Types.int32)

  H.describe "Nested sets" $ do
    expectTypeOf "set of lists"
      (Terms.set $ S.fromList [
        list [string "a", string "b"],
        list [string "c", string "d"]])
      (Types.set $ Types.list Types.string)
    expectTypeOf "set of tuples"
      (Terms.set $ S.fromList [
        tuple [int32 1, int32 2],
        tuple [int32 3, int32 4]])
      (Types.set $ Types.product [Types.int32, Types.int32])
    expectTypeOf "set of sets"
      (Terms.set $ S.singleton $ Terms.set $ S.fromList [string "nested"])
      (Types.set $ Types.set Types.string)

  H.describe "Sets with complex types" $ do
    expectTypeOf "set of records"
      (Terms.set $ S.singleton $ record testTypePersonName [
        field "firstName" (string "Alice"),
        field "lastName" (string "Smith"),
        field "age" (int32 30)])
      (Types.set $ Types.var "Person")
    expectTypeOf "set of optionals"
      (Terms.set $ S.fromList [
        optional $ Just $ int32 42,
        optional Nothing])
      (Types.set $ Types.optional Types.int32)
    expectTypeOf "set of maps"
      (Terms.set $ S.singleton $ Terms.map $ M.singleton (string "key") (int32 42))
      (Types.set $ Types.map Types.string Types.int32)

checkTypeOfSums :: H.SpecWith ()
checkTypeOfSums = H.describe "Sums" $ do
  return ()  -- TODO: implement

checkTypeOfUnions :: H.SpecWith ()
checkTypeOfUnions = H.describe "Unions" $ do
  return ()  -- TODO: implement

checkTypeOfUnionEliminations :: H.SpecWith ()
checkTypeOfUnionEliminations = H.describe "Union eliminations" $ do
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
  H.describe "Simple variable lookup" $ do
    expectTypeOf "int variable"
      (lambda "x" $ var "x")
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.var "t0"))
    expectTypeOf "variable in let binding"
      (lets ["x">: int32 42] $ var "x")
      Types.int32
    expectTypeOf "multiple variables"
      (lets ["x">: string "hello",
             "y">: int32 42] $
            tuple [var "x", var "y"])
      (Types.product [Types.string, Types.int32])

  H.describe "Variable scoping" $ do
    expectTypeOf "lambda parameter"
      (lambda "x" $ lambda "y" $ var "x")
      (Types.forAlls ["t1", "t0"] $ Types.function (Types.var "t0") (Types.function (Types.var "t1") (Types.var "t0")))
    expectTypeOf "let binding scope"
      (lets ["x">: int32 1] $
       lets ["y">: string "hello"] $
            var "x")
      Types.int32
    expectTypeOf "variable shadowing"
      (lets ["x">: int32 1] $
       lambda "x" $ var "x")
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.var "t0"))
    expectTypeOf "nested scoping"
      (lambda "x" $
       lets ["y">: var "x"] $
            lambda "z" $
            tuple [var "x", var "y", var "z"])
      (Types.forAlls ["t1", "t0"] $ Types.function (Types.var "t0") (Types.function (Types.var "t1") (Types.product [Types.var "t0", Types.var "t0", Types.var "t1"])))

  H.describe "Polymorphic variables" $ do
    expectTypeOf "polymorphic function"
      (lets ["id">: lambda "x" $ var "x"] $
            var "id")
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.var "t0"))
    expectTypeOf "polymorphic application"
      (lets ["id">: lambda "x" $ var "x"] $
            tuple [var "id" @@ int32 42, var "id" @@ string "test"])
      (Types.product [Types.int32, Types.string])
    expectTypeOf "higher order polymorphic"
      (lets ["apply">: lambda "f" $ lambda "x" $ var "f" @@ var "x"] $
            var "apply")
      (Types.forAlls ["t1", "t0"] $ Types.function (Types.function (Types.var "t0") (Types.var "t1")) (Types.function (Types.var "t0") (Types.var "t1")))

  H.describe "Variables in complex contexts" $ do
    expectTypeOf "variable in record"
      (lambda "name" $
       record testTypePersonName [
         field "firstName" (var "name"),
         field "lastName" (string "Doe"),
         field "age" (int32 25)])
      (Types.function Types.string (Types.var "Person"))
    expectTypeOf "variable in list"
      (lambda "x" $ list [var "x", var "x"])
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.list $ Types.var "t0"))
    expectTypeOf "variable in map"
      (lambda "key" $ lambda "value" $
       Terms.map $ M.singleton (var "key") (var "value"))
      (Types.forAlls ["t1", "t0"] $ Types.function (Types.var "t0") (Types.function (Types.var "t1") (Types.map (Types.var "t0") (Types.var "t1"))))
    expectTypeOf "variable in optional"
      (lambda "x" $ optional $ Just $ var "x")
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.optional $ Types.var "t0"))

  H.describe "Recursive variables" $ do
    expectTypeOf "simple recursion"
      (lets ["f">: lambda "x" $ primitive _math_add @@ var "x" @@ int32 1] $
            var "f")
      (Types.function Types.int32 Types.int32)
    expectTypeOf "mutual recursion"
      (lets ["f">: lambda "x" $ var "g" @@ var "x",
             "g">: lambda "y" $ primitive _math_add @@ var "y" @@ int32 1] $
            var "f")
      (Types.function Types.int32 Types.int32)

checkTypeOfWrappedTerms :: H.SpecWith ()
checkTypeOfWrappedTerms = H.describe "Wrapped terms" $ do
  H.describe "Monomorphic wrapped terms" $ do
    expectTypeOf "string alias"
      (wrap testTypeStringAliasName (string "hello"))
      (Types.var "StringTypeAlias")
    expectTypeOf "wrapped integer"
      (wrap testTypeStringAliasName (string "wrapped"))
      (Types.var "StringTypeAlias")
    expectTypeOf "wrapped in tuple"
      (tuple [wrap testTypeStringAliasName (string "first"),
              string "second"])
      (Types.product [Types.var "StringTypeAlias", Types.string])

  H.describe "Polymorphic wrapped terms" $ do
    expectTypeOf "polymorphic wrapper with int"
      (wrap testTypePolymorphicWrapperName (list [int32 1, int32 2]))
      (Types.apply (Types.var "PolymorphicWrapper") Types.int32)
    expectTypeOf "polymorphic wrapper with string"
      (wrap testTypePolymorphicWrapperName (list [string "a", string "b"]))
      (Types.apply (Types.var "PolymorphicWrapper") Types.string)
    expectTypeOf "polymorphic wrapper from lambda"
      (lambda "x" $ wrap testTypePolymorphicWrapperName (list [var "x"]))
      (Types.forAll "t0" $ Types.function (Types.var "t0") (Types.apply (Types.var "PolymorphicWrapper") (Types.var "t0")))

  H.describe "Wrapped terms in complex contexts" $ do
    expectTypeOf "wrapped in record"
      (record testTypePersonName [
        field "firstName" (string "John"),
        field "lastName" (string "Doe"),
        field "age" (int32 30)])
      (Types.var "Person")
    expectTypeOf "wrapped in let binding"
      (lets ["alias">: wrap testTypeStringAliasName (string "test")] $
            var "alias")
      (Types.var "StringTypeAlias")
    expectTypeOf "wrapped in list"
      (list [wrap testTypeStringAliasName (string "first"),
             wrap testTypeStringAliasName (string "second")])
      (Types.list $ Types.var "StringTypeAlias")

  H.describe "Nested wrapped terms" $ do
    expectTypeOf "wrapped tuple"
      (wrap testTypePolymorphicWrapperName (list [tuple [int32 1, string "a"]]))
      (Types.apply (Types.var "PolymorphicWrapper") (Types.product [Types.int32, Types.string]))
    expectTypeOf "wrapped optional"
      (wrap testTypePolymorphicWrapperName (list [optional $ Just $ int32 42]))
      (Types.apply (Types.var "PolymorphicWrapper") (Types.optional Types.int32))
    expectTypeOf "wrapped map"
      (wrap testTypePolymorphicWrapperName (list [Terms.map $ M.singleton (string "key") (int32 42)]))
      (Types.apply (Types.var "PolymorphicWrapper") (Types.map Types.string Types.int32))

  H.describe "Multiple wrapping levels" $ do
    expectTypeOf "wrapped in optional"
      (optional $ Just $ wrap testTypeStringAliasName (string "wrapped"))
      (Types.optional $ Types.var "StringTypeAlias")
    expectTypeOf "list of wrapped polymorphic"
      (list [wrap testTypePolymorphicWrapperName (list [int32 1]),
             wrap testTypePolymorphicWrapperName (list [int32 2])])
      (Types.list $ Types.apply (Types.var "PolymorphicWrapper") Types.int32)

checkTypeOfWrapEliminations :: H.SpecWith ()
checkTypeOfWrapEliminations = H.describe "Wrap eliminations" $ do
  H.describe "Monomorphic unwrapping" $ do
    expectTypeOf "unwrap string alias"
      (unwrap testTypeStringAliasName)
      (Types.function (Types.var "StringTypeAlias") Types.string)

  H.describe "Polymorphic unwrapping" $ do
    expectTypeOf "unwrap polymorphic wrapper"
      (unwrap testTypePolymorphicWrapperName)
      (Types.forAll "t0" $ Types.function (Types.apply (Types.var "PolymorphicWrapper") (Types.var "t0")) (Types.list $ Types.var "t0"))

  H.describe "Unwrap eliminations in applications" $ do
    expectTypeOf "unwrap applied to wrapped term"
      (unwrap testTypeStringAliasName @@ wrap testTypeStringAliasName (string "hello"))
      Types.string
    expectTypeOf "unwrap polymorphic applied"
      (unwrap testTypePolymorphicWrapperName @@ wrap testTypePolymorphicWrapperName (list [int32 1, int32 2]))
      (Types.list Types.int32)

  H.describe "Unwrap in complex contexts" $ do
    expectTypeOf "unwrap in let binding"
      (lets ["unwrapper">: unwrap testTypeStringAliasName,
             "wrapped">: wrap testTypeStringAliasName (string "test")] $
            var "unwrapper" @@ var "wrapped")
      Types.string
    expectTypeOf "unwrap in tuple"
      (tuple [unwrap testTypeStringAliasName, string "context"])
      (Types.product [Types.function (Types.var "StringTypeAlias") Types.string, Types.string])
    expectTypeOf "unwrap in lambda"
      (lambda "wrapped" $ unwrap testTypeStringAliasName @@ var "wrapped")
      (Types.function (Types.var "StringTypeAlias") Types.string)

  H.describe "Chained unwrapping" $ do
    expectTypeOf "unwrap then process"
      (lambda "wrapped" $
       primitive _strings_cat2 @@ (unwrap testTypeStringAliasName @@ var "wrapped") @@ string " suffix")
      (Types.function (Types.var "StringTypeAlias") Types.string)
    expectTypeOf "unwrap polymorphic then map"
      (lambda "wrappedList" $
       primitive _lists_map @@ (primitive _math_add @@ int32 1) @@ (unwrap testTypePolymorphicWrapperName @@ var "wrappedList"))
      (Types.function (Types.apply (Types.var "PolymorphicWrapper") Types.int32) (Types.list Types.int32))

  H.describe "Multiple unwrap operations" $ do
    expectTypeOf "unwrap different types"
      (lambda "stringWrapped" $ lambda "listWrapped" $
       tuple [unwrap testTypeStringAliasName @@ var "stringWrapped",
              unwrap testTypePolymorphicWrapperName @@ var "listWrapped"])
      (Types.forAll "t0" $ Types.function (Types.var "StringTypeAlias")
        (Types.function (Types.apply (Types.var "PolymorphicWrapper") (Types.var "t0"))
          (Types.product [Types.string, Types.list $ Types.var "t0"])))

----------------------------------------

expectTypeOf :: String -> Term -> Type -> H.SpecWith ()
expectTypeOf desc term typ = H.it desc $ withDefaults expectTypeOfResult desc term typ

typeOfShouldFail :: String -> M.Map Name Type -> Term -> H.SpecWith ()
typeOfShouldFail desc types term = H.it desc $ shouldFail $ do
  cx <- graphToInferenceContext testGraph
  typeOf cx S.empty types term

withDefaults :: (String -> M.Map Name Type -> Term -> x) -> String -> Term -> x
withDefaults f desc = f desc M.empty

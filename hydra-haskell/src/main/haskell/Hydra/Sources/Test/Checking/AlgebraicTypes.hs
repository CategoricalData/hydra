
-- | Algebraic type checking test cases: unit, pairs, eithers, optionals
module Hydra.Sources.Test.Checking.AlgebraicTypes where

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
ns = Namespace "hydra.test.checking.algebraicTypes"

module_ :: Module
module_ = Module ns definitions
    [TestGraph.ns, Namespace "hydra.rewriting"]
    kernelTypesNamespaces
    (Just "Algebraic type checking test cases: unit, pairs, eithers, optionals")
  where
    definitions = [
      Phantoms.toDefinition allTests,
      Phantoms.toDefinition unitTests,
      Phantoms.toDefinition unitTermTests,
      Phantoms.toDefinition unitTermInPolymorphicContextTests,
      Phantoms.toDefinition pairsTests,
      Phantoms.toDefinition basicPairsTests,
      Phantoms.toDefinition polymorphicPairsTests,
      Phantoms.toDefinition pairsInComplexContextsTests,
      Phantoms.toDefinition nestedPairsTests,
      Phantoms.toDefinition pairsWithComplexTypesTests,
      Phantoms.toDefinition eithersTests,
      Phantoms.toDefinition leftValuesTests,
      Phantoms.toDefinition rightValuesTests,
      Phantoms.toDefinition polymorphicEithersTests,
      Phantoms.toDefinition eithersInComplexContextsTests,
      Phantoms.toDefinition nestedEithersTests,
      Phantoms.toDefinition eithersWithComplexTypesTests,
      Phantoms.toDefinition optionalsTests,
      Phantoms.toDefinition monomorphicOptionalsTests,
      Phantoms.toDefinition polymorphicOptionalsTests,
      Phantoms.toDefinition optionalsInComplexContextsTests,
      Phantoms.toDefinition nestedOptionalsTests,
      Phantoms.toDefinition optionalsWithComplexTypesTests]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

allTests :: TTermDefinition TestGroup
allTests = define "allTests" $
  supergroup "Algebraic types" [
  unitTests,
  pairsTests,
  eithersTests,
  optionalsTests]

------ Helper functions ------

------ Unit ------

unitTests :: TTermDefinition TestGroup
unitTests = define "unitTests" $
  supergroup "Unit" [
  unitTermTests,
  unitTermInPolymorphicContextTests]

unitTermTests :: TTermDefinition TestGroup
unitTermTests = define "unitTermTests" $
  subgroup "Unit term" [
  noChange "unit literal"
    unit
    T.unit]

unitTermInPolymorphicContextTests :: TTermDefinition TestGroup
unitTermInPolymorphicContextTests = define "unitTermInPolymorphicContextTests" $
  subgroup "Unit term in polymorphic context" [
  checkTest "unit from lambda" []
    (lambda "x" unit)
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") unit)
    (T.forAlls ["t0"] $ T.function (T.var "t0") T.unit)]

------ Pairs ------

pairsTests :: TTermDefinition TestGroup
pairsTests = define "pairsTests" $
  supergroup "Pairs" [
  basicPairsTests,
  polymorphicPairsTests,
  pairsInComplexContextsTests,
  nestedPairsTests,
  pairsWithComplexTypesTests]

basicPairsTests :: TTermDefinition TestGroup
basicPairsTests = define "basicPairsTests" $
  subgroup "Basic pairs" [
  checkTest "pair of int and string" []
    (pair (int32 42) (string "hello"))
    (tyapps (pair (int32 42) (string "hello")) [T.int32, T.string])
    (T.pair T.int32 T.string),
  checkTest "pair of string and boolean" []
    (pair (string "test") (boolean True))
    (tyapps (pair (string "test") (boolean True)) [T.string, T.boolean])
    (T.pair T.string T.boolean),
  checkTest "pair of boolean and int" []
    (pair (boolean False) (int32 100))
    (tyapps (pair (boolean False) (int32 100)) [T.boolean, T.int32])
    (T.pair T.boolean T.int32)]

polymorphicPairsTests :: TTermDefinition TestGroup
polymorphicPairsTests = define "polymorphicPairsTests" $
  subgroup "Polymorphic pairs" [
  checkTest "pair from lambda (first element)" []
    (lambda "x" $ pair (var "x") (string "constant"))
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ tyapps (pair (var "x") (string "constant")) [T.var "t0", T.string])
    (T.forAll "t0" $ T.function (T.var "t0") (T.pair (T.var "t0") T.string)),
  checkTest "pair from lambda (second element)" []
    (lambda "x" $ pair (string "constant") (var "x"))
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ tyapps (pair (string "constant") (var "x")) [T.string, T.var "t0"])
    (T.forAll "t0" $ T.function (T.var "t0") (T.pair T.string (T.var "t0"))),
  checkTest "pair from two lambdas" []
    (lambda "x" $ lambda "y" $ pair (var "x") (var "y"))
    (tylams ["t0", "t1"] $ lambdaTyped "x" (T.var "t0") $ lambdaTyped "y" (T.var "t1") $ tyapps (pair (var "x") (var "y")) [T.var "t0", T.var "t1"])
    (T.forAlls ["t0", "t1"] $ T.function (T.var "t0") (T.function (T.var "t1") (T.pair (T.var "t0") (T.var "t1")))),
  checkTest "pair with repeated variable" []
    (lambda "x" $ pair (var "x") (var "x"))
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ tyapps (pair (var "x") (var "x")) [T.var "t0", T.var "t0"])
    (T.forAll "t0" $ T.function (T.var "t0") (T.pair (T.var "t0") (T.var "t0")))]

pairsInComplexContextsTests :: TTermDefinition TestGroup
pairsInComplexContextsTests = define "pairsInComplexContextsTests" $
  subgroup "Pairs in complex contexts" [
  checkTest "pair in list" []
    (list [pair (int32 1) (string "one"), pair (int32 2) (string "two")])
    (list [tyapps (pair (int32 1) (string "one")) [T.int32, T.string], tyapps (pair (int32 2) (string "two")) [T.int32, T.string]])
    (T.list $ T.pair T.int32 T.string),
  checkTest "pair in let binding" []
    (lets ["result" >: pair (int32 42) (string "answer")] $
      var "result")
    (letsTyped [("result", tyapps (pair (int32 42) (string "answer")) [T.int32, T.string], T.mono $ T.pair T.int32 T.string)] $
      var "result")
    (T.pair T.int32 T.string)]

nestedPairsTests :: TTermDefinition TestGroup
nestedPairsTests = define "nestedPairsTests" $
  subgroup "Nested pairs" [
  checkTest "pair of pairs" []
    (pair (pair (int32 1) (string "one")) (pair (boolean True) (int32 2)))
    (tyapps (pair (tyapps (pair (int32 1) (string "one")) [T.int32, T.string]) (tyapps (pair (boolean True) (int32 2)) [T.boolean, T.int32])) [T.pair T.int32 T.string, T.pair T.boolean T.int32])
    (T.pair (T.pair T.int32 T.string) (T.pair T.boolean T.int32)),
  checkTest "pair with list" []
    (pair (list [int32 1, int32 2]) (string "numbers"))
    (tyapps (pair (list [int32 1, int32 2]) (string "numbers")) [T.list T.int32, T.string])
    (T.pair (T.list T.int32) T.string),
  checkTest "list of pairs" []
    (list [pair (int32 1) (string "a"), pair (int32 2) (string "b")])
    (list [tyapps (pair (int32 1) (string "a")) [T.int32, T.string], tyapps (pair (int32 2) (string "b")) [T.int32, T.string]])
    (T.list $ T.pair T.int32 T.string)]

personRecord :: String -> String -> Int -> TTerm Term
personRecord fName lName age' =
  record (name "Person") [
    "firstName" >: string fName,
    "lastName" >: string lName,
    "age" >: int32 age']

pairsWithComplexTypesTests :: TTermDefinition TestGroup
pairsWithComplexTypesTests = define "pairsWithComplexTypesTests" $
  subgroup "Pairs with complex types" [
  checkTest "pair with record on first" []
    (pair (personRecord "Alice" "Smith" 30) (int32 1))
    (tyapps (pair (personRecord "Alice" "Smith" 30) (int32 1)) [T.var "Person", T.int32])
    (T.pair (T.var "Person") T.int32),
  checkTest "pair with record on second" []
    (pair (string "name") (personRecord "Bob" "Jones" 25))
    (tyapps (pair (string "name") (personRecord "Bob" "Jones" 25)) [T.string, T.var "Person"])
    (T.pair T.string (T.var "Person"))]

------ Eithers ------

eithersTests :: TTermDefinition TestGroup
eithersTests = define "eithersTests" $
  supergroup "Eithers" [
  leftValuesTests,
  rightValuesTests,
  polymorphicEithersTests,
  eithersInComplexContextsTests,
  nestedEithersTests,
  eithersWithComplexTypesTests]

leftValuesTests :: TTermDefinition TestGroup
leftValuesTests = define "leftValuesTests" $
  subgroup "Left values" [
  checkTest "left int" []
    (left $ int32 42)
    (tylam "t0" $ tyapps (left $ int32 42) [T.int32, T.var "t0"])
    (T.forAlls ["t0"] $ T.either_ T.int32 (T.var "t0")),
  checkTest "left string" []
    (left $ string "error")
    (tylam "t0" $ tyapps (left $ string "error") [T.string, T.var "t0"])
    (T.forAlls ["t0"] $ T.either_ T.string (T.var "t0")),
  checkTest "left boolean" []
    (left $ boolean False)
    (tylam "t0" $ tyapps (left $ boolean False) [T.boolean, T.var "t0"])
    (T.forAlls ["t0"] $ T.either_ T.boolean (T.var "t0"))]

rightValuesTests :: TTermDefinition TestGroup
rightValuesTests = define "rightValuesTests" $
  subgroup "Right values" [
  checkTest "right int" []
    (right $ int32 42)
    (tylam "t0" $ tyapps (right $ int32 42) [T.var "t0", T.int32])
    (T.forAlls ["t0"] $ T.either_ (T.var "t0") T.int32),
  checkTest "right string" []
    (right $ string "success")
    (tylam "t0" $ tyapps (right $ string "success") [T.var "t0", T.string])
    (T.forAlls ["t0"] $ T.either_ (T.var "t0") T.string),
  checkTest "right boolean" []
    (right $ boolean True)
    (tylam "t0" $ tyapps (right $ boolean True) [T.var "t0", T.boolean])
    (T.forAlls ["t0"] $ T.either_ (T.var "t0") T.boolean)]

polymorphicEithersTests :: TTermDefinition TestGroup
polymorphicEithersTests = define "polymorphicEithersTests" $
  subgroup "Polymorphic eithers" [
  checkTest "left from lambda" []
    (lambda "x" $ left $ var "x")
    (tylams ["t0", "t1"] $ lambdaTyped "x" (T.var "t0") $ tyapps (left $ var "x") [T.var "t0", T.var "t1"])
    (T.forAlls ["t0", "t1"] $ T.function (T.var "t0") (T.either_ (T.var "t0") (T.var "t1"))),
  checkTest "right from lambda" []
    (lambda "x" $ right $ var "x")
    (tylams ["t0", "t1"] $ lambdaTyped "x" (T.var "t0") $ tyapps (right $ var "x") [T.var "t1", T.var "t0"])
    (T.forAlls ["t0", "t1"] $ T.function (T.var "t0") (T.either_ (T.var "t1") (T.var "t0"))),
  checkTest "either from two lambdas" []
    (lambda "flag" $ lambda "x" $
      primitive _logic_ifElse @@ var "flag" @@
        (left $ var "x") @@
        (right $ var "x"))
    (tylam "t0" $ lambdaTyped "flag" T.boolean $ lambdaTyped "x" (T.var "t0") $
      tyapp (primitive _logic_ifElse) (T.either_ (T.var "t0") (T.var "t0")) @@ var "flag" @@
        tyapps (left $ var "x") [T.var "t0", T.var "t0"] @@
        tyapps (right $ var "x") [T.var "t0", T.var "t0"])
    (T.forAlls ["t0"] $ T.function T.boolean (T.function (T.var "t0") (T.either_ (T.var "t0") (T.var "t0"))))]

eithersInComplexContextsTests :: TTermDefinition TestGroup
eithersInComplexContextsTests = define "eithersInComplexContextsTests" $
  subgroup "Eithers in complex contexts" [
  checkTest "either in list" []
    (list [left $ string "error", right $ int32 42])
    (list [tyapps (left $ string "error") [T.string, T.int32], tyapps (right $ int32 42) [T.string, T.int32]])
    (T.list $ T.either_ T.string T.int32),
  checkTest "either in let binding" []
    (lets ["result">: right $ int32 42] $
      var "result")
    (tylam "t0" $ letsTyped [("result", tylam "t1" $ tyapps (right $ int32 42) [T.var "t1", T.int32], T.poly ["t1"] $ T.either_ (T.var "t1") T.int32)] $
      tyapp (var "result") (T.var "t0"))
    (T.forAlls ["t0"] $ T.either_ (T.var "t0") T.int32)]

nestedEithersTests :: TTermDefinition TestGroup
nestedEithersTests = define "nestedEithersTests" $
  subgroup "Nested eithers" [
  checkTest "either of either (left left)" []
    (left $ left $ int32 1)
    (tylams ["t0", "t1"] $ tyapps (left $ tyapps (left $ int32 1) [T.int32, T.var "t0"]) [T.either_ T.int32 (T.var "t0"), T.var "t1"])
    (T.forAlls ["t0", "t1"] $ T.either_ (T.either_ T.int32 (T.var "t0")) (T.var "t1")),
  checkTest "either of either (left right)" []
    (left $ right $ string "nested")
    (tylams ["t0", "t1"] $ tyapps (left $ tyapps (right $ string "nested") [T.var "t0", T.string]) [T.either_ (T.var "t0") T.string, T.var "t1"])
    (T.forAlls ["t0", "t1"] $ T.either_ (T.either_ (T.var "t0") T.string) (T.var "t1")),
  checkTest "either of either (right)" []
    (right $ boolean True)
    (tylam "t0" $ tyapps (right $ boolean True) [T.var "t0", T.boolean])
    (T.forAlls ["t0"] $ T.either_ (T.var "t0") T.boolean),
  checkTest "either of list" []
    (left $ list [int32 1, int32 2])
    (tylam "t0" $ tyapps (left $ list [int32 1, int32 2]) [T.list T.int32, T.var "t0"])
    (T.forAlls ["t0"] $ T.either_ (T.list T.int32) (T.var "t0")),
  checkTest "list of eithers" []
    (list [left $ string "a", right $ int32 1, left $ string "b"])
    (list [tyapps (left $ string "a") [T.string, T.int32], tyapps (right $ int32 1) [T.string, T.int32], tyapps (left $ string "b") [T.string, T.int32]])
    (T.list $ T.either_ T.string T.int32)]

eithersWithComplexTypesTests :: TTermDefinition TestGroup
eithersWithComplexTypesTests = define "eithersWithComplexTypesTests" $
  subgroup "Eithers with complex types" [
  checkTest "either with record on left" []
    (left $ record TestTypes.testTypePersonName [
      "firstName">: string "Alice",
      "lastName">: string "Smith",
      "age">: int32 30])
    (tylam "t0" $ tyapps (left $ record TestTypes.testTypePersonName [
      "firstName">: string "Alice",
      "lastName">: string "Smith",
      "age">: int32 30]) [Core.typeVariable TestTypes.testTypePersonName, T.var "t0"])
    (T.forAlls ["t0"] $ T.either_ (Core.typeVariable TestTypes.testTypePersonName) (T.var "t0")),
  checkTest "either with record on right" []
    (right $ record TestTypes.testTypePersonName [
      "firstName">: string "Bob",
      "lastName">: string "Jones",
      "age">: int32 25])
    (tylam "t0" $ tyapps (right $ record TestTypes.testTypePersonName [
      "firstName">: string "Bob",
      "lastName">: string "Jones",
      "age">: int32 25]) [T.var "t0", Core.typeVariable TestTypes.testTypePersonName])
    (T.forAlls ["t0"] $ T.either_ (T.var "t0") (Core.typeVariable TestTypes.testTypePersonName))]

------ Optionals ------

optionalsTests :: TTermDefinition TestGroup
optionalsTests = define "optionalsTests" $
  supergroup "Optionals" [
  monomorphicOptionalsTests,
  polymorphicOptionalsTests,
  optionalsInComplexContextsTests,
  nestedOptionalsTests,
  optionalsWithComplexTypesTests]

monomorphicOptionalsTests :: TTermDefinition TestGroup
monomorphicOptionalsTests = define "monomorphicOptionalsTests" $
  subgroup "Monomorphic optionals" [
  checkTest "nothing" []
    (optional nothing)
    (tylam "t0" $ tyapp (optional nothing) (T.var "t0"))
    (T.forAll "t0" $ T.optional $ T.var "t0"),
  noChange "just int"
    (optional $ just $ int32 42)
    (T.optional T.int32),
  noChange "just string"
    (optional $ just $ string "hello")
    (T.optional T.string),
  noChange "just boolean"
    (optional $ just $ boolean True)
    (T.optional T.boolean)]

polymorphicOptionalsTests :: TTermDefinition TestGroup
polymorphicOptionalsTests = define "polymorphicOptionalsTests" $
  subgroup "Polymorphic optionals" [
  checkTest "optional from lambda" []
    (lambda "x" $ optional $ just $ var "x")
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ optional $ just $ var "x")
    (T.forAll "t0" $ T.function (T.var "t0") (T.optional $ T.var "t0")),
  checkTest "nothing from lambda" []
    (lambda "x" $ optional nothing)
    (tylams ["t0", "t1"] $ lambdaTyped "x" (T.var "t0") $ tyapp (optional nothing) (T.var "t1"))
    (T.forAlls ["t0", "t1"] $ T.function (T.var "t0") (T.optional $ T.var "t1")),
  checkTest "conditional optional" []
    (lambda "x" $ lambda "flag" $
      primitive _logic_ifElse @@ var "flag" @@
        (optional $ just $ var "x") @@
        (optional nothing))
    (tylams ["t0"] $ lambdaTyped "x" (T.var "t0") $ lambdaTyped "flag" T.boolean $
      tyapp (primitive _logic_ifElse) (T.optional $ T.var "t0") @@ var "flag" @@
        (optional $ just $ var "x") @@
        (tyapp (optional nothing) (T.var "t0")))
    (T.forAlls ["t0"] $ T.function (T.var "t0") (T.function T.boolean (T.optional $ T.var "t0")))]

optionalsInComplexContextsTests :: TTermDefinition TestGroup
optionalsInComplexContextsTests = define "optionalsInComplexContextsTests" $
  subgroup "Optionals in complex contexts" [
  checkTest "optional in record" []
    (record TestTypes.testTypeBuddyListAName [
      "head">: string "first",
      "tail">: optional $ just $ record TestTypes.testTypeBuddyListBName [
        "head">: string "second",
        "tail">: optional nothing]])
    (tyapp (record TestTypes.testTypeBuddyListAName [
      "head">: string "first",
      "tail">: optional $ just $ tyapp (record TestTypes.testTypeBuddyListBName [
        "head">: string "second",
        "tail">: tyapp (optional nothing) (T.apply (Core.typeVariable TestTypes.testTypeBuddyListAName) T.string)]) T.string]) T.string)
    (T.apply (Core.typeVariable TestTypes.testTypeBuddyListAName) T.string),
  checkTest "optional in let binding" []
    (lets ["maybeValue" >: optional $ just $ int32 42] $
      var "maybeValue")
    (letsTyped [("maybeValue", optional $ just $ int32 42, T.mono $ T.optional T.int32)] $
      var "maybeValue")
    (T.optional T.int32)]

nestedOptionalsTests :: TTermDefinition TestGroup
nestedOptionalsTests = define "nestedOptionalsTests" $
  subgroup "Nested optionals" [
  noChange "optional of optional"
    (optional $ just $ optional $ just $ string "nested")
    (T.optional $ T.optional T.string),
  noChange "optional of list"
    (optional $ just $ list [int32 1, int32 2, int32 3])
    (T.optional $ T.list T.int32),
  checkTest "list of optionals" []
    (list [optional $ just $ string "a", optional nothing, optional $ just $ string "b"])
    (list [optional $ just $ string "a", tyapp (optional nothing) T.string, optional $ just $ string "b"])
    (T.list $ T.optional T.string)]

optionalsWithComplexTypesTests :: TTermDefinition TestGroup
optionalsWithComplexTypesTests = define "optionalsWithComplexTypesTests" $
  subgroup "Optionals with complex types" [
  noChange "optional map"
    (optional $ just $ mapTerm [(string "key", int32 42)])
    (T.optional $ T.map T.string T.int32)]

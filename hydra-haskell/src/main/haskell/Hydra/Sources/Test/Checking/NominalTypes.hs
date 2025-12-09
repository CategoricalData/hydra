
-- | Nominal type checking test cases: records, unions, wrapped terms, and eliminations (projections, case statements, unwrapping)
module Hydra.Sources.Test.Checking.NominalTypes where

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


module_ :: Module
module_ = Module (Namespace "hydra.test.checking.nominalTypes") elements
    [TestGraph.module_]
    kernelTypesModules
    (Just "Nominal type checking test cases: records, unions, field access, injection, projection")
  where
    elements = [
      Phantoms.toBinding allTests,
      Phantoms.toBinding recordsTests,
      Phantoms.toBinding monomorphicRecordsTests,
      Phantoms.toBinding polymorphicRecordsTests,
      Phantoms.toBinding recordsInComplexContextsTests,
      Phantoms.toBinding multiParameterPolymorphicRecordsTests,
      Phantoms.toBinding recordEliminationsTests,
      Phantoms.toBinding simpleRecordProjectionsTests,
      Phantoms.toBinding recordProjectionsAppliedToRecordsTests,
      Phantoms.toBinding polymorphicRecordProjectionsTests,
      Phantoms.toBinding polymorphicRecordProjectionsAppliedTests,
      Phantoms.toBinding recordProjectionsWithVariablesTests,
      Phantoms.toBinding recordProjectionsInComplexContextsTests,
      Phantoms.toBinding multiParameterPolymorphicProjectionsTests,
      Phantoms.toBinding higherOrderRecordProjectionsTests,
      Phantoms.toBinding recursiveRecordProjectionsTests,
      Phantoms.toBinding recordProjectionsWithMutualRecursionTests,
      Phantoms.toBinding unionsTests,
      Phantoms.toBinding simpleUnionInjectionsTests,
      Phantoms.toBinding unionInjectionsWithDataTests,
      Phantoms.toBinding polymorphicUnionInjectionsTests,
      Phantoms.toBinding polymorphicRecursiveUnionInjectionsTests,
      Phantoms.toBinding polymorphicUnionsFromLambdaTests,
      Phantoms.toBinding unionsInComplexContextsTests,
      Phantoms.toBinding multiParameterPolymorphicInjectionsTests,
      Phantoms.toBinding unionEliminationsTests,
      Phantoms.toBinding simpleUnitVariantEliminationsTests,
      Phantoms.toBinding unionEliminationsWithDataTests,
      Phantoms.toBinding polymorphicUnionEliminationsTests,
      Phantoms.toBinding simplePolymorphicUnionTests,
      Phantoms.toBinding usingUnionPolymorphicRecursiveTests,
      Phantoms.toBinding usingKernelTypesTests,
      Phantoms.toBinding unionEliminationsWithDefaultsTests,
      Phantoms.toBinding nestedUnionEliminationsTests,
      Phantoms.toBinding unionEliminationsInComplexContextsTests,
      Phantoms.toBinding multiParameterPolymorphicCaseStatementsTests,
      Phantoms.toBinding higherOrderUnionEliminationsTests,
      Phantoms.toBinding recursiveUnionEliminationsTests,
      Phantoms.toBinding wrappedTermsTests,
      Phantoms.toBinding monomorphicWrappedTermsTests,
      Phantoms.toBinding polymorphicWrappedTermsTests,
      Phantoms.toBinding wrappedTermsInComplexContextsTests,
      Phantoms.toBinding nestedWrappedTermsTests,
      Phantoms.toBinding multipleWrappingLevelsTests,
      Phantoms.toBinding multiParameterPolymorphicWrappersTests,
      Phantoms.toBinding wrapEliminationsTests,
      Phantoms.toBinding monomorphicUnwrappingTests,
      Phantoms.toBinding polymorphicUnwrappingTests,
      Phantoms.toBinding unwrapEliminationsInApplicationsTests,
      Phantoms.toBinding unwrapInComplexContextsTests,
      Phantoms.toBinding multiParameterPolymorphicUnwrappersTests,
      Phantoms.toBinding chainedUnwrappingTests,
      Phantoms.toBinding multipleUnwrapOperationsTests,
      Phantoms.toBinding eliminationsTests,
      Phantoms.toBinding projectionsWithVariablesTests]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

allTests :: TBinding TestGroup
allTests = define "allTests" $
  supergroup "Nominal types" [
  recordsTests,
  unionsTests,
  wrappedTermsTests,
  eliminationsTests]

------ Helper functions ------

-- Helper function to create a type checking test case
checkTest :: String -> [Tag] -> TTerm Term -> TTerm Term -> TTerm Type -> TTerm TestCaseWithMetadata
checkTest name tags input outputTerm outputType = testCaseWithMetadata (Phantoms.string name)
  (testCaseTypeChecking $ typeCheckingTestCase input outputTerm outputType) Phantoms.nothing (Phantoms.list $ tag . unTag <$> tags)

-- Helper for tests where the term doesn't change during type checking
noChange :: String -> TTerm Term -> TTerm Type -> TTerm TestCaseWithMetadata
noChange name term typ = checkTest name [] term term typ

-- Create a TestCase inject for type checking
testCaseTypeChecking :: TTerm TypeCheckingTestCase -> TTerm TestCase
testCaseTypeChecking = Phantoms.inject _TestCase _TestCase_typeChecking

-- Create a TypeCheckingTestCase record
typeCheckingTestCase :: TTerm Term -> TTerm Term -> TTerm Type -> TTerm TypeCheckingTestCase
typeCheckingTestCase input outputTerm outputType = Phantoms.record _TypeCheckingTestCase [
  Phantoms.field _TypeCheckingTestCase_input input,
  Phantoms.field _TypeCheckingTestCase_outputTerm outputTerm,
  Phantoms.field _TypeCheckingTestCase_outputType outputType]

------ Records ------

recordsTests :: TBinding TestGroup
recordsTests = define "recordsTests" $
  supergroup "Records" [
  monomorphicRecordsTests,
  polymorphicRecordsTests,
  recordsInComplexContextsTests,
  multiParameterPolymorphicRecordsTests]

monomorphicRecordsTests :: TBinding TestGroup
monomorphicRecordsTests = define "monomorphicRecordsTests" $
  subgroup "Monomorphic records" [
  noChange "latlon record"
    (record (name "LatLon") [
      "lat" >: float32 19.5429,
      "lon" >: float32 (-155.6659)])
    (T.var "LatLon"),
  checkTest "latlon with variable" []
    (lambda "x" $ record (name "LatLon") [
      "lat" >: float32 19.5429,
      "lon" >: var "x"])
    (lambdaTyped "x" T.float32 $ record (name "LatLon") [
      "lat" >: float32 19.5429,
      "lon" >: var "x"])
    (T.function T.float32 (T.var "LatLon")),
  noChange "person record"
    (record (name "Person") [
      "firstName" >: string "Alice",
      "lastName" >: string "Smith",
      "age" >: int32 30])
    (T.var "Person"),
  noChange "empty record"
    (record (name "Unit") [])
    (T.var "Unit"),
  checkTest "person with variables" []
    (lambda "name" $ lambda "age" $ record (name "Person") [
      "firstName" >: var "name",
      "lastName" >: string "Doe",
      "age" >: var "age"])
    (lambdaTyped "name" T.string $ lambdaTyped "age" T.int32 $ record (name "Person") [
      "firstName" >: var "name",
      "lastName" >: string "Doe",
      "age" >: var "age"])
    (T.function T.string (T.function T.int32 (T.var "Person")))]

polymorphicRecordsTests :: TBinding TestGroup
polymorphicRecordsTests = define "polymorphicRecordsTests" $
  subgroup "Polymorphic records" [
  checkTest "latlon poly float" []
    (record (name "LatLonPoly") [
      "lat" >: float32 19.5429,
      "lon" >: float32 (-155.6659)])
    (tyapp (record (name "LatLonPoly") [
      "lat" >: float32 19.5429,
      "lon" >: float32 (-155.6659)]) T.float32)
    (T.apply (T.var "LatLonPoly") T.float32),
  checkTest "latlon poly int64" []
    (record (name "LatLonPoly") [
      "lat" >: int64 195429,
      "lon" >: int64 (-1556659)])
    (tyapp (record (name "LatLonPoly") [
      "lat" >: int64 195429,
      "lon" >: int64 (-1556659)]) T.int64)
    (T.apply (T.var "LatLonPoly") T.int64),
  checkTest "latlon poly variable" []
    (lambda "x" $ record (name "LatLonPoly") [
      "lat" >: var "x",
      "lon" >: var "x"])
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ tyapp (record (name "LatLonPoly") [
      "lat" >: var "x",
      "lon" >: var "x"]) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.var "t0") (T.apply (T.var "LatLonPoly") (T.var "t0"))),
  checkTest "buddylist string" []
    (record (name "BuddyListA") [
      "head" >: string "first",
      "tail" >: optional nothing])
    (tyapp (record (name "BuddyListA") [
      "head" >: string "first",
      "tail" >: tyapp (optional nothing) (T.apply (T.var "BuddyListB") T.string)]) T.string)
    (T.apply (T.var "BuddyListA") T.string),
  checkTest "buddylist variable" []
    (lambda "x" $ record (name "BuddyListA") [
      "head" >: var "x",
      "tail" >: optional nothing])
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ tyapp (record (name "BuddyListA") [
      "head" >: var "x",
      "tail" >: tyapp (optional nothing) (T.apply (T.var "BuddyListB") (T.var "t0"))]) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.var "t0") (T.apply (T.var "BuddyListA") (T.var "t0")))]

recordsInComplexContextsTests :: TBinding TestGroup
recordsInComplexContextsTests = define "recordsInComplexContextsTests" $
  subgroup "Records in complex contexts" [
  checkTest "records in tuple" []
    (tuple [
      record (name "Person") [
        "firstName" >: string "Bob",
        "lastName" >: string "Jones",
        "age" >: int32 25],
      record (name "LatLon") [
        "lat" >: float32 1.0,
        "lon" >: float32 2.0]])
    (tyapps (pair
      (record (name "Person") [
        "firstName" >: string "Bob",
        "lastName" >: string "Jones",
        "age" >: int32 25])
      (record (name "LatLon") [
        "lat" >: float32 1.0,
        "lon" >: float32 2.0])) [T.var "Person", T.var "LatLon"])
    (T.pair (T.var "Person") (T.var "LatLon")),
  checkTest "poly records in tuple" []
    (tuple [
      record (name "LatLonPoly") [
        "lat" >: int32 1,
        "lon" >: int32 2],
      record (name "BuddyListA") [
        "head" >: string "test",
        "tail" >: optional nothing]])
    (tyapps (pair
      (tyapp (record (name "LatLonPoly") [
        "lat" >: int32 1,
        "lon" >: int32 2]) T.int32)
      (tyapp (record (name "BuddyListA") [
        "head" >: string "test",
        "tail" >: tyapp (optional nothing) (T.apply (T.var "BuddyListB") T.string)]) T.string))
      [T.apply (T.var "LatLonPoly") T.int32, T.apply (T.var "BuddyListA") T.string])
    (T.pair
      (T.apply (T.var "LatLonPoly") T.int32)
      (T.apply (T.var "BuddyListA") T.string)),
  checkTest "recursive record" []
    (record (name "IntList") [
      "head" >: int32 42,
      "tail" >: optional (Phantoms.just (
        record (name "IntList") [
          "head" >: int32 43,
          "tail" >: optional nothing]))])
    (record (name "IntList") [
      "head" >: int32 42,
      "tail" >: optional (Phantoms.just (
        record (name "IntList") [
          "head" >: int32 43,
          "tail" >: tyapp (optional nothing) (T.var "IntList")]))])
    (T.var "IntList")]

multiParameterPolymorphicRecordsTests :: TBinding TestGroup
multiParameterPolymorphicRecordsTests = define "multiParameterPolymorphicRecordsTests" $
  subgroup "Multi-parameter polymorphic records" [
  checkTest "triple with three monomorphic types" []
    (record (name "Triple") [
      "first" >: int32 1,
      "second" >: string "middle",
      "third" >: boolean True])
    (tyapps (record (name "Triple") [
      "first" >: int32 1,
      "second" >: string "middle",
      "third" >: boolean True]) [T.int32, T.string, T.boolean])
    (T.applys (T.var "Triple") [T.int32, T.string, T.boolean]),
  checkTest "triple with PersonOrSomething containing map" []
    (lambda "k" $ lambda "v" $
      record (name "Triple") [
        "first" >: string "prefix",
        "second" >: inject (TestTypes.testTypePersonOrSomethingName) "other"
          (mapTerm [(var "k", var "v")]),
        "third" >: int32 999])
    (tylams ["t0", "t1"] $
      lambdaTyped "k" (T.var "t0") $
      lambdaTyped "v" (T.var "t1") $
      tyapps (record (name "Triple") [
        "first" >: string "prefix",
        "second" >: tyapp (inject (TestTypes.testTypePersonOrSomethingName) "other"
          (mapTerm [(var "k", var "v")])) (T.map (T.var "t0") (T.var "t1")),
        "third" >: int32 999])
        [T.string,
         T.apply (T.var "PersonOrSomething") (T.map (T.var "t0") (T.var "t1")),
         T.int32])
    (T.forAlls ["t0", "t1"] $
      T.function (T.var "t0") $
      T.function (T.var "t1") $
      T.applys (T.var "Triple")
        [T.string,
         T.apply (T.var "PersonOrSomething") (T.map (T.var "t0") (T.var "t1")),
         T.int32])]

------ Record eliminations ------

recordEliminationsTests :: TBinding TestGroup
recordEliminationsTests = define "recordEliminationsTests" $
  supergroup "Record eliminations" [
  simpleRecordProjectionsTests,
  recordProjectionsAppliedToRecordsTests,
  polymorphicRecordProjectionsTests,
  polymorphicRecordProjectionsAppliedTests,
  recordProjectionsWithVariablesTests,
  recordProjectionsInComplexContextsTests,
  multiParameterPolymorphicProjectionsTests,
  higherOrderRecordProjectionsTests,
  recursiveRecordProjectionsTests,
  recordProjectionsWithMutualRecursionTests,
  projectionsWithVariablesTests]

simpleRecordProjectionsTests :: TBinding TestGroup
simpleRecordProjectionsTests = define "simpleRecordProjectionsTests" $
  subgroup "Simple record projections" [
  noChange "project firstName from Person"
    (project (TestTypes.testTypePersonName) (name "firstName"))
    (T.function (T.var "Person") T.string),
  noChange "project lastName from Person"
    (project (TestTypes.testTypePersonName) (name "lastName"))
    (T.function (T.var "Person") T.string),
  noChange "project age from Person"
    (project (TestTypes.testTypePersonName) (name "age"))
    (T.function (T.var "Person") T.int32),
  noChange "project lat from LatLon"
    (project (TestTypes.testTypeLatLonName) (name "lat"))
    (T.function (T.var "LatLon") T.float32),
  noChange "project lon from LatLon"
    (project (TestTypes.testTypeLatLonName) (name "lon"))
    (T.function (T.var "LatLon") T.float32)]

recordProjectionsAppliedToRecordsTests :: TBinding TestGroup
recordProjectionsAppliedToRecordsTests = define "recordProjectionsAppliedToRecordsTests" $
  subgroup "Record projections applied to records" [
  noChange "project firstName applied to person record"
    (project (TestTypes.testTypePersonName) (name "firstName") @@
     record (name "Person") [
       "firstName" >: string "Alice",
       "lastName" >: string "Smith",
       "age" >: int32 30])
    T.string,
  noChange "project age applied to person record"
    (project (TestTypes.testTypePersonName) (name "age") @@
     record (name "Person") [
       "firstName" >: string "Bob",
       "lastName" >: string "Jones",
       "age" >: int32 25])
    T.int32,
  noChange "project lat applied to LatLon record"
    (project (TestTypes.testTypeLatLonName) (name "lat") @@
     record (name "LatLon") [
       "lat" >: float32 40.7128,
       "lon" >: float32 (-74.0060)])
    T.float32]

polymorphicRecordProjectionsTests :: TBinding TestGroup
polymorphicRecordProjectionsTests = define "polymorphicRecordProjectionsTests" $
  subgroup "Polymorphic record projections" [
  checkTest "project lat from polymorphic LatLonPoly" []
    (project (TestTypes.testTypeLatLonPolyName) (name "lat"))
    (tylam "t0" $ tyapp (project (TestTypes.testTypeLatLonPolyName) (name "lat")) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.apply (T.var "LatLonPoly") (T.var "t0")) (T.var "t0")),
  checkTest "project lon from polymorphic LatLonPoly" []
    (project (TestTypes.testTypeLatLonPolyName) (name "lon"))
    (tylam "t0" $ tyapp (project (TestTypes.testTypeLatLonPolyName) (name "lon")) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.apply (T.var "LatLonPoly") (T.var "t0")) (T.var "t0")),
  checkTest "project head from BuddyListA" []
    (project (TestTypes.testTypeBuddyListAName) (name "head"))
    (tylam "t0" $ tyapp (project (TestTypes.testTypeBuddyListAName) (name "head")) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.apply (T.var "BuddyListA") (T.var "t0")) (T.var "t0")),
  checkTest "project tail from BuddyListA" []
    (project (TestTypes.testTypeBuddyListAName) (name "tail"))
    (tylam "t0" $ tyapp (project (TestTypes.testTypeBuddyListAName) (name "tail")) (T.var "t0"))
    (T.forAll "t0" $ T.function
      (T.apply (T.var "BuddyListA") (T.var "t0"))
      (T.optional (T.apply (T.var "BuddyListB") (T.var "t0"))))]

polymorphicRecordProjectionsAppliedTests :: TBinding TestGroup
polymorphicRecordProjectionsAppliedTests = define "polymorphicRecordProjectionsAppliedTests" $
  subgroup "Polymorphic record projections applied" [
  checkTest "project lat from LatLonPoly with int32" []
    (project (TestTypes.testTypeLatLonPolyName) (name "lat") @@
     record (name "LatLonPoly") [
       "lat" >: int32 40,
       "lon" >: int32 (-74)])
    (tyapp (project (TestTypes.testTypeLatLonPolyName) (name "lat")) T.int32 @@
     tyapp (record (name "LatLonPoly") [
       "lat" >: int32 40,
       "lon" >: int32 (-74)]) T.int32)
    T.int32,
  checkTest "project lon from LatLonPoly with float64" []
    (project (TestTypes.testTypeLatLonPolyName) (name "lon") @@
     record (name "LatLonPoly") [
       "lat" >: float64 40.7128,
       "lon" >: float64 (-74.0060)])
    (tyapp (project (TestTypes.testTypeLatLonPolyName) (name "lon")) T.float64 @@
     tyapp (record (name "LatLonPoly") [
       "lat" >: float64 40.7128,
       "lon" >: float64 (-74.0060)]) T.float64)
    T.float64,
  checkTest "project head from BuddyListA with string" []
    (project (TestTypes.testTypeBuddyListAName) (name "head") @@
     record (name "BuddyListA") [
       "head" >: string "Alice",
       "tail" >: optional nothing])
    (tyapp (project (TestTypes.testTypeBuddyListAName) (name "head")) T.string @@
     tyapp (record (name "BuddyListA") [
       "head" >: string "Alice",
       "tail" >: tyapp (optional nothing) (T.apply (T.var "BuddyListB") T.string)]) T.string)
    T.string]

recordProjectionsWithVariablesTests :: TBinding TestGroup
recordProjectionsWithVariablesTests = define "recordProjectionsWithVariablesTests" $
  subgroup "Record projections with variables" [
  checkTest "project from lambda parameter" []
    (lambda "person" $ project (TestTypes.testTypePersonName) (name "firstName") @@ var "person")
    (lambdaTyped "person" (T.var "Person") $ project (TestTypes.testTypePersonName) (name "firstName") @@ var "person")
    (T.function (T.var "Person") T.string),
  checkTest "project from polymorphic lambda parameter" []
    (lambda "coords" $ project (TestTypes.testTypeLatLonPolyName) (name "lat") @@ var "coords")
    (tylam "t0" $ lambdaTyped "coords" (T.apply (T.var "LatLonPoly") (T.var "t0")) $ tyapp (project (TestTypes.testTypeLatLonPolyName) (name "lat")) (T.var "t0") @@ var "coords")
    (T.forAll "t0" $ T.function (T.apply (T.var "LatLonPoly") (T.var "t0")) (T.var "t0")),
  checkTest "multiple projections from same record" []
    (lambda "person" $
     tuple [project (TestTypes.testTypePersonName) (name "firstName") @@ var "person",
            project (TestTypes.testTypePersonName) (name "lastName") @@ var "person"])
    (lambdaTyped "person" (T.var "Person") $
     tyapps (pair
       (project (TestTypes.testTypePersonName) (name "firstName") @@ var "person")
       (project (TestTypes.testTypePersonName) (name "lastName") @@ var "person")) [T.string, T.string])
    (T.function (T.var "Person") (T.pair T.string T.string))]

recordProjectionsInComplexContextsTests :: TBinding TestGroup
recordProjectionsInComplexContextsTests = define "recordProjectionsInComplexContextsTests" $
  subgroup "Record projections in complex contexts" [
  checkTest "projection in let binding" []
    (lets ["person">: record (name "Person") [
             "firstName" >: string "Charlie",
             "lastName" >: string "Brown",
             "age" >: int32 35],
           "getName">: project (TestTypes.testTypePersonName) (name "firstName")] $
          var "getName" @@ var "person")
    (letsTyped [("person", record (name "Person") [
                   "firstName" >: string "Charlie",
                   "lastName" >: string "Brown",
                   "age" >: int32 35],
                 T.mono $ T.var "Person"),
                ("getName", project (TestTypes.testTypePersonName) (name "firstName"),
                 T.mono $ T.function (T.var "Person") T.string)] $
      var "getName" @@ var "person")
    T.string,
  checkTest "projection in tuple" []
    (tuple [project (TestTypes.testTypePersonName) (name "firstName"),
            project (TestTypes.testTypePersonName) (name "age")])
    (tyapps (pair
      (project (TestTypes.testTypePersonName) (name "firstName"))
      (project (TestTypes.testTypePersonName) (name "age")))
      [T.function (T.var "Person") T.string, T.function (T.var "Person") T.int32])
    (T.pair (T.function (T.var "Person") T.string) (T.function (T.var "Person") T.int32)),
  noChange "projection in list"
    (list [project (TestTypes.testTypePersonName) (name "firstName"),
           project (TestTypes.testTypePersonName) (name "lastName")])
    (T.list (T.function (T.var "Person") T.string))]

multiParameterPolymorphicProjectionsTests :: TBinding TestGroup
multiParameterPolymorphicProjectionsTests = define "multiParameterPolymorphicProjectionsTests" $
  subgroup "Multi-parameter polymorphic projections" [
  checkTest "project first from Triple" []
    (project (TestTypes.testTypeTripleName) (name "first"))
    (tylams ["t0", "t1", "t2"] $ tyapps (project (TestTypes.testTypeTripleName) (name "first")) [T.var "t0", T.var "t1", T.var "t2"])
    (T.forAlls ["t0", "t1", "t2"] $
      T.function
        (T.applys (Core.typeVariable $ TestTypes.testTypeTripleName) [T.var "t0", T.var "t1", T.var "t2"])
        (T.var "t0")),
  checkTest "project second from Triple applied" []
    (project (TestTypes.testTypeTripleName) (name "second") @@
      record (TestTypes.testTypeTripleName) [
        "first">: int32 1,
        "second">: string "middle",
        "third">: boolean True])
    (tyapps (project (TestTypes.testTypeTripleName) (name "second")) [T.int32, T.string, T.boolean] @@
      tyapps (record (TestTypes.testTypeTripleName) [
        "first">: int32 1,
        "second">: string "middle",
        "third">: boolean True]) [T.int32, T.string, T.boolean])
    T.string,
  checkTest "project from Triple and use second field, which is another polymorphic record" []
    (lambda "triple" $ lambda "key" $
      match (TestTypes.testTypePersonOrSomethingName) nothing [
        "person">: lambda "p" $ Core.termMaybe nothing,
        "other">: lambda "m" $ primitive _maps_lookup @@ var "key" @@ var "m"] @@
      (project (TestTypes.testTypeTripleName) (name "second") @@ var "triple"))
    (tylams ["t0", "t1", "t2", "t3"] $
      lambdaTyped "triple"
        (T.applys (Core.typeVariable $ TestTypes.testTypeTripleName)
          [T.var "t0",
           T.apply (Core.typeVariable $ TestTypes.testTypePersonOrSomethingName) (T.map (T.var "t1") (T.var "t2")),
           T.var "t3"]) $
      lambdaTyped "key" (T.var "t1") $
      tyapp (match (TestTypes.testTypePersonOrSomethingName) nothing [
        "person">: lambdaTyped "p" (Core.typeVariable $ TestTypes.testTypePersonName) (tyapp (Core.termMaybe nothing) (T.var "t2")),
        "other">: lambdaTyped "m" (T.map (T.var "t1") (T.var "t2")) $
          tyapps (primitive _maps_lookup) [T.var "t1", T.var "t2"] @@ var "key" @@ var "m"]) (T.map (T.var "t1") (T.var "t2")) @@
      (tyapps (project (TestTypes.testTypeTripleName) (name "second"))
        [T.var "t0",
         T.apply (Core.typeVariable $ TestTypes.testTypePersonOrSomethingName) (T.map (T.var "t1") (T.var "t2")),
         T.var "t3"] @@ var "triple"))
    (T.forAlls ["t0", "t1", "t2", "t3"] $
      T.function
        (T.applys (Core.typeVariable $ TestTypes.testTypeTripleName)
          [T.var "t0",
           T.apply (Core.typeVariable $ TestTypes.testTypePersonOrSomethingName) (T.map (T.var "t1") (T.var "t2")),
           T.var "t3"])
        (T.function (T.var "t1") (T.optional (T.var "t2"))))]

higherOrderRecordProjectionsTests :: TBinding TestGroup
higherOrderRecordProjectionsTests = define "higherOrderRecordProjectionsTests" $
  subgroup "Higher-order record projections" [
  checkTest "map projection over list of records" []
    (primitive _lists_map @@ (project (TestTypes.testTypePersonName) (name "firstName")) @@
     list [record (TestTypes.testTypePersonName) [
             "firstName">: (string "Alice"),
             "lastName">: (string "Smith"),
             "age">: (int32 30)],
           record (TestTypes.testTypePersonName) [
             "firstName">: (string "Bob"),
             "lastName">: (string "Jones"),
             "age">: (int32 25)]])
    (tyapps (primitive _lists_map) [Core.typeVariable $ TestTypes.testTypePersonName, T.string] @@ (project (TestTypes.testTypePersonName) (name "firstName")) @@
     list [record (TestTypes.testTypePersonName) [
             "firstName">: (string "Alice"),
             "lastName">: (string "Smith"),
             "age">: (int32 30)],
           record (TestTypes.testTypePersonName) [
             "firstName">: (string "Bob"),
             "lastName">: (string "Jones"),
             "age">: (int32 25)]])
    (T.list T.string),
  checkTest "map polymorphic projection" []
    (primitive _lists_map @@ (project (TestTypes.testTypeLatLonPolyName) (name "lat")) @@
     list [record (TestTypes.testTypeLatLonPolyName) [
             "lat">: (int32 40),
             "lon">: (int32 (-74))],
           record (TestTypes.testTypeLatLonPolyName) [
             "lat">: (int32 34),
             "lon">: (int32 (-118))]])
    (tyapps (primitive _lists_map) [T.apply (Core.typeVariable $ TestTypes.testTypeLatLonPolyName) T.int32, T.int32]
      @@ (tyapp (project (TestTypes.testTypeLatLonPolyName) (name "lat")) T.int32) @@
     list [tyapp (record (TestTypes.testTypeLatLonPolyName) [
             "lat">: (int32 40),
             "lon">: (int32 (-74))]) T.int32,
           tyapp (record (TestTypes.testTypeLatLonPolyName) [
             "lat">: (int32 34),
             "lon">: (int32 (-118))]) T.int32])
    (T.list T.int32),
  checkTest "filter using projection" []
    (primitive _lists_filter @@
     (lambda "person" $
      primitive _equality_gt @@
      (project (TestTypes.testTypePersonName) (name "age") @@ var "person") @@
      int32 30) @@
     list [record (TestTypes.testTypePersonName) [
             "firstName">: (string "Alice"),
             "lastName">: (string "Smith"),
             "age">: (int32 35)],
           record (TestTypes.testTypePersonName) [
             "firstName">: (string "Bob"),
             "lastName">: (string "Jones"),
             "age">: (int32 25)]])
    (tyapp (primitive _lists_filter) (Core.typeVariable $ TestTypes.testTypePersonName) @@
     (lambdaTyped "person" (Core.typeVariable $ TestTypes.testTypePersonName) $
      tyapp (primitive _equality_gt) T.int32 @@
      (project (TestTypes.testTypePersonName) (name "age") @@ var "person") @@
      int32 30) @@
     list [record (TestTypes.testTypePersonName) [
             "firstName">: (string "Alice"),
             "lastName">: (string "Smith"),
             "age">: (int32 35)],
           record (TestTypes.testTypePersonName) [
             "firstName">: (string "Bob"),
             "lastName">: (string "Jones"),
             "age">: (int32 25)]])
    (T.list (Core.typeVariable $ TestTypes.testTypePersonName))]

recursiveRecordProjectionsTests :: TBinding TestGroup
recursiveRecordProjectionsTests = define "recursiveRecordProjectionsTests" $
  subgroup "Recursive record projections" [
  checkTest "nested projection from recursive record" []
    (lambda "intList" $
     primitive _maybes_maybe @@
     int32 0 @@
     (project (TestTypes.testTypeIntListName) (name "head")) @@
     (project (TestTypes.testTypeIntListName) (name "tail") @@ var "intList"))
    (lambdaTyped "intList" (Core.typeVariable $ TestTypes.testTypeIntListName) $
     tyapps (primitive _maybes_maybe) [T.int32, Core.typeVariable $ TestTypes.testTypeIntListName] @@
     int32 0 @@
     (project (TestTypes.testTypeIntListName) (name "head")) @@
     (project (TestTypes.testTypeIntListName) (name "tail") @@ var "intList"))
    (T.function (Core.typeVariable $ TestTypes.testTypeIntListName) T.int32)]

recordProjectionsWithMutualRecursionTests :: TBinding TestGroup
recordProjectionsWithMutualRecursionTests = define "recordProjectionsWithMutualRecursionTests" $
  subgroup "Record projections with mutual recursion" [
  checkTest "project head from BuddyListA" []
    (project (TestTypes.testTypeBuddyListAName) (name "head"))
    (tylam "t0" $ tyapp (project (TestTypes.testTypeBuddyListAName) (name "head")) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.apply (Core.typeVariable $ TestTypes.testTypeBuddyListAName) (T.var "t0")) (T.var "t0")),
  checkTest "project tail from BuddyListB" []
    (project (TestTypes.testTypeBuddyListBName) (name "tail"))
    (tylam "t0" $ tyapp (project (TestTypes.testTypeBuddyListBName) (name "tail")) (T.var "t0"))
    (T.forAll "t0" $ T.function
      (T.apply (Core.typeVariable $ TestTypes.testTypeBuddyListBName) (T.var "t0"))
      (T.optional (T.apply (Core.typeVariable $ TestTypes.testTypeBuddyListAName) (T.var "t0")))),
  checkTest "chained projections across mutual recursion" []
    (lambda "listA" $
      primitive _maybes_maybe @@
      Core.termMaybe nothing @@
      (lambda "listB" $
        primitive _maybes_maybe @@
        Core.termMaybe nothing @@
        (project (TestTypes.testTypeBuddyListAName) (name "tail")) @@
        (project (TestTypes.testTypeBuddyListBName) (name "tail") @@ var "listB")) @@
      (project (TestTypes.testTypeBuddyListAName) (name "tail") @@ var "listA"))
    (tylam "t0" $ lambdaTyped "listA" (T.apply (Core.typeVariable $ TestTypes.testTypeBuddyListAName) (T.var "t0")) $
      tyapps (primitive _maybes_maybe) [T.optional (T.apply (Core.typeVariable $ TestTypes.testTypeBuddyListBName) (T.var "t0")), T.apply (Core.typeVariable $ TestTypes.testTypeBuddyListBName) (T.var "t0")] @@
      tyapp (Core.termMaybe nothing) (T.apply (Core.typeVariable $ TestTypes.testTypeBuddyListBName) (T.var "t0")) @@
      (lambdaTyped "listB" (T.apply (Core.typeVariable $ TestTypes.testTypeBuddyListBName) (T.var "t0")) $
        tyapps (primitive _maybes_maybe) [T.optional (T.apply (Core.typeVariable $ TestTypes.testTypeBuddyListBName) (T.var "t0")), T.apply (Core.typeVariable $ TestTypes.testTypeBuddyListAName) (T.var "t0")] @@
        tyapp (Core.termMaybe nothing) (T.apply (Core.typeVariable $ TestTypes.testTypeBuddyListBName) (T.var "t0")) @@
        (tyapp (project (TestTypes.testTypeBuddyListAName) (name "tail")) (T.var "t0")) @@
        (tyapp (project (TestTypes.testTypeBuddyListBName) (name "tail")) (T.var "t0") @@ var "listB")) @@
      (tyapp (project (TestTypes.testTypeBuddyListAName) (name "tail")) (T.var "t0") @@ var "listA"))
    (T.forAll "t0" $ T.function
      (T.apply (Core.typeVariable $ TestTypes.testTypeBuddyListAName) (T.var "t0"))
      (T.optional (T.apply (Core.typeVariable $ TestTypes.testTypeBuddyListBName) (T.var "t0"))))]

------ Unions ------

unionsTests :: TBinding TestGroup
unionsTests = define "unionsTests" $
  supergroup "Unions" [
  simpleUnionInjectionsTests,
  unionInjectionsWithDataTests,
  polymorphicUnionInjectionsTests,
  polymorphicRecursiveUnionInjectionsTests,
  polymorphicUnionsFromLambdaTests,
  unionsInComplexContextsTests,
  multiParameterPolymorphicInjectionsTests]

simpleUnionInjectionsTests :: TBinding TestGroup
simpleUnionInjectionsTests = define "simpleUnionInjectionsTests" $
  subgroup "Simple union injections" [
  noChange "inject into Comparison lessThan variant"
    (injectUnit (TestTypes.testTypeComparisonName) "lessThan")
    (Core.typeVariable $ TestTypes.testTypeComparisonName),
  noChange "inject into Comparison equalTo variant"
    (injectUnit (TestTypes.testTypeComparisonName) "equalTo")
    (Core.typeVariable $ TestTypes.testTypeComparisonName),
  noChange "inject into Comparison greaterThan variant"
    (injectUnit (TestTypes.testTypeComparisonName) "greaterThan")
    (Core.typeVariable $ TestTypes.testTypeComparisonName)]

unionInjectionsWithDataTests :: TBinding TestGroup
unionInjectionsWithDataTests = define "unionInjectionsWithDataTests" $
  subgroup "Union injections with data" [
  noChange "inject into Number int variant"
    (inject (TestTypes.testTypeNumberName) "int" (int32 42))
    (Core.typeVariable $ TestTypes.testTypeNumberName),
  noChange "inject into Number float variant"
    (inject (TestTypes.testTypeNumberName) "float" (float32 3.14))
    (Core.typeVariable $ TestTypes.testTypeNumberName),
  noChange "inject into Timestamp unixTimeMillis variant"
    (inject (TestTypes.testTypeTimestampName) "unixTimeMillis" (uint64 1609459200000))
    (Core.typeVariable $ TestTypes.testTypeTimestampName),
  noChange "inject into Timestamp date variant"
    (inject (TestTypes.testTypeTimestampName) "date" (string "2021-01-01"))
    (Core.typeVariable $ TestTypes.testTypeTimestampName)]

polymorphicUnionInjectionsTests :: TBinding TestGroup
polymorphicUnionInjectionsTests = define "polymorphicUnionInjectionsTests" $
  subgroup "Polymorphic union injections" [
  checkTest "inject person into PersonOrSomething" []
    (inject (TestTypes.testTypePersonOrSomethingName) "person"
      (record (TestTypes.testTypePersonName) [
        "firstName">: string "Alice",
        "lastName">: string "Smith",
        "age">: int32 30]))
    (tylam "t0" $ tyapp (inject (TestTypes.testTypePersonOrSomethingName) "person"
      (record (TestTypes.testTypePersonName) [
        "firstName">: string "Alice",
        "lastName">: string "Smith",
        "age">: int32 30])) (T.var "t0"))
    (T.forAll "t0" $ T.apply (Core.typeVariable $ TestTypes.testTypePersonOrSomethingName) (T.var "t0")),
  checkTest "inject string into PersonOrSomething other variant" []
    (inject (TestTypes.testTypePersonOrSomethingName) "other" (string "something else"))
    (tyapp (inject (TestTypes.testTypePersonOrSomethingName) "other" (string "something else")) T.string)
    (T.apply (Core.typeVariable $ TestTypes.testTypePersonOrSomethingName) T.string),
  checkTest "inject int into PersonOrSomething other variant" []
    (inject (TestTypes.testTypePersonOrSomethingName) "other" (int32 42))
    (tyapp (inject (TestTypes.testTypePersonOrSomethingName) "other" (int32 42)) T.int32)
    (T.apply (Core.typeVariable $ TestTypes.testTypePersonOrSomethingName) T.int32)]

polymorphicRecursiveUnionInjectionsTests :: TBinding TestGroup
polymorphicRecursiveUnionInjectionsTests = define "polymorphicRecursiveUnionInjectionsTests" $
  subgroup "Polymorphic recursive union injections" [
  checkTest "inject boolean into UnionPolymorphicRecursive" []
    (inject (TestTypes.testTypeUnionPolymorphicRecursiveName) "bool" (boolean True))
    (tylam "t0" $ tyapp (inject (TestTypes.testTypeUnionPolymorphicRecursiveName) "bool" (boolean True)) (T.var "t0"))
    (T.forAll "t0" $ T.apply (Core.typeVariable $ TestTypes.testTypeUnionPolymorphicRecursiveName) (T.var "t0")),
  checkTest "inject string value into UnionPolymorphicRecursive" []
    (inject (TestTypes.testTypeUnionPolymorphicRecursiveName) "value" (string "test"))
    (tyapp (inject (TestTypes.testTypeUnionPolymorphicRecursiveName) "value" (string "test")) T.string)
    (T.apply (Core.typeVariable $ TestTypes.testTypeUnionPolymorphicRecursiveName) T.string),
  checkTest "inject int value into UnionPolymorphicRecursive" []
    (inject (TestTypes.testTypeUnionPolymorphicRecursiveName) "value" (int32 123))
    (tyapp (inject (TestTypes.testTypeUnionPolymorphicRecursiveName) "value" (int32 123)) T.int32)
    (T.apply (Core.typeVariable $ TestTypes.testTypeUnionPolymorphicRecursiveName) T.int32)]

polymorphicUnionsFromLambdaTests :: TBinding TestGroup
polymorphicUnionsFromLambdaTests = define "polymorphicUnionsFromLambdaTests" $
  subgroup "Polymorphic unions from lambda" [
  checkTest "lambda creating PersonOrSomething other variant" []
    (lambda "x" $ inject (TestTypes.testTypePersonOrSomethingName) "other" (var "x"))
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ tyapp (inject (TestTypes.testTypePersonOrSomethingName) "other" (var "x")) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.var "t0") (T.apply (Core.typeVariable $ TestTypes.testTypePersonOrSomethingName) (T.var "t0"))),
  checkTest "lambda creating UnionPolymorphicRecursive value variant" []
    (lambda "x" $ inject (TestTypes.testTypeUnionPolymorphicRecursiveName) "value" (var "x"))
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ tyapp (inject (TestTypes.testTypeUnionPolymorphicRecursiveName) "value" (var "x")) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.var "t0") (T.apply (Core.typeVariable $ TestTypes.testTypeUnionPolymorphicRecursiveName) (T.var "t0")))]

unionsInComplexContextsTests :: TBinding TestGroup
unionsInComplexContextsTests = define "unionsInComplexContextsTests" $
  subgroup "Unions in complex contexts" [
  checkTest "union in tuple" []
    (tuple [inject (TestTypes.testTypeNumberName) "int" (int32 42),
            string "context"])
    (tyapps (pair
      (inject (TestTypes.testTypeNumberName) "int" (int32 42))
      (string "context"))
      [Core.typeVariable $ TestTypes.testTypeNumberName, T.string])
    (T.pair (Core.typeVariable $ TestTypes.testTypeNumberName) T.string),
  noChange "union in list"
    (list [inject (TestTypes.testTypeNumberName) "int" (int32 1),
           inject (TestTypes.testTypeNumberName) "float" (float32 2.5)])
    (T.list $ Core.typeVariable $ TestTypes.testTypeNumberName),
  checkTest "polymorphic union in let binding" []
    (lets ["value">: inject (TestTypes.testTypePersonOrSomethingName) "other" (string "test")] $
          var "value")
    (letsTyped [("value", tyapp (inject (TestTypes.testTypePersonOrSomethingName) "other" (string "test")) T.string,
                 T.mono $ T.apply (Core.typeVariable $ TestTypes.testTypePersonOrSomethingName) T.string)] $
      var "value")
    (T.apply (Core.typeVariable $ TestTypes.testTypePersonOrSomethingName) T.string)]

multiParameterPolymorphicInjectionsTests :: TBinding TestGroup
multiParameterPolymorphicInjectionsTests = define "multiParameterPolymorphicInjectionsTests" $
  subgroup "Multi-parameter polymorphic injections" [
  checkTest "either left with int" []
    (inject (TestTypes.testTypeEitherName) "left" (int32 42))
    (tylam "t0" $ tyapps (inject (TestTypes.testTypeEitherName) "left" (int32 42)) [T.int32, T.var "t0"])
    (T.forAll "t0" $ T.applys (Core.typeVariable $ TestTypes.testTypeEitherName) [T.int32, T.var "t0"]),
  checkTest "either right with string" []
    (inject (TestTypes.testTypeEitherName) "right" (string "hello"))
    (tylam "t0" $ tyapps (inject (TestTypes.testTypeEitherName) "right" (string "hello")) [T.var "t0", T.string])
    (T.forAll "t0" $ T.applys (Core.typeVariable $ TestTypes.testTypeEitherName) [T.var "t0", T.string]),
  checkTest "either containing LatLonPoly in list" []
    (inject (TestTypes.testTypeEitherName) "right"
      (list [record (TestTypes.testTypeLatLonPolyName) [
        "lat">: int32 40,
        "lon">: int32 (-74)]]))
    (tylam "t0" $ tyapps (inject (TestTypes.testTypeEitherName) "right"
      (list [tyapp (record (TestTypes.testTypeLatLonPolyName) [
        "lat">: int32 40,
        "lon">: int32 (-74)]) T.int32]))
      [T.var "t0", T.list (T.apply (Core.typeVariable $ TestTypes.testTypeLatLonPolyName) T.int32)])
    (T.forAll "t0" $ T.applys (Core.typeVariable $ TestTypes.testTypeEitherName) [T.var "t0", T.list (T.apply (Core.typeVariable $ TestTypes.testTypeLatLonPolyName) T.int32)]),
  checkTest "either in triple in map with shared type variables" []
    (lambda "x0" $ lambda "x1" $ lambda "x2" $
      Terms.map $ Phantoms.map $ M.singleton (string "key") $
        record (TestTypes.testTypeTripleName) [
          "first">: inject (TestTypes.testTypeEitherName) "left" (var "x0"),
          "second">: inject (TestTypes.testTypeEitherName) "left" (var "x0"),
          "third">: inject (TestTypes.testTypeEitherName) "right" (var "x1")])
    (tylams ["t0", "t1", "t2", "t3", "t4", "t5"] $
      lambdaTyped "x0" (T.var "t0") $
      lambdaTyped "x1" (T.var "t1") $
      lambdaTyped "x2" (T.var "t2") $
      Terms.map $ Phantoms.map $ M.singleton (string "key") $
        tyapps (record (TestTypes.testTypeTripleName) [
          "first">: tyapps (inject (TestTypes.testTypeEitherName) "left" (var "x0")) [T.var "t0", T.var "t3"],
          "second">: tyapps (inject (TestTypes.testTypeEitherName) "left" (var "x0")) [T.var "t0", T.var "t4"],
          "third">: tyapps (inject (TestTypes.testTypeEitherName) "right" (var "x1")) [T.var "t5", T.var "t1"]])
        [T.applys (Core.typeVariable $ TestTypes.testTypeEitherName) [T.var "t0", T.var "t3"],
         T.applys (Core.typeVariable $ TestTypes.testTypeEitherName) [T.var "t0", T.var "t4"],
         T.applys (Core.typeVariable $ TestTypes.testTypeEitherName) [T.var "t5", T.var "t1"]])
    (T.forAlls ["t0", "t1", "t2", "t3", "t4", "t5"] $
      T.function (T.var "t0") $
      T.function (T.var "t1") $
      T.function (T.var "t2") $
      T.map T.string $
        T.applys (Core.typeVariable $ TestTypes.testTypeTripleName)
          [T.applys (Core.typeVariable $ TestTypes.testTypeEitherName) [T.var "t0", T.var "t3"],
           T.applys (Core.typeVariable $ TestTypes.testTypeEitherName) [T.var "t0", T.var "t4"],
           T.applys (Core.typeVariable $ TestTypes.testTypeEitherName) [T.var "t5", T.var "t1"]])]

------ Union eliminations ------

unionEliminationsTests :: TBinding TestGroup
unionEliminationsTests = define "unionEliminationsTests" $
  supergroup "Union eliminations" [
  simpleUnitVariantEliminationsTests,
  unionEliminationsWithDataTests,
  polymorphicUnionEliminationsTests,
  unionEliminationsWithDefaultsTests,
  nestedUnionEliminationsTests,
  unionEliminationsInComplexContextsTests,
  multiParameterPolymorphicCaseStatementsTests,
  higherOrderUnionEliminationsTests,
  recursiveUnionEliminationsTests]

simpleUnitVariantEliminationsTests :: TBinding TestGroup
simpleUnitVariantEliminationsTests = define "simpleUnitVariantEliminationsTests" $
  subgroup "Simple unit inject eliminations" [
  checkTest "match Comparison with all cases" []
    (match (TestTypes.testTypeComparisonName) nothing [
      "lessThan">: lambda "x" (string "less"),
      "equalTo">: lambda "x" (string "equal"),
      "greaterThan">: lambda "x" (string "greater")])
    (match (TestTypes.testTypeComparisonName) nothing [
      "lessThan">: lambdaTyped "x" T.unit (string "less"),
      "equalTo">: lambdaTyped "x" T.unit (string "equal"),
      "greaterThan">: lambdaTyped "x" T.unit (string "greater")])
    (T.function (Core.typeVariable $ TestTypes.testTypeComparisonName) T.string),
  checkTest "match Comparison returning int32" []
    (match (TestTypes.testTypeComparisonName) nothing [
      "lessThan">: lambda "x" (int32 (-1)),
      "equalTo">: lambda "x" (int32 0),
      "greaterThan">: lambda "x" (int32 1)])
    (match (TestTypes.testTypeComparisonName) nothing [
      "lessThan">: lambdaTyped "x" T.unit (int32 (-1)),
      "equalTo">: lambdaTyped "x" T.unit (int32 0),
      "greaterThan">: lambdaTyped "x" T.unit (int32 1)])
    (T.function (Core.typeVariable $ TestTypes.testTypeComparisonName) T.int32),
  checkTest "match applied to Comparison variant" []
    (match (TestTypes.testTypeComparisonName) nothing [
      "lessThan">: lambda "x" (string "less"),
      "equalTo">: lambda "x" (string "equal"),
      "greaterThan">: lambda "x" (string "greater")] @@
     injectUnit (TestTypes.testTypeComparisonName) "equalTo")
    (match (TestTypes.testTypeComparisonName) nothing [
      "lessThan">: lambdaTyped "x" T.unit (string "less"),
      "equalTo">: lambdaTyped "x" T.unit (string "equal"),
      "greaterThan">: lambdaTyped "x" T.unit (string "greater")] @@
     injectUnit (TestTypes.testTypeComparisonName) "equalTo")
    T.string]

unionEliminationsWithDataTests :: TBinding TestGroup
unionEliminationsWithDataTests = define "unionEliminationsWithDataTests" $
  subgroup "Union eliminations with data" [
  checkTest "match Number extracting int values" []
    (match (TestTypes.testTypeNumberName) nothing [
      "int">: lambda "i" (var "i"),
      "float">: lambda "f" (int32 0)])
    (match (TestTypes.testTypeNumberName) nothing [
      "int">: lambdaTyped "i" T.int32 (var "i"),
      "float">: lambdaTyped "f" T.float32 (int32 0)])
    (T.function (Core.typeVariable $ TestTypes.testTypeNumberName) T.int32),
  checkTest "match Number converting to string" []
    (match (TestTypes.testTypeNumberName) nothing [
      "int">: lambda "i" (primitive _literals_showInt32 @@ var "i"),
      "float">: lambda "f" (primitive _literals_showFloat32 @@ var "f")])
    (match (TestTypes.testTypeNumberName) nothing [
      "int">: lambdaTyped "i" T.int32 (primitive _literals_showInt32 @@ var "i"),
      "float">: lambdaTyped "f" T.float32 (primitive _literals_showFloat32 @@ var "f")])
    (T.function (Core.typeVariable $ TestTypes.testTypeNumberName) T.string),
  checkTest "match Number applied to int variant" []
    (match (TestTypes.testTypeNumberName) nothing [
      "int">: lambda "i" (primitive _math_add @@ var "i" @@ int32 10),
      "float">: lambda "f" (int32 0)] @@
     inject (TestTypes.testTypeNumberName) "int" (int32 42))
    (match (TestTypes.testTypeNumberName) nothing [
      "int">: lambdaTyped "i" T.int32 (primitive _math_add @@ var "i" @@ int32 10),
      "float">: lambdaTyped "f" T.float32 (int32 0)] @@
     inject (TestTypes.testTypeNumberName) "int" (int32 42))
    T.int32,
  checkTest "match Timestamp with mixed data types" []
    (match (TestTypes.testTypeTimestampName) nothing [
      "unixTimeMillis">: lambda "millis" (primitive _literals_showUint64 @@ var "millis"),
      "date">: lambda "dateStr" (var "dateStr")])
    (match (TestTypes.testTypeTimestampName) nothing [
      "unixTimeMillis">: lambdaTyped "millis" T.uint64 (primitive _literals_showUint64 @@ var "millis"),
      "date">: lambdaTyped "dateStr" T.string (var "dateStr")])
    (T.function (Core.typeVariable $ TestTypes.testTypeTimestampName) T.string)]

polymorphicUnionEliminationsTests :: TBinding TestGroup
polymorphicUnionEliminationsTests = define "polymorphicUnionEliminationsTests" $
  supergroup "Polymorphic union eliminations" [
  simplePolymorphicUnionTests,
  usingUnionPolymorphicRecursiveTests,
  usingKernelTypesTests]

simplePolymorphicUnionTests :: TBinding TestGroup
simplePolymorphicUnionTests = define "simplePolymorphicUnionTests" $
  subgroup "Simple polymorphic unions" [
  checkTest "match PersonOrSomething with string" []
    (match (TestTypes.testTypePersonOrSomethingName) nothing [
      "person">: lambda "p" (project (TestTypes.testTypePersonName) (name "firstName") @@ var "p"),
      "other">: lambda "x" (var "x")])
    (tyapp (match (TestTypes.testTypePersonOrSomethingName) nothing [
      "person">: lambdaTyped "p" (Core.typeVariable $ TestTypes.testTypePersonName) (project (TestTypes.testTypePersonName) (name "firstName") @@ var "p"),
      "other">: lambdaTyped "x" T.string (var "x")]) T.string)
    (T.function (T.apply (Core.typeVariable $ TestTypes.testTypePersonOrSomethingName) T.string) T.string),
  checkTest "match PersonOrSomething instantiated with string" []
    (match (TestTypes.testTypePersonOrSomethingName) nothing [
      "person">: lambda "p" (project (TestTypes.testTypePersonName) (name "firstName") @@ var "p"),
      "other">: lambda "x" (var "x")] @@
     inject (TestTypes.testTypePersonOrSomethingName) "other" (string "test"))
    (tyapp (match (TestTypes.testTypePersonOrSomethingName) nothing [
      "person">: lambdaTyped "p" (Core.typeVariable $ TestTypes.testTypePersonName) (project (TestTypes.testTypePersonName) (name "firstName") @@ var "p"),
      "other">: lambdaTyped "x" T.string (var "x")]) T.string @@
     tyapp (inject (TestTypes.testTypePersonOrSomethingName) "other" (string "test")) T.string)
    T.string]

usingUnionPolymorphicRecursiveTests :: TBinding TestGroup
usingUnionPolymorphicRecursiveTests = define "usingUnionPolymorphicRecursiveTests" $
  subgroup "using UnionPolymorphicRecursive" [
  checkTest "non-applied UnionPolymorphicRecursive" []
    (lets [
      "test">: (match (TestTypes.testTypeUnionPolymorphicRecursiveName)
        (just $ string "other") [
        "value">: lambda "i" $ primitive _literals_showInt32 @@ var "i"])] $
      var "test")
    (letsTyped [
        ("test",
         tyapp (match (TestTypes.testTypeUnionPolymorphicRecursiveName)
           (just $ string "other") [
           "value">: lambdaTyped "i" T.int32 $ primitive _literals_showInt32 @@ var "i"]) T.int32,
         T.mono $ T.function (T.apply (Core.typeVariable $ TestTypes.testTypeUnionPolymorphicRecursiveName) T.int32) T.string)] $
      var "test")
    (T.function (T.apply (Core.typeVariable $ TestTypes.testTypeUnionPolymorphicRecursiveName) T.int32) T.string),
  checkTest "applied UnionPolymorphicRecursive with int32" []
    (lets [
      "test">: (match (TestTypes.testTypeUnionPolymorphicRecursiveName)
          (just $ string "other") [
          "value">: lambda "i" $ primitive _literals_showInt32 @@ var "i"])
        @@ (inject (TestTypes.testTypeUnionPolymorphicRecursiveName) "value" $ int32 42)] $
      var "test")
    (letsTyped [
      ("test",
       tyapp (match (TestTypes.testTypeUnionPolymorphicRecursiveName)
           (just $ string "other") [
           "value">: lambdaTyped "i" T.int32 $ primitive _literals_showInt32 @@ var "i"]) T.int32
         @@ tyapp (inject (TestTypes.testTypeUnionPolymorphicRecursiveName) "value" $ int32 42) T.int32,
       T.mono T.string)] $
      var "test")
    T.string,
  checkTest "applied UnionPolymorphicRecursive with int32 in lambda" []
    (lets [
      "test">: lambda "x" $ match (TestTypes.testTypeUnionPolymorphicRecursiveName)
          (just $ string "other") [
          "value">: lambda "i" $ primitive _literals_showInt32 @@ var "i"]
        @@ var "x"] $
      var "test")
    (letsTyped [
      ("test",
       lambdaTyped "x" (T.apply (Core.typeVariable $ TestTypes.testTypeUnionPolymorphicRecursiveName) T.int32) $
         tyapp (match (TestTypes.testTypeUnionPolymorphicRecursiveName)
             (just $ string "other") [
             "value">: lambdaTyped "i" T.int32 $ primitive _literals_showInt32 @@ var "i"]) T.int32
           @@ var "x",
       T.mono $ T.function (T.apply (Core.typeVariable $ TestTypes.testTypeUnionPolymorphicRecursiveName) T.int32) T.string)] $
      var "test")
    (T.function (T.apply (Core.typeVariable $ TestTypes.testTypeUnionPolymorphicRecursiveName) T.int32) T.string),
  checkTest "applied generic UnionPolymorphicRecursive in lambda" []
    (lets [
      "test">: lambda "x" $ match (TestTypes.testTypeUnionPolymorphicRecursiveName)
          (just $ string "other") [
          "value">: lambda "ignored" $ string "foo"]
        @@ var "x"] $
      var "test")
    (tylam "t0" $ letsTyped [
      ("test",
       tylam "t1" $ lambdaTyped "x" (T.apply (Core.typeVariable $ TestTypes.testTypeUnionPolymorphicRecursiveName) (T.var "t1")) $
         tyapp (match (TestTypes.testTypeUnionPolymorphicRecursiveName)
             (just $ string "other") [
             "value">: lambdaTyped "ignored" (T.var "t1") $ string "foo"]) (T.var "t1")
           @@ var "x",
       T.poly ["t1"] $ T.function (T.apply (Core.typeVariable $ TestTypes.testTypeUnionPolymorphicRecursiveName) (T.var "t1")) T.string)] $
      tyapp (var "test") $ T.var "t0")
    (T.forAll "t0" $ T.function (T.apply (Core.typeVariable $ TestTypes.testTypeUnionPolymorphicRecursiveName) (T.var "t0")) T.string)]

usingKernelTypesTests :: TBinding TestGroup
usingKernelTypesTests = define "usingKernelTypesTests" $
  subgroup "Using kernel types" [
  checkTest "case statement on CoderDirection applied to argument" []
    (lambda "dir" $
      lambda "coder" $
        match (name "hydra.coders.CoderDirection")
          nothing [
          "encode">: lambda "_" $
            lambda "v12" $
              project (name "hydra.compute.Coder") (name "encode")
                @@ var "coder" @@ var "v12",
          "decode">: lambda "_" $
            lambda "v12" $
              project (name "hydra.compute.Coder") (name "decode")
                @@ var "coder" @@ var "v12"]
          @@ var "dir")
    (tylams ["t0", "t1"] $
      lambdaTyped "dir" (T.var "hydra.coders.CoderDirection") $
        lambdaTyped "coder" (T.applys (T.var "hydra.compute.Coder") (T.var <$> ["t0", "t0", "t1", "t1"])) $
          match (name "hydra.coders.CoderDirection")
            nothing [
            "encode">: lambdaTyped "_" T.unit $
              lambdaTyped "v12" (T.var "t1") $
                tyapps (project (name "hydra.compute.Coder") (name "encode")) (T.var <$> ["t0", "t0", "t1", "t1"])
                  @@ var "coder" @@ var "v12",
            "decode">: lambdaTyped "_" T.unit $
              lambdaTyped "v12" (T.var "t1") $
                tyapps (project (name "hydra.compute.Coder") (name "decode")) (T.var <$> ["t0", "t0", "t1", "t1"])
                  @@ var "coder" @@ var "v12"]
          @@ var "dir")
    (T.forAlls ["t0", "t1"] $
      T.functionMany [
        T.var "hydra.coders.CoderDirection",
        T.applys (T.var "hydra.compute.Coder") (T.var <$> ["t0", "t0", "t1", "t1"]),
        T.var "t1",
        T.applys (Core.typeVariable $ name "hydra.compute.Flow") [T.var "t0", T.var "t1"]])]

unionEliminationsWithDefaultsTests :: TBinding TestGroup
unionEliminationsWithDefaultsTests = define "unionEliminationsWithDefaultsTests" $
  subgroup "Union eliminations with defaults" [
  checkTest "match Comparison with default case" []
    (match (TestTypes.testTypeComparisonName) (just (string "unknown")) [
      "lessThan">: lambda "x" (string "less"),
      "equalTo">: lambda "x" (string "equal")])
    (match (TestTypes.testTypeComparisonName) (just (string "unknown")) [
      "lessThan">: lambdaTyped "x" T.unit (string "less"),
      "equalTo">: lambdaTyped "x" T.unit (string "equal")])
    (T.function (Core.typeVariable $ TestTypes.testTypeComparisonName) T.string),
  checkTest "match Number with default case" []
    (match (TestTypes.testTypeNumberName) (just (int32 (-1))) [
      "int">: lambda "i" (var "i")])
    (match (TestTypes.testTypeNumberName) (just (int32 (-1))) [
      "int">: lambdaTyped "i" T.int32 (var "i")])
    (T.function (Core.typeVariable $ TestTypes.testTypeNumberName) T.int32),
  checkTest "match UnionMonomorphic with default" []
    (match (TestTypes.testTypeUnionMonomorphicName) (just (string "fallback")) [
      "bool">: lambda "b" (primitive _literals_showBoolean @@ var "b"),
      "string">: lambda "s" (var "s")])
    (match (TestTypes.testTypeUnionMonomorphicName) (just (string "fallback")) [
      "bool">: lambdaTyped "b" T.boolean (primitive _literals_showBoolean @@ var "b"),
      "string">: lambdaTyped "s" T.string (var "s")])
    (T.function (Core.typeVariable $ TestTypes.testTypeUnionMonomorphicName) T.string)]

nestedUnionEliminationsTests :: TBinding TestGroup
nestedUnionEliminationsTests = define "nestedUnionEliminationsTests" $
  subgroup "Nested union eliminations" [
  checkTest "nested match statements" []
    (match (TestTypes.testTypePersonOrSomethingName) nothing [
      "person">: lambda "p" (project (TestTypes.testTypePersonName) (name "firstName") @@ var "p"),
      "other">: lambda "x" (
        match (TestTypes.testTypeNumberName) nothing [
          "int">: lambda "i" (primitive _literals_showInt32 @@ var "i"),
          "float">: lambda "f" (primitive _literals_showFloat32 @@ var "f")] @@
        var "x")])
    (tyapp (match (TestTypes.testTypePersonOrSomethingName) nothing [
      "person">: lambdaTyped "p" (Core.typeVariable $ TestTypes.testTypePersonName) (project (TestTypes.testTypePersonName) (name "firstName") @@ var "p"),
      "other">: lambdaTyped "x" (Core.typeVariable $ TestTypes.testTypeNumberName) (
        match (TestTypes.testTypeNumberName) nothing [
          "int">: lambdaTyped "i" T.int32 (primitive _literals_showInt32 @@ var "i"),
          "float">: lambdaTyped "f" T.float32 (primitive _literals_showFloat32 @@ var "f")] @@
        var "x")]) (Core.typeVariable $ TestTypes.testTypeNumberName))
    (T.function (T.apply (Core.typeVariable $ TestTypes.testTypePersonOrSomethingName) (Core.typeVariable $ TestTypes.testTypeNumberName)) T.string),
  checkTest "match in tuple" []
    (tuple [
      match (TestTypes.testTypeComparisonName) nothing [
        "lessThan">: lambda "x" (int32 1),
        "equalTo">: lambda "x" (int32 0),
        "greaterThan">: lambda "x" (int32 (-1))],
      string "context"])
    (tyapps (pair
      (match (TestTypes.testTypeComparisonName) nothing [
        "lessThan">: lambdaTyped "x" T.unit (int32 1),
        "equalTo">: lambdaTyped "x" T.unit (int32 0),
        "greaterThan">: lambdaTyped "x" T.unit (int32 (-1))])
      (string "context"))
      [T.function (Core.typeVariable $ TestTypes.testTypeComparisonName) T.int32, T.string])
    (T.pair (T.function (Core.typeVariable $ TestTypes.testTypeComparisonName) T.int32) T.string)]

unionEliminationsInComplexContextsTests :: TBinding TestGroup
unionEliminationsInComplexContextsTests = define "unionEliminationsInComplexContextsTests" $
  subgroup "Union eliminations in complex contexts" [
  checkTest "match in let binding" []
    (lets ["matcher">: match (TestTypes.testTypeComparisonName) nothing [
             "lessThan">: lambda "x" (string "less"),
             "equalTo">: lambda "x" (string "equal"),
             "greaterThan">: lambda "x" (string "greater")]] $
          var "matcher")
    (letsTyped [("matcher", match (TestTypes.testTypeComparisonName) nothing [
                   "lessThan">: lambdaTyped "x" T.unit (string "less"),
                   "equalTo">: lambdaTyped "x" T.unit (string "equal"),
                   "greaterThan">: lambdaTyped "x" T.unit (string "greater")],
                 T.mono $ T.function (Core.typeVariable $ TestTypes.testTypeComparisonName) T.string)] $
      var "matcher")
    (T.function (Core.typeVariable $ TestTypes.testTypeComparisonName) T.string),
  checkTest "match in record" []
    (record (TestTypes.testTypePersonName) [
      "firstName">: (match (TestTypes.testTypePersonOrSomethingName) nothing [
        "person">: lambda "p" (project (TestTypes.testTypePersonName) (name "firstName") @@ var "p"),
        "other">: lambda "x" (var "x")] @@
       inject (TestTypes.testTypePersonOrSomethingName) "other" (string "John")),
      "lastName">: (string "Doe"),
      "age">: (int32 30)])
    (record (TestTypes.testTypePersonName) [
      "firstName">: (tyapp (match (TestTypes.testTypePersonOrSomethingName) nothing [
        "person">: lambdaTyped "p" (Core.typeVariable $ TestTypes.testTypePersonName) (project (TestTypes.testTypePersonName) (name "firstName") @@ var "p"),
        "other">: lambdaTyped "x" T.string (var "x")]) T.string @@
       tyapp (inject (TestTypes.testTypePersonOrSomethingName) "other" (string "John")) T.string),
      "lastName">: (string "Doe"),
      "age">: (int32 30)])
    (Core.typeVariable $ TestTypes.testTypePersonName),
  checkTest "match with polymorphic result in list" []
    (list [
      match (TestTypes.testTypePersonOrSomethingName) nothing [
        "person">: lambda "p" (project (TestTypes.testTypePersonName) (name "age") @@ var "p"),
        "other">: lambda "x" (var "x")] @@
      inject (TestTypes.testTypePersonOrSomethingName) "other" (int32 25),
      int32 30])
    (list [
      tyapp (match (TestTypes.testTypePersonOrSomethingName) nothing [
        "person">: lambdaTyped "p" (Core.typeVariable $ TestTypes.testTypePersonName) (project (TestTypes.testTypePersonName) (name "age") @@ var "p"),
        "other">: lambdaTyped "x" T.int32 (var "x")]) T.int32 @@
      tyapp (inject (TestTypes.testTypePersonOrSomethingName) "other" (int32 25)) T.int32,
      int32 30])
    (T.list T.int32)]

multiParameterPolymorphicCaseStatementsTests :: TBinding TestGroup
multiParameterPolymorphicCaseStatementsTests = define "multiParameterPolymorphicCaseStatementsTests" $
  subgroup "Multi-parameter polymorphic case statements" [
  checkTest "case Either converting both to string" []
    (match (TestTypes.testTypeEitherName) nothing [
      "left">: lambda "x" $ primitive _literals_showInt32 @@ var "x",
      "right">: lambda "y" $ primitive _literals_showFloat32 @@ var "y"])
    (tyapps (match (TestTypes.testTypeEitherName) nothing [
      "left">: lambdaTyped "x" T.int32 (primitive _literals_showInt32 @@ var "x"),
      "right">: lambdaTyped "y" T.float32 (primitive _literals_showFloat32 @@ var "y")]) [T.int32, T.float32])
    (T.function
      (T.applys (Core.typeVariable $ TestTypes.testTypeEitherName) [T.int32, T.float32])
      T.string),
  checkTest "case Either applied to injection" []
    (match (TestTypes.testTypeEitherName) nothing [
      "left">: lambda "n" $ primitive _math_add @@ var "n" @@ int32 1,
      "right">: lambda "s" $ primitive _strings_length @@ var "s"] @@
     inject (TestTypes.testTypeEitherName) "left" (int32 42))
    (tyapps (match (TestTypes.testTypeEitherName) nothing [
      "left">: lambdaTyped "n" T.int32 (primitive _math_add @@ var "n" @@ int32 1),
      "right">: lambdaTyped "s" T.string (primitive _strings_length @@ var "s")]) [T.int32, T.string] @@
     tyapps (inject (TestTypes.testTypeEitherName) "left" (int32 42)) [T.int32, T.string])
    T.int32,
  checkTest "case Either with Triple and nested projections" []
    (lambda "triple" $
      match (TestTypes.testTypeEitherName) nothing [
        "left">: lambda "coords" $
          project (TestTypes.testTypeLatLonPolyName) (name "lat") @@ var "coords",
        "right">: lambda "t" $
          project (TestTypes.testTypeTripleName) (name "first") @@ var "t"] @@
      (project (TestTypes.testTypeTripleName) (name "second") @@ var "triple"))
    (tylams ["t0", "t1", "t2", "t3", "t4"] $
      lambdaTyped "triple"
        (T.applys (Core.typeVariable $ TestTypes.testTypeTripleName)
          [T.var "t0",
           T.applys (Core.typeVariable $ TestTypes.testTypeEitherName)
             [T.apply (Core.typeVariable $ TestTypes.testTypeLatLonPolyName) (T.var "t1"),
              T.applys (Core.typeVariable $ TestTypes.testTypeTripleName) [T.var "t1", T.var "t2", T.var "t3"]],
           T.var "t4"]) $
      tyapps (match (TestTypes.testTypeEitherName) nothing [
        "left">: lambdaTyped "coords" (T.apply (Core.typeVariable $ TestTypes.testTypeLatLonPolyName) (T.var "t1")) $
          tyapp (project (TestTypes.testTypeLatLonPolyName) (name "lat")) (T.var "t1") @@ var "coords",
        "right">: lambdaTyped "t" (T.applys (Core.typeVariable $ TestTypes.testTypeTripleName) [T.var "t1", T.var "t2", T.var "t3"]) $
          tyapps (project (TestTypes.testTypeTripleName) (name "first")) [T.var "t1", T.var "t2", T.var "t3"] @@ var "t"])
        [T.apply (Core.typeVariable $ TestTypes.testTypeLatLonPolyName) (T.var "t1"),
         T.applys (Core.typeVariable $ TestTypes.testTypeTripleName) [T.var "t1", T.var "t2", T.var "t3"]] @@
      (tyapps (project (TestTypes.testTypeTripleName) (name "second"))
        [T.var "t0",
         T.applys (Core.typeVariable $ TestTypes.testTypeEitherName)
           [T.apply (Core.typeVariable $ TestTypes.testTypeLatLonPolyName) (T.var "t1"),
            T.applys (Core.typeVariable $ TestTypes.testTypeTripleName) [T.var "t1", T.var "t2", T.var "t3"]],
         T.var "t4"] @@ var "triple"))
    (T.forAlls ["t0", "t1", "t2", "t3", "t4"] $
      T.function
        (T.applys (Core.typeVariable $ TestTypes.testTypeTripleName)
          [T.var "t0",
           T.applys (Core.typeVariable $ TestTypes.testTypeEitherName)
             [T.apply (Core.typeVariable $ TestTypes.testTypeLatLonPolyName) (T.var "t1"),
              T.applys (Core.typeVariable $ TestTypes.testTypeTripleName) [T.var "t1", T.var "t2", T.var "t3"]],
           T.var "t4"])
        (T.var "t1")),
  checkTest "case Either with polymorphic let bindings" []
    (lets ["makeLeft">: lambda "x" $ inject (TestTypes.testTypeEitherName) "left" (var "x"),
           "makeRight">: lambda "y" $ inject (TestTypes.testTypeEitherName) "right" (var "y")] $
      lambda "flag" $
        match (TestTypes.testTypeEitherName) nothing [
          "left">: lambda "n" $ var "makeRight" @@ (primitive _math_add @@ var "n" @@ int32 10),
          "right">: lambda "s" $ var "makeLeft" @@ (primitive _strings_length @@ var "s")] @@
        var "flag")
    (letsTyped [("makeLeft", tylams ["t0", "t1"] $ lambdaTyped "x" (T.var "t0") $ tyapps (inject (TestTypes.testTypeEitherName) "left" (var "x")) [T.var "t0", T.var "t1"],
                 T.poly ["t0", "t1"] $ T.function (T.var "t0") (T.applys (Core.typeVariable $ TestTypes.testTypeEitherName) [T.var "t0", T.var "t1"])),
                ("makeRight", tylams ["t0", "t1"] $ lambdaTyped "y" (T.var "t0") $ tyapps (inject (TestTypes.testTypeEitherName) "right" (var "y")) [T.var "t1", T.var "t0"],
                 T.poly ["t0", "t1"] $ T.function (T.var "t0") (T.applys (Core.typeVariable $ TestTypes.testTypeEitherName) [T.var "t1", T.var "t0"]))] $
      lambdaTyped "flag" (T.applys (Core.typeVariable $ TestTypes.testTypeEitherName) [T.int32, T.string]) $
        tyapps (match (TestTypes.testTypeEitherName) nothing [
          "left">: lambdaTyped "n" T.int32 $ tyapps (var "makeRight") [T.int32, T.int32] @@ (primitive _math_add @@ var "n" @@ int32 10),
          "right">: lambdaTyped "s" T.string $ tyapps (var "makeLeft") [T.int32, T.int32] @@ (primitive _strings_length @@ var "s")]) [T.int32, T.string] @@
        var "flag")
    (T.function (T.applys (Core.typeVariable $ TestTypes.testTypeEitherName) [T.int32, T.string]) (T.applys (Core.typeVariable $ TestTypes.testTypeEitherName) [T.int32, T.int32]))]

higherOrderUnionEliminationsTests :: TBinding TestGroup
higherOrderUnionEliminationsTests = define "higherOrderUnionEliminationsTests" $
  subgroup "Higher-order union eliminations" [
  checkTest "map match over list" []
    (primitive _lists_map @@
     (match (TestTypes.testTypeComparisonName) nothing [
       "lessThan">: lambda "x" (string "less"),
       "equalTo">: lambda "x" (string "equal"),
       "greaterThan">: lambda "x" (string "greater")]) @@
     list [injectUnit (TestTypes.testTypeComparisonName) "lessThan",
           injectUnit (TestTypes.testTypeComparisonName) "equalTo"])
    (tyapps (primitive _lists_map) [Core.typeVariable $ TestTypes.testTypeComparisonName, T.string] @@
     (match (TestTypes.testTypeComparisonName) nothing [
       "lessThan">: lambdaTyped "x" T.unit (string "less"),
       "equalTo">: lambdaTyped "x" T.unit (string "equal"),
       "greaterThan">: lambdaTyped "x" T.unit (string "greater")]) @@
     list [injectUnit (TestTypes.testTypeComparisonName) "lessThan",
           injectUnit (TestTypes.testTypeComparisonName) "equalTo"])
    (T.list T.string),
  checkTest "compose match with other functions" []
    (lambda "comp" $
     primitive _strings_length @@
     (match (TestTypes.testTypeComparisonName) nothing [
       "lessThan">: lambda "x" (string "less"),
       "equalTo">: lambda "x" (string "equal"),
       "greaterThan">: lambda "x" (string "greater")] @@
      var "comp"))
    (lambdaTyped "comp" (Core.typeVariable $ TestTypes.testTypeComparisonName) $
     primitive _strings_length @@
     (match (TestTypes.testTypeComparisonName) nothing [
       "lessThan">: lambdaTyped "x" T.unit (string "less"),
       "equalTo">: lambdaTyped "x" T.unit (string "equal"),
       "greaterThan">: lambdaTyped "x" T.unit (string "greater")] @@
      var "comp"))
    (T.function (Core.typeVariable $ TestTypes.testTypeComparisonName) T.int32),
  checkTest "match in lambda body" []
    (lambda "unionValue" $
     match (TestTypes.testTypeNumberName) nothing [
       "int">: lambda "i" (primitive _math_add @@ var "i" @@ int32 1),
       "float">: lambda "f" (int32 0)] @@
     var "unionValue")
    (lambdaTyped "unionValue" (Core.typeVariable $ TestTypes.testTypeNumberName) $
     match (TestTypes.testTypeNumberName) nothing [
       "int">: lambdaTyped "i" T.int32 (primitive _math_add @@ var "i" @@ int32 1),
       "float">: lambdaTyped "f" T.float32 (int32 0)] @@
     var "unionValue")
    (T.function (Core.typeVariable $ TestTypes.testTypeNumberName) T.int32)]

recursiveUnionEliminationsTests :: TBinding TestGroup
recursiveUnionEliminationsTests = define "recursiveUnionEliminationsTests" $
  subgroup "Recursive union eliminations" [
  checkTest "match HydraType recursively" []
    (match (TestTypes.testTypeHydraTypeName) nothing [
      "literal">: lambda "lit" (
        match (TestTypes.testTypeHydraLiteralTypeName) nothing [
          "boolean">: lambda "b" (primitive _literals_showBoolean @@ var "b"),
          "string">: lambda "s" (var "s")] @@
        var "lit"),
      "list">: lambda "nested" (string "list")])
    (match (TestTypes.testTypeHydraTypeName) nothing [
      "literal">: lambdaTyped "lit" (Core.typeVariable $ TestTypes.testTypeHydraLiteralTypeName) (
        match (TestTypes.testTypeHydraLiteralTypeName) nothing [
          "boolean">: lambdaTyped "b" T.boolean (primitive _literals_showBoolean @@ var "b"),
          "string">: lambdaTyped "s" T.string (var "s")] @@
        var "lit"),
      "list">: lambdaTyped "nested" (Core.typeVariable $ TestTypes.testTypeHydraTypeName) (string "list")])
    (T.function (Core.typeVariable $ TestTypes.testTypeHydraTypeName) T.string)]

------ Wrapped terms ------

wrappedTermsTests :: TBinding TestGroup
wrappedTermsTests = define "wrappedTermsTests" $
  supergroup "Wrapped terms" [
  monomorphicWrappedTermsTests,
  polymorphicWrappedTermsTests,
  wrappedTermsInComplexContextsTests,
  nestedWrappedTermsTests,
  multipleWrappingLevelsTests,
  multiParameterPolymorphicWrappersTests]

monomorphicWrappedTermsTests :: TBinding TestGroup
monomorphicWrappedTermsTests = define "monomorphicWrappedTermsTests" $
  subgroup "Monomorphic wrapped terms" [
  noChange "string alias"
    (wrap (TestTypes.testTypeStringAliasName) (string "hello"))
    (Core.typeVariable $ TestTypes.testTypeStringAliasName),
  noChange "wrapped integer"
    (wrap (TestTypes.testTypeStringAliasName) (string "wrapped"))
    (Core.typeVariable $ TestTypes.testTypeStringAliasName),
  checkTest "wrapped in tuple" []
    (tuple [wrap (TestTypes.testTypeStringAliasName) (string "first"),
            string "second"])
    (tyapps (pair
      (wrap (TestTypes.testTypeStringAliasName) (string "first"))
      (string "second"))
      [Core.typeVariable $ TestTypes.testTypeStringAliasName, T.string])
    (T.pair (Core.typeVariable $ TestTypes.testTypeStringAliasName) T.string)]

polymorphicWrappedTermsTests :: TBinding TestGroup
polymorphicWrappedTermsTests = define "polymorphicWrappedTermsTests" $
  subgroup "Polymorphic wrapped terms" [
  checkTest "polymorphic wrapper with int" []
    (wrap (TestTypes.testTypePolymorphicWrapperName) (list [int32 1, int32 2]))
    (tyapp (wrap (TestTypes.testTypePolymorphicWrapperName) (list [int32 1, int32 2])) T.int32)
    (T.apply (Core.typeVariable $ TestTypes.testTypePolymorphicWrapperName) T.int32),
  checkTest "polymorphic wrapper with string" []
    (wrap (TestTypes.testTypePolymorphicWrapperName) (list [string "a", string "b"]))
    (tyapp (wrap (TestTypes.testTypePolymorphicWrapperName) (list [string "a", string "b"])) T.string)
    (T.apply (Core.typeVariable $ TestTypes.testTypePolymorphicWrapperName) T.string),
  checkTest "polymorphic wrapper from lambda" []
    (lambda "x" $ wrap (TestTypes.testTypePolymorphicWrapperName) (list [var "x"]))
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ tyapp (wrap (TestTypes.testTypePolymorphicWrapperName) (list [var "x"])) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.var "t0") (T.apply (Core.typeVariable $ TestTypes.testTypePolymorphicWrapperName) (T.var "t0")))]

wrappedTermsInComplexContextsTests :: TBinding TestGroup
wrappedTermsInComplexContextsTests = define "wrappedTermsInComplexContextsTests" $
  subgroup "Wrapped terms in complex contexts" [
  noChange "wrapped in record"
    (record (TestTypes.testTypePersonName) [
      "firstName">: (string "John"),
      "lastName">: (string "Doe"),
      "age">: (int32 30)])
    (Core.typeVariable $ TestTypes.testTypePersonName),
  checkTest "wrapped in let binding" []
    (lets ["alias">: wrap (TestTypes.testTypeStringAliasName) (string "test")] $
          var "alias")
    (letsTyped [("alias", wrap (TestTypes.testTypeStringAliasName) (string "test"),
                 T.mono $ Core.typeVariable $ TestTypes.testTypeStringAliasName)] $
      var "alias")
    (Core.typeVariable $ TestTypes.testTypeStringAliasName),
  noChange "wrapped in list"
    (list [wrap (TestTypes.testTypeStringAliasName) (string "first"),
           wrap (TestTypes.testTypeStringAliasName) (string "second")])
    (T.list $ Core.typeVariable $ TestTypes.testTypeStringAliasName)]

nestedWrappedTermsTests :: TBinding TestGroup
nestedWrappedTermsTests = define "nestedWrappedTermsTests" $
  subgroup "Nested wrapped terms" [
  checkTest "wrapped tuple" []
    (wrap (TestTypes.testTypePolymorphicWrapperName) (list [tuple [int32 1, string "a"]]))
    (tyapp (wrap (TestTypes.testTypePolymorphicWrapperName) (list [tyapps (pair (int32 1) (string "a")) [T.int32, T.string]])) (T.pair T.int32 T.string))
    (T.apply (Core.typeVariable $ TestTypes.testTypePolymorphicWrapperName) (T.pair T.int32 T.string)),
  checkTest "wrapped optional" []
    (wrap (TestTypes.testTypePolymorphicWrapperName) (list [Core.termMaybe $ just $ int32 42]))
    (tyapp (wrap (TestTypes.testTypePolymorphicWrapperName) (list [Core.termMaybe $ just $ int32 42])) (T.optional T.int32))
    (T.apply (Core.typeVariable $ TestTypes.testTypePolymorphicWrapperName) (T.optional T.int32)),
  checkTest "wrapped map" []
    (wrap (TestTypes.testTypePolymorphicWrapperName) (list [mapTerm [(string "key", int32 42)]]))
    (tyapp (wrap (TestTypes.testTypePolymorphicWrapperName) (list [mapTerm [(string "key", int32 42)]])) (T.map T.string T.int32))
    (T.apply (Core.typeVariable $ TestTypes.testTypePolymorphicWrapperName) (T.map T.string T.int32))]

multipleWrappingLevelsTests :: TBinding TestGroup
multipleWrappingLevelsTests = define "multipleWrappingLevelsTests" $
  subgroup "Multiple wrapping levels" [
  noChange "wrapped in optional"
    (Core.termMaybe $ just $ wrap (TestTypes.testTypeStringAliasName) (string "wrapped"))
    (T.optional $ Core.typeVariable $ TestTypes.testTypeStringAliasName),
  checkTest "list of wrapped polymorphic" []
    (list [wrap (TestTypes.testTypePolymorphicWrapperName) (list [int32 1]),
           wrap (TestTypes.testTypePolymorphicWrapperName) (list [int32 2])])
    (list [tyapp (wrap (TestTypes.testTypePolymorphicWrapperName) (list [int32 1])) T.int32,
           tyapp (wrap (TestTypes.testTypePolymorphicWrapperName) (list [int32 2])) T.int32])
    (T.list $ T.apply (Core.typeVariable $ TestTypes.testTypePolymorphicWrapperName) T.int32)]

multiParameterPolymorphicWrappersTests :: TBinding TestGroup
multiParameterPolymorphicWrappersTests = define "multiParameterPolymorphicWrappersTests" $
  subgroup "Multi-parameter polymorphic wrappers" [
  checkTest "symmetric triple wrapping simple types" []
    (wrap (TestTypes.testTypeSymmetricTripleName) $
      record (TestTypes.testTypeTripleName) [
        "first">: (int32 1),
        "second">: (string "edge"),
        "third">: (int32 2)])
    (tyapps (wrap (TestTypes.testTypeSymmetricTripleName) $
      tyapps (record (TestTypes.testTypeTripleName) [
        "first">: (int32 1),
        "second">: (string "edge"),
        "third">: (int32 2)]) [T.int32, T.string, T.int32])
      [T.int32, T.string])
    (T.applys (Core.typeVariable $ TestTypes.testTypeSymmetricTripleName) [T.int32, T.string]),
  checkTest "symmetric triple from lambda" []
    (lambda "v1" $ lambda "e" $ lambda "v2" $
      wrap (TestTypes.testTypeSymmetricTripleName) $
        record (TestTypes.testTypeTripleName) [
          "first">: (var "v1"),
          "second">: (var "e"),
          "third">: (var "v2")])
    (tylams ["t0", "t1"] $
      lambdaTyped "v1" (T.var "t0") $
      lambdaTyped "e" (T.var "t1") $
      lambdaTyped "v2" (T.var "t0") $
      tyapps (wrap (TestTypes.testTypeSymmetricTripleName) $
        tyapps (record (TestTypes.testTypeTripleName) [
          "first">: (var "v1"),
          "second">: (var "e"),
          "third">: (var "v2")]) [T.var "t0", T.var "t1", T.var "t0"])
        [T.var "t0", T.var "t1"])
    (T.forAlls ["t0", "t1"] $
      T.function (T.var "t0") $
      T.function (T.var "t1") $
      T.function (T.var "t0") $
      T.applys (Core.typeVariable $ TestTypes.testTypeSymmetricTripleName) [T.var "t0", T.var "t1"]),
  checkTest "symmetric triple with nested polymorphic types and foldl" []
    (lets ["sumList">: lambda "lst" $
            primitive _lists_foldl @@
            (lambda "acc" $ lambda "x" $ primitive _math_add @@ var "acc" @@ var "x") @@
            int32 0 @@
            var "lst"] $
      lambda "nums1" $ lambda "nums2" $
        wrap (TestTypes.testTypeSymmetricTripleName) $
          record (TestTypes.testTypeTripleName) [
            "first">: (var "sumList" @@ var "nums1"),
            "second">: (list [var "nums1", var "nums2"]),
            "third">: (var "sumList" @@ var "nums2")])
    (letsTyped [("sumList",
                 lambdaTyped "lst" (T.list T.int32) $
                   tyapps (primitive _lists_foldl) [T.int32, T.int32] @@
                   (lambdaTyped "acc" T.int32 $ lambdaTyped "x" T.int32 $ primitive _math_add @@ var "acc" @@ var "x") @@
                   int32 0 @@
                   var "lst",
                 T.mono $ T.function (T.list T.int32) T.int32)] $
      lambdaTyped "nums1" (T.list T.int32) $
      lambdaTyped "nums2" (T.list T.int32) $
        tyapps (wrap (TestTypes.testTypeSymmetricTripleName) $
          tyapps (record (TestTypes.testTypeTripleName) [
            "first">: (var "sumList" @@ var "nums1"),
            "second">: (list [var "nums1", var "nums2"]),
            "third">: (var "sumList" @@ var "nums2")])
            [T.int32, T.list (T.list T.int32), T.int32])
          [T.int32, T.list (T.list T.int32)])
    (T.function (T.list T.int32) $
      T.function (T.list T.int32) $
      T.applys (Core.typeVariable $ TestTypes.testTypeSymmetricTripleName) [T.int32, T.list (T.list T.int32)])]

------ Wrap eliminations ------

wrapEliminationsTests :: TBinding TestGroup
wrapEliminationsTests = define "wrapEliminationsTests" $
  supergroup "Wrap eliminations" [
  monomorphicUnwrappingTests,
  polymorphicUnwrappingTests,
  unwrapEliminationsInApplicationsTests,
  unwrapInComplexContextsTests,
  multiParameterPolymorphicUnwrappersTests,
  chainedUnwrappingTests,
  multipleUnwrapOperationsTests]

monomorphicUnwrappingTests :: TBinding TestGroup
monomorphicUnwrappingTests = define "monomorphicUnwrappingTests" $
  subgroup "Monomorphic unwrapping" [
  noChange "unwrap string alias"
    (unwrap (TestTypes.testTypeStringAliasName))
    (T.function (Core.typeVariable $ TestTypes.testTypeStringAliasName) T.string)]

polymorphicUnwrappingTests :: TBinding TestGroup
polymorphicUnwrappingTests = define "polymorphicUnwrappingTests" $
  subgroup "Polymorphic unwrapping" [
  checkTest "unwrap polymorphic wrapper" []
    (unwrap (TestTypes.testTypePolymorphicWrapperName))
    (tylam "t0" $ tyapp (unwrap (TestTypes.testTypePolymorphicWrapperName)) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.apply (Core.typeVariable $ TestTypes.testTypePolymorphicWrapperName) (T.var "t0")) (T.list $ T.var "t0"))]

unwrapEliminationsInApplicationsTests :: TBinding TestGroup
unwrapEliminationsInApplicationsTests = define "unwrapEliminationsInApplicationsTests" $
  subgroup "Unwrap eliminations in applications" [
  noChange "unwrap applied to wrapped term"
    (unwrap (TestTypes.testTypeStringAliasName) @@ wrap (TestTypes.testTypeStringAliasName) (string "hello"))
    T.string,
  checkTest "unwrap polymorphic applied" []
    (unwrap (TestTypes.testTypePolymorphicWrapperName) @@ wrap (TestTypes.testTypePolymorphicWrapperName) (list [int32 1, int32 2]))
    (tyapp (unwrap (TestTypes.testTypePolymorphicWrapperName)) T.int32 @@ tyapp (wrap (TestTypes.testTypePolymorphicWrapperName) (list [int32 1, int32 2])) T.int32)
    (T.list T.int32)]

unwrapInComplexContextsTests :: TBinding TestGroup
unwrapInComplexContextsTests = define "unwrapInComplexContextsTests" $
  subgroup "Unwrap in complex contexts" [
  checkTest "unwrap in let binding" []
    (lets ["unwrapper" >: unwrap (TestTypes.testTypeStringAliasName),
           "wrapped" >: wrap (TestTypes.testTypeStringAliasName) (string "test")] $
          var "unwrapper" @@ var "wrapped")
    (letsTyped [
      ("unwrapper", unwrap (TestTypes.testTypeStringAliasName), T.mono $ T.function (Core.typeVariable $ TestTypes.testTypeStringAliasName) T.string),
      ("wrapped", wrap (TestTypes.testTypeStringAliasName) (string "test"), T.mono $ Core.typeVariable $ TestTypes.testTypeStringAliasName)] $
      var "unwrapper" @@ var "wrapped")
    T.string,
  checkTest "unwrap in tuple" []
    (tuple [unwrap (TestTypes.testTypeStringAliasName), string "context"])
    (tyapps (pair
      (unwrap (TestTypes.testTypeStringAliasName))
      (string "context"))
      [T.function (Core.typeVariable $ TestTypes.testTypeStringAliasName) T.string, T.string])
    (T.pair (T.function (Core.typeVariable $ TestTypes.testTypeStringAliasName) T.string) T.string),
  checkTest "unwrap in lambda" []
    (lambda "wrapped" $ unwrap (TestTypes.testTypeStringAliasName) @@ var "wrapped")
    (lambdaTyped "wrapped" (Core.typeVariable $ TestTypes.testTypeStringAliasName) $ unwrap (TestTypes.testTypeStringAliasName) @@ var "wrapped")
    (T.function (Core.typeVariable $ TestTypes.testTypeStringAliasName) T.string)]

multiParameterPolymorphicUnwrappersTests :: TBinding TestGroup
multiParameterPolymorphicUnwrappersTests = define "multiParameterPolymorphicUnwrappersTests" $
  subgroup "Multi-parameter polymorphic unwrappers" [
  checkTest "unwrap symmetric triple to tuple" []
    (lambda "st" $
      tuple [
        project (TestTypes.testTypeTripleName) (name "first") @@ (unwrap (TestTypes.testTypeSymmetricTripleName) @@ var "st"),
        project (TestTypes.testTypeTripleName) (name "third") @@ (unwrap (TestTypes.testTypeSymmetricTripleName) @@ var "st")])
    (tylams ["t0", "t1"] $
      lambdaTyped "st" (T.applys (Core.typeVariable $ TestTypes.testTypeSymmetricTripleName) [T.var "t0", T.var "t1"]) $
      tyapps (pair
        (tyapps (project (TestTypes.testTypeTripleName) (name "first")) [T.var "t0", T.var "t1", T.var "t0"] @@
          (tyapps (unwrap (TestTypes.testTypeSymmetricTripleName)) [T.var "t0", T.var "t1"] @@ var "st"))
        (tyapps (project (TestTypes.testTypeTripleName) (name "third")) [T.var "t0", T.var "t1", T.var "t0"] @@
          (tyapps (unwrap (TestTypes.testTypeSymmetricTripleName)) [T.var "t0", T.var "t1"] @@ var "st")))
        [T.var "t0", T.var "t0"])
    (T.forAlls ["t0", "t1"] $
      T.function
        (T.applys (Core.typeVariable $ TestTypes.testTypeSymmetricTripleName) [T.var "t0", T.var "t1"])
        (T.pair (T.var "t0") (T.var "t0"))),
  checkTest "unwrap and collect edges in set" []
    (lets ["getEdge" >: lambda "st" $
            project (TestTypes.testTypeTripleName) (name "second") @@ (unwrap (TestTypes.testTypeSymmetricTripleName) @@ var "st")] $
      lambda "triples" $
        primitive _sets_map @@ var "getEdge" @@ var "triples")
    (tylams ["t0", "t1"] $
      letsTyped [("getEdge",
                 tylams ["t2", "t3"] $
                 lambdaTyped "st" (T.applys (Core.typeVariable $ TestTypes.testTypeSymmetricTripleName) [T.var "t2", T.var "t3"]) $
                   tyapps (project (TestTypes.testTypeTripleName) (name "second")) [T.var "t2", T.var "t3", T.var "t2"] @@
                   (tyapps (unwrap (TestTypes.testTypeSymmetricTripleName)) [T.var "t2", T.var "t3"] @@ var "st"),
                 T.poly ["t2", "t3"] $ T.function
                   (T.applys (Core.typeVariable $ TestTypes.testTypeSymmetricTripleName) [T.var "t2", T.var "t3"])
                   (T.var "t3"))] $
      lambdaTyped "triples" (T.set $ T.applys (Core.typeVariable $ TestTypes.testTypeSymmetricTripleName) [T.var "t0", T.var "t1"]) $
        tyapps (primitive _sets_map) [T.applys (Core.typeVariable $ TestTypes.testTypeSymmetricTripleName) [T.var "t0", T.var "t1"], T.var "t1"] @@
        (tyapps (var "getEdge") [T.var "t0", T.var "t1"]) @@
        var "triples")
    (T.forAlls ["t0", "t1"] $
      T.function
        (T.set $ T.applys (Core.typeVariable $ TestTypes.testTypeSymmetricTripleName) [T.var "t0", T.var "t1"])
        (T.set $ T.var "t1")),

  checkTest "unwrap with maybe to handle optional symmetric triple" []
    (lambda "mst" $
      primitive _maybes_maybe @@
      (Core.termMaybe nothing) @@
      (lambda "st" $ Core.termMaybe $
        just $ project (TestTypes.testTypeTripleName) (name "second") @@ (unwrap (TestTypes.testTypeSymmetricTripleName) @@ var "st")) @@
      var "mst")
    (tylams ["t0", "t1"] $
      lambdaTyped "mst" (T.optional $ T.applys (Core.typeVariable $ TestTypes.testTypeSymmetricTripleName) [T.var "t0", T.var "t1"]) $
      tyapps (primitive _maybes_maybe)
        [T.optional (T.var "t1"),
         T.applys (Core.typeVariable $ TestTypes.testTypeSymmetricTripleName) [T.var "t0", T.var "t1"]] @@
      tyapp (Core.termMaybe nothing) (T.var "t1") @@
      (lambdaTyped "st" (T.applys (Core.typeVariable $ TestTypes.testTypeSymmetricTripleName) [T.var "t0", T.var "t1"]) $
        Core.termMaybe $ just $
        (tyapps (project (TestTypes.testTypeTripleName) (name "second")) [T.var "t0", T.var "t1", T.var "t0"] @@
         (tyapps (unwrap (TestTypes.testTypeSymmetricTripleName)) [T.var "t0", T.var "t1"] @@ var "st"))) @@
      var "mst")
    (T.forAlls ["t0", "t1"] $
      T.function
        (T.optional $ T.applys (Core.typeVariable $ TestTypes.testTypeSymmetricTripleName) [T.var "t0", T.var "t1"])
        (T.optional $ T.var "t1"))]

chainedUnwrappingTests :: TBinding TestGroup
chainedUnwrappingTests = define "chainedUnwrappingTests" $
  subgroup "Chained unwrapping" [
  checkTest "unwrap then process" []
    (lambda "wrapped" $
      primitive _strings_cat2 @@ (unwrap (TestTypes.testTypeStringAliasName) @@ var "wrapped") @@ string " suffix")
    (lambdaTyped "wrapped" (Core.typeVariable $ TestTypes.testTypeStringAliasName) $
      primitive _strings_cat2 @@ (unwrap (TestTypes.testTypeStringAliasName) @@ var "wrapped") @@ string " suffix")
    (T.function (Core.typeVariable $ TestTypes.testTypeStringAliasName) T.string),
  checkTest "unwrap polymorphic then map" []
    (lambda "wrappedList" $
      primitive _lists_map @@ (primitive _math_add @@ int32 1) @@ (unwrap (TestTypes.testTypePolymorphicWrapperName) @@ var "wrappedList"))
    (lambdaTyped "wrappedList" (T.apply (Core.typeVariable $ TestTypes.testTypePolymorphicWrapperName) T.int32) $
      (tyapps (primitive _lists_map) [T.int32, T.int32]) @@ (primitive _math_add @@ int32 1) @@ (tyapp (unwrap (TestTypes.testTypePolymorphicWrapperName)) T.int32 @@ var "wrappedList"))
    (T.function (T.apply (Core.typeVariable $ TestTypes.testTypePolymorphicWrapperName) T.int32) (T.list T.int32))]

multipleUnwrapOperationsTests :: TBinding TestGroup
multipleUnwrapOperationsTests = define "multipleUnwrapOperationsTests" $
  subgroup "Multiple unwrap operations" [
  checkTest "unwrap different types" []
    (lambda "stringWrapped" $
      lambda "listWrapped" $
        tuple [
          unwrap (TestTypes.testTypeStringAliasName) @@ var "stringWrapped",
          unwrap (TestTypes.testTypePolymorphicWrapperName) @@ var "listWrapped"])
    (tylam "t0" $ lambdaTyped "stringWrapped" (Core.typeVariable $ TestTypes.testTypeStringAliasName) $
      lambdaTyped "listWrapped" (T.apply (Core.typeVariable $ TestTypes.testTypePolymorphicWrapperName) (T.var "t0")) $
        tyapps (pair
          (unwrap (TestTypes.testTypeStringAliasName) @@ var "stringWrapped")
          (tyapp (unwrap (TestTypes.testTypePolymorphicWrapperName)) (T.var "t0") @@ var "listWrapped"))
          [T.string, T.list $ T.var "t0"])
    (T.forAll "t0" $ T.function (Core.typeVariable $ TestTypes.testTypeStringAliasName)
      (T.function (T.apply (Core.typeVariable $ TestTypes.testTypePolymorphicWrapperName) (T.var "t0"))
        (T.pair T.string (T.list $ T.var "t0"))))]

------ Eliminations parent group ------

eliminationsTests :: TBinding TestGroup
eliminationsTests = define "eliminationsTests" $
  supergroup "Eliminations" [
  recordEliminationsTests,
  unionEliminationsTests,
  wrapEliminationsTests]

projectionsWithVariablesTests :: TBinding TestGroup
projectionsWithVariablesTests = define "projectionsWithVariablesTests" $
  subgroup "Projections with variables" [
  checkTest "project from lambda parameter" []
    (lambda "person" $ project (TestTypes.testTypePersonName) (name "firstName") @@ var "person")
    (lambdaTyped "person" (T.var "Person") $ project (TestTypes.testTypePersonName) (name "firstName") @@ var "person")
    (T.function (T.var "Person") T.string),
  checkTest "project from polymorphic lambda parameter" []
    (lambda "coords" $ project (TestTypes.testTypeLatLonPolyName) (name "lat") @@ var "coords")
    (tylam "t0" $ lambdaTyped "coords" (T.apply (T.var "LatLonPoly") (T.var "t0")) $ tyapp (project (TestTypes.testTypeLatLonPolyName) (name "lat")) (T.var "t0") @@ var "coords")
    (T.forAll "t0" $ T.function (T.apply (T.var "LatLonPoly") (T.var "t0")) (T.var "t0")),
  checkTest "multiple projections from same record" []
    (lambda "person" $
     tuple [project (TestTypes.testTypePersonName) (name "firstName") @@ var "person",
            project (TestTypes.testTypePersonName) (name "lastName") @@ var "person"])
    (lambdaTyped "person" (T.var "Person") $
     tyapps (pair
       (project (TestTypes.testTypePersonName) (name "firstName") @@ var "person")
       (project (TestTypes.testTypePersonName) (name "lastName") @@ var "person")) [T.string, T.string])
    (T.function (T.var "Person") (T.pair T.string T.string))]


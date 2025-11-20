{-# LANGUAGE OverloadedStrings #-}

-- | Nominal type checking test cases: records, unions, wrapped terms, and eliminations (projections, case statements, unwrapping)
module Hydra.Sources.Test.Checking.NominalTypes where

import Hydra.Kernel hiding (map)
import Hydra.Testing
import Hydra.Dsl.Meta.Testing
import Hydra.Dsl.Meta.Terms as MetaTerms
import qualified Hydra.Dsl.Meta.Types as T
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Meta.Core as Core
import Hydra.Sources.Test.TestGraph

import Prelude hiding (map, product, sum)
import qualified Data.List as L
import qualified Data.Map as M


nominalTypesTests :: TTerm TestGroup
nominalTypesTests = supergroup "Nominal types" [
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

recordsTests :: TTerm TestGroup
recordsTests = supergroup "Records" [
  monomorphicRecordsTests,
  polymorphicRecordsTests,
  recordsInComplexContextsTests,
  multiParameterPolymorphicRecordsTests]

monomorphicRecordsTests :: TTerm TestGroup
monomorphicRecordsTests = subgroup "Monomorphic records" [
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

polymorphicRecordsTests :: TTerm TestGroup
polymorphicRecordsTests = subgroup "Polymorphic records" [
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

recordsInComplexContextsTests :: TTerm TestGroup
recordsInComplexContextsTests = subgroup "Records in complex contexts" [
  noChange "records in tuple"
    (tuple [
      record (name "Person") [
        "firstName" >: string "Bob",
        "lastName" >: string "Jones",
        "age" >: int32 25],
      record (name "LatLon") [
        "lat" >: float32 1.0,
        "lon" >: float32 2.0]])
    (T.product [T.var "Person", T.var "LatLon"]),
  checkTest "poly records in tuple" []
    (tuple [
      record (name "LatLonPoly") [
        "lat" >: int32 1,
        "lon" >: int32 2],
      record (name "BuddyListA") [
        "head" >: string "test",
        "tail" >: optional nothing]])
    (tuple [
      tyapp (record (name "LatLonPoly") [
        "lat" >: int32 1,
        "lon" >: int32 2]) T.int32,
      tyapp (record (name "BuddyListA") [
        "head" >: string "test",
        "tail" >: tyapp (optional nothing) (T.apply (T.var "BuddyListB") T.string)]) T.string])
    (T.product [
      T.apply (T.var "LatLonPoly") T.int32,
      T.apply (T.var "BuddyListA") T.string]),
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

multiParameterPolymorphicRecordsTests :: TTerm TestGroup
multiParameterPolymorphicRecordsTests = subgroup "Multi-parameter polymorphic records" [
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
        "second" >: inject (ref testTypePersonOrSomethingNameDef) "other"
          (mapTerm [(var "k", var "v")]),
        "third" >: int32 999])
    (tylams ["t0", "t1"] $
      lambdaTyped "k" (T.var "t0") $
      lambdaTyped "v" (T.var "t1") $
      tyapps (record (name "Triple") [
        "first" >: string "prefix",
        "second" >: tyapp (inject (ref testTypePersonOrSomethingNameDef) "other"
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

recordEliminationsTests :: TTerm TestGroup
recordEliminationsTests = supergroup "Record eliminations" [
  simpleRecordProjectionsTests,
  recordProjectionsAppliedToRecordsTests,
  polymorphicRecordProjectionsTests,
  polymorphicRecordProjectionsAppliedTests,
  recordProjectionsWithVariablesTests,
  recordProjectionsInComplexContextsTests,
  multiParameterPolymorphicProjectionsTests,
  higherOrderRecordProjectionsTests,
  recursiveRecordProjectionsTests,
  recordProjectionsWithMutualRecursionTests]

simpleRecordProjectionsTests :: TTerm TestGroup
simpleRecordProjectionsTests = subgroup "Simple record projections" [
  noChange "project firstName from Person"
    (project (ref testTypePersonNameDef) (name "firstName"))
    (T.function (T.var "Person") T.string),
  noChange "project lastName from Person"
    (project (ref testTypePersonNameDef) (name "lastName"))
    (T.function (T.var "Person") T.string),
  noChange "project age from Person"
    (project (ref testTypePersonNameDef) (name "age"))
    (T.function (T.var "Person") T.int32),
  noChange "project lat from LatLon"
    (project (ref testTypeLatLonNameDef) (name "lat"))
    (T.function (T.var "LatLon") T.float32),
  noChange "project lon from LatLon"
    (project (ref testTypeLatLonNameDef) (name "lon"))
    (T.function (T.var "LatLon") T.float32)]

recordProjectionsAppliedToRecordsTests :: TTerm TestGroup
recordProjectionsAppliedToRecordsTests = subgroup "Record projections applied to records" [
  noChange "project firstName applied to person record"
    (project (ref testTypePersonNameDef) (name "firstName") @@
     record (name "Person") [
       "firstName" >: string "Alice",
       "lastName" >: string "Smith",
       "age" >: int32 30])
    T.string,
  noChange "project age applied to person record"
    (project (ref testTypePersonNameDef) (name "age") @@
     record (name "Person") [
       "firstName" >: string "Bob",
       "lastName" >: string "Jones",
       "age" >: int32 25])
    T.int32,
  noChange "project lat applied to LatLon record"
    (project (ref testTypeLatLonNameDef) (name "lat") @@
     record (name "LatLon") [
       "lat" >: float32 40.7128,
       "lon" >: float32 (-74.0060)])
    T.float32]

polymorphicRecordProjectionsTests :: TTerm TestGroup
polymorphicRecordProjectionsTests = subgroup "Polymorphic record projections" [
  checkTest "project lat from polymorphic LatLonPoly" []
    (project (ref testTypeLatLonPolyNameDef) (name "lat"))
    (tylam "t0" $ tyapp (project (ref testTypeLatLonPolyNameDef) (name "lat")) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.apply (T.var "LatLonPoly") (T.var "t0")) (T.var "t0")),
  checkTest "project lon from polymorphic LatLonPoly" []
    (project (ref testTypeLatLonPolyNameDef) (name "lon"))
    (tylam "t0" $ tyapp (project (ref testTypeLatLonPolyNameDef) (name "lon")) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.apply (T.var "LatLonPoly") (T.var "t0")) (T.var "t0")),
  checkTest "project head from BuddyListA" []
    (project (ref testTypeBuddyListANameDef) (name "head"))
    (tylam "t0" $ tyapp (project (ref testTypeBuddyListANameDef) (name "head")) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.apply (T.var "BuddyListA") (T.var "t0")) (T.var "t0")),
  checkTest "project tail from BuddyListA" []
    (project (ref testTypeBuddyListANameDef) (name "tail"))
    (tylam "t0" $ tyapp (project (ref testTypeBuddyListANameDef) (name "tail")) (T.var "t0"))
    (T.forAll "t0" $ T.function
      (T.apply (T.var "BuddyListA") (T.var "t0"))
      (T.optional (T.apply (T.var "BuddyListB") (T.var "t0"))))]

polymorphicRecordProjectionsAppliedTests :: TTerm TestGroup
polymorphicRecordProjectionsAppliedTests = subgroup "Polymorphic record projections applied" [
  checkTest "project lat from LatLonPoly with int32" []
    (project (ref testTypeLatLonPolyNameDef) (name "lat") @@
     record (name "LatLonPoly") [
       "lat" >: int32 40,
       "lon" >: int32 (-74)])
    (tyapp (project (ref testTypeLatLonPolyNameDef) (name "lat")) T.int32 @@
     tyapp (record (name "LatLonPoly") [
       "lat" >: int32 40,
       "lon" >: int32 (-74)]) T.int32)
    T.int32,
  checkTest "project lon from LatLonPoly with float64" []
    (project (ref testTypeLatLonPolyNameDef) (name "lon") @@
     record (name "LatLonPoly") [
       "lat" >: float64 40.7128,
       "lon" >: float64 (-74.0060)])
    (tyapp (project (ref testTypeLatLonPolyNameDef) (name "lon")) T.float64 @@
     tyapp (record (name "LatLonPoly") [
       "lat" >: float64 40.7128,
       "lon" >: float64 (-74.0060)]) T.float64)
    T.float64,
  checkTest "project head from BuddyListA with string" []
    (project (ref testTypeBuddyListANameDef) (name "head") @@
     record (name "BuddyListA") [
       "head" >: string "Alice",
       "tail" >: optional nothing])
    (tyapp (project (ref testTypeBuddyListANameDef) (name "head")) T.string @@
     tyapp (record (name "BuddyListA") [
       "head" >: string "Alice",
       "tail" >: tyapp (optional nothing) (T.apply (T.var "BuddyListB") T.string)]) T.string)
    T.string]

recordProjectionsWithVariablesTests :: TTerm TestGroup
recordProjectionsWithVariablesTests = subgroup "Record projections with variables" [
  checkTest "project from lambda parameter" []
    (lambda "person" $ project (ref testTypePersonNameDef) (name "firstName") @@ var "person")
    (lambdaTyped "person" (T.var "Person") $ project (ref testTypePersonNameDef) (name "firstName") @@ var "person")
    (T.function (T.var "Person") T.string),
  checkTest "project from polymorphic lambda parameter" []
    (lambda "coords" $ project (ref testTypeLatLonPolyNameDef) (name "lat") @@ var "coords")
    (tylam "t0" $ lambdaTyped "coords" (T.apply (T.var "LatLonPoly") (T.var "t0")) $ tyapp (project (ref testTypeLatLonPolyNameDef) (name "lat")) (T.var "t0") @@ var "coords")
    (T.forAll "t0" $ T.function (T.apply (T.var "LatLonPoly") (T.var "t0")) (T.var "t0")),
  checkTest "multiple projections from same record" []
    (lambda "person" $
     tuple [project (ref testTypePersonNameDef) (name "firstName") @@ var "person",
            project (ref testTypePersonNameDef) (name "lastName") @@ var "person"])
    (lambdaTyped "person" (T.var "Person") $
     tuple [project (ref testTypePersonNameDef) (name "firstName") @@ var "person",
            project (ref testTypePersonNameDef) (name "lastName") @@ var "person"])
    (T.function (T.var "Person") (T.product [T.string, T.string]))]

recordProjectionsInComplexContextsTests :: TTerm TestGroup
recordProjectionsInComplexContextsTests = subgroup "Record projections in complex contexts" [
  checkTest "projection in let binding" []
    (lets ["person">: record (name "Person") [
             "firstName" >: string "Charlie",
             "lastName" >: string "Brown",
             "age" >: int32 35],
           "getName">: project (ref testTypePersonNameDef) (name "firstName")] $
          var "getName" @@ var "person")
    (letsTyped [("person", record (name "Person") [
                   "firstName" >: string "Charlie",
                   "lastName" >: string "Brown",
                   "age" >: int32 35],
                 T.mono $ T.var "Person"),
                ("getName", project (ref testTypePersonNameDef) (name "firstName"),
                 T.mono $ T.function (T.var "Person") T.string)] $
      var "getName" @@ var "person")
    T.string,
  noChange "projection in tuple"
    (tuple [project (ref testTypePersonNameDef) (name "firstName"),
            project (ref testTypePersonNameDef) (name "age")])
    (T.product [T.function (T.var "Person") T.string,
                T.function (T.var "Person") T.int32]),
  noChange "projection in list"
    (list [project (ref testTypePersonNameDef) (name "firstName"),
           project (ref testTypePersonNameDef) (name "lastName")])
    (T.list (T.function (T.var "Person") T.string))]

multiParameterPolymorphicProjectionsTests :: TTerm TestGroup
multiParameterPolymorphicProjectionsTests = subgroup "Multi-parameter polymorphic projections" [
  checkTest "project first from Triple" []
    (project (ref testTypeTripleNameDef) (name "first"))
    (tylams ["t0", "t1", "t2"] $ tyapps (project (ref testTypeTripleNameDef) (name "first")) [T.var "t0", T.var "t1", T.var "t2"])
    (T.forAlls ["t0", "t1", "t2"] $
      T.function
        (T.applys (Core.typeVariable $ ref testTypeTripleNameDef) [T.var "t0", T.var "t1", T.var "t2"])
        (T.var "t0")),
  checkTest "project second from Triple applied" []
    (project (ref testTypeTripleNameDef) (name "second") @@
      record (ref testTypeTripleNameDef) [
        "first">: int32 1,
        "second">: string "middle",
        "third">: boolean True])
    (tyapps (project (ref testTypeTripleNameDef) (name "second")) [T.int32, T.string, T.boolean] @@
      tyapps (record (ref testTypeTripleNameDef) [
        "first">: int32 1,
        "second">: string "middle",
        "third">: boolean True]) [T.int32, T.string, T.boolean])
    T.string,
  checkTest "project from Triple and use second field, which is another polymorphic record" []
    (lambda "triple" $ lambda "key" $
      match (ref testTypePersonOrSomethingNameDef) nothing [
        "person">: lambda "p" $ Core.termMaybe nothing,
        "other">: lambda "m" $ primitive _maps_lookup @@ var "key" @@ var "m"] @@
      (project (ref testTypeTripleNameDef) (name "second") @@ var "triple"))
    (tylams ["t0", "t1", "t2", "t3"] $
      lambdaTyped "triple"
        (T.applys (Core.typeVariable $ ref testTypeTripleNameDef)
          [T.var "t0",
           T.apply (Core.typeVariable $ ref testTypePersonOrSomethingNameDef) (T.map (T.var "t1") (T.var "t2")),
           T.var "t3"]) $
      lambdaTyped "key" (T.var "t1") $
      tyapp (match (ref testTypePersonOrSomethingNameDef) nothing [
        "person">: lambdaTyped "p" (Core.typeVariable $ ref testTypePersonNameDef) (tyapp (Core.termMaybe nothing) (T.var "t2")),
        "other">: lambdaTyped "m" (T.map (T.var "t1") (T.var "t2")) $
          tyapps (primitive _maps_lookup) [T.var "t1", T.var "t2"] @@ var "key" @@ var "m"]) (T.map (T.var "t1") (T.var "t2")) @@
      (tyapps (project (ref testTypeTripleNameDef) (name "second"))
        [T.var "t0",
         T.apply (Core.typeVariable $ ref testTypePersonOrSomethingNameDef) (T.map (T.var "t1") (T.var "t2")),
         T.var "t3"] @@ var "triple"))
    (T.forAlls ["t0", "t1", "t2", "t3"] $
      T.function
        (T.applys (Core.typeVariable $ ref testTypeTripleNameDef)
          [T.var "t0",
           T.apply (Core.typeVariable $ ref testTypePersonOrSomethingNameDef) (T.map (T.var "t1") (T.var "t2")),
           T.var "t3"])
        (T.function (T.var "t1") (T.optional (T.var "t2"))))]

higherOrderRecordProjectionsTests :: TTerm TestGroup
higherOrderRecordProjectionsTests = subgroup "Higher-order record projections" [
  checkTest "map projection over list of records" []
    (primitive _lists_map @@ (project (ref testTypePersonNameDef) (name "firstName")) @@
     list [record (ref testTypePersonNameDef) [
             "firstName">: (string "Alice"),
             "lastName">: (string "Smith"),
             "age">: (int32 30)],
           record (ref testTypePersonNameDef) [
             "firstName">: (string "Bob"),
             "lastName">: (string "Jones"),
             "age">: (int32 25)]])
    (tyapps (primitive _lists_map) [Core.typeVariable $ ref testTypePersonNameDef, T.string] @@ (project (ref testTypePersonNameDef) (name "firstName")) @@
     list [record (ref testTypePersonNameDef) [
             "firstName">: (string "Alice"),
             "lastName">: (string "Smith"),
             "age">: (int32 30)],
           record (ref testTypePersonNameDef) [
             "firstName">: (string "Bob"),
             "lastName">: (string "Jones"),
             "age">: (int32 25)]])
    (T.list T.string),
  checkTest "map polymorphic projection" []
    (primitive _lists_map @@ (project (ref testTypeLatLonPolyNameDef) (name "lat")) @@
     list [record (ref testTypeLatLonPolyNameDef) [
             "lat">: (int32 40),
             "lon">: (int32 (-74))],
           record (ref testTypeLatLonPolyNameDef) [
             "lat">: (int32 34),
             "lon">: (int32 (-118))]])
    (tyapps (primitive _lists_map) [T.apply (Core.typeVariable $ ref testTypeLatLonPolyNameDef) T.int32, T.int32]
      @@ (tyapp (project (ref testTypeLatLonPolyNameDef) (name "lat")) T.int32) @@
     list [tyapp (record (ref testTypeLatLonPolyNameDef) [
             "lat">: (int32 40),
             "lon">: (int32 (-74))]) T.int32,
           tyapp (record (ref testTypeLatLonPolyNameDef) [
             "lat">: (int32 34),
             "lon">: (int32 (-118))]) T.int32])
    (T.list T.int32),
  checkTest "filter using projection" []
    (primitive _lists_filter @@
     (lambda "person" $
      primitive _equality_gt @@
      (project (ref testTypePersonNameDef) (name "age") @@ var "person") @@
      int32 30) @@
     list [record (ref testTypePersonNameDef) [
             "firstName">: (string "Alice"),
             "lastName">: (string "Smith"),
             "age">: (int32 35)],
           record (ref testTypePersonNameDef) [
             "firstName">: (string "Bob"),
             "lastName">: (string "Jones"),
             "age">: (int32 25)]])
    (tyapp (primitive _lists_filter) (Core.typeVariable $ ref testTypePersonNameDef) @@
     (lambdaTyped "person" (Core.typeVariable $ ref testTypePersonNameDef) $
      tyapp (primitive _equality_gt) T.int32 @@
      (project (ref testTypePersonNameDef) (name "age") @@ var "person") @@
      int32 30) @@
     list [record (ref testTypePersonNameDef) [
             "firstName">: (string "Alice"),
             "lastName">: (string "Smith"),
             "age">: (int32 35)],
           record (ref testTypePersonNameDef) [
             "firstName">: (string "Bob"),
             "lastName">: (string "Jones"),
             "age">: (int32 25)]])
    (T.list (Core.typeVariable $ ref testTypePersonNameDef))]

recursiveRecordProjectionsTests :: TTerm TestGroup
recursiveRecordProjectionsTests = subgroup "Recursive record projections" [
  checkTest "nested projection from recursive record" []
    (lambda "intList" $
     primitive _maybes_maybe @@
     int32 0 @@
     (project (ref testTypeIntListNameDef) (name "head")) @@
     (project (ref testTypeIntListNameDef) (name "tail") @@ var "intList"))
    (lambdaTyped "intList" (Core.typeVariable $ ref testTypeIntListNameDef) $
     tyapps (primitive _maybes_maybe) [T.int32, Core.typeVariable $ ref testTypeIntListNameDef] @@
     int32 0 @@
     (project (ref testTypeIntListNameDef) (name "head")) @@
     (project (ref testTypeIntListNameDef) (name "tail") @@ var "intList"))
    (T.function (Core.typeVariable $ ref testTypeIntListNameDef) T.int32)]

recordProjectionsWithMutualRecursionTests :: TTerm TestGroup
recordProjectionsWithMutualRecursionTests = subgroup "Record projections with mutual recursion" [
  checkTest "project head from BuddyListA" []
    (project (ref testTypeBuddyListANameDef) (name "head"))
    (tylam "t0" $ tyapp (project (ref testTypeBuddyListANameDef) (name "head")) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.apply (Core.typeVariable $ ref testTypeBuddyListANameDef) (T.var "t0")) (T.var "t0")),
  checkTest "project tail from BuddyListB" []
    (project (ref testTypeBuddyListBNameDef) (name "tail"))
    (tylam "t0" $ tyapp (project (ref testTypeBuddyListBNameDef) (name "tail")) (T.var "t0"))
    (T.forAll "t0" $ T.function
      (T.apply (Core.typeVariable $ ref testTypeBuddyListBNameDef) (T.var "t0"))
      (T.optional (T.apply (Core.typeVariable $ ref testTypeBuddyListANameDef) (T.var "t0")))),
  checkTest "chained projections across mutual recursion" []
    (lambda "listA" $
      primitive _maybes_maybe @@
      Core.termMaybe nothing @@
      (lambda "listB" $
        primitive _maybes_maybe @@
        Core.termMaybe nothing @@
        (project (ref testTypeBuddyListANameDef) (name "tail")) @@
        (project (ref testTypeBuddyListBNameDef) (name "tail") @@ var "listB")) @@
      (project (ref testTypeBuddyListANameDef) (name "tail") @@ var "listA"))
    (tylam "t0" $ lambdaTyped "listA" (T.apply (Core.typeVariable $ ref testTypeBuddyListANameDef) (T.var "t0")) $
      tyapps (primitive _maybes_maybe) [T.optional (T.apply (Core.typeVariable $ ref testTypeBuddyListBNameDef) (T.var "t0")), T.apply (Core.typeVariable $ ref testTypeBuddyListBNameDef) (T.var "t0")] @@
      tyapp (Core.termMaybe nothing) (T.apply (Core.typeVariable $ ref testTypeBuddyListBNameDef) (T.var "t0")) @@
      (lambdaTyped "listB" (T.apply (Core.typeVariable $ ref testTypeBuddyListBNameDef) (T.var "t0")) $
        tyapps (primitive _maybes_maybe) [T.optional (T.apply (Core.typeVariable $ ref testTypeBuddyListBNameDef) (T.var "t0")), T.apply (Core.typeVariable $ ref testTypeBuddyListANameDef) (T.var "t0")] @@
        tyapp (Core.termMaybe nothing) (T.apply (Core.typeVariable $ ref testTypeBuddyListBNameDef) (T.var "t0")) @@
        (tyapp (project (ref testTypeBuddyListANameDef) (name "tail")) (T.var "t0")) @@
        (tyapp (project (ref testTypeBuddyListBNameDef) (name "tail")) (T.var "t0") @@ var "listB")) @@
      (tyapp (project (ref testTypeBuddyListANameDef) (name "tail")) (T.var "t0") @@ var "listA"))
    (T.forAll "t0" $ T.function
      (T.apply (Core.typeVariable $ ref testTypeBuddyListANameDef) (T.var "t0"))
      (T.optional (T.apply (Core.typeVariable $ ref testTypeBuddyListBNameDef) (T.var "t0"))))]

------ Unions ------

unionsTests :: TTerm TestGroup
unionsTests = supergroup "Unions" [
  simpleUnionInjectionsTests,
  unionInjectionsWithDataTests,
  polymorphicUnionInjectionsTests,
  polymorphicRecursiveUnionInjectionsTests,
  polymorphicUnionsFromLambdaTests,
  unionsInComplexContextsTests,
  multiParameterPolymorphicInjectionsTests]

simpleUnionInjectionsTests :: TTerm TestGroup
simpleUnionInjectionsTests = subgroup "Simple union injections" [
  noChange "inject into Comparison lessThan variant"
    (injectUnit (ref testTypeComparisonNameDef) "lessThan")
    (Core.typeVariable $ ref testTypeComparisonNameDef),
  noChange "inject into Comparison equalTo variant"
    (injectUnit (ref testTypeComparisonNameDef) "equalTo")
    (Core.typeVariable $ ref testTypeComparisonNameDef),
  noChange "inject into Comparison greaterThan variant"
    (injectUnit (ref testTypeComparisonNameDef) "greaterThan")
    (Core.typeVariable $ ref testTypeComparisonNameDef)]

unionInjectionsWithDataTests :: TTerm TestGroup
unionInjectionsWithDataTests = subgroup "Union injections with data" [
  noChange "inject into Number int variant"
    (inject (ref testTypeNumberNameDef) "int" (int32 42))
    (Core.typeVariable $ ref testTypeNumberNameDef),
  noChange "inject into Number float variant"
    (inject (ref testTypeNumberNameDef) "float" (float32 3.14))
    (Core.typeVariable $ ref testTypeNumberNameDef),
  noChange "inject into Timestamp unixTimeMillis variant"
    (inject (ref testTypeTimestampNameDef) "unixTimeMillis" (uint64 1609459200000))
    (Core.typeVariable $ ref testTypeTimestampNameDef),
  noChange "inject into Timestamp date variant"
    (inject (ref testTypeTimestampNameDef) "date" (string "2021-01-01"))
    (Core.typeVariable $ ref testTypeTimestampNameDef)]

polymorphicUnionInjectionsTests :: TTerm TestGroup
polymorphicUnionInjectionsTests = subgroup "Polymorphic union injections" [
  checkTest "inject person into PersonOrSomething" []
    (inject (ref testTypePersonOrSomethingNameDef) "person"
      (record (ref testTypePersonNameDef) [
        "firstName">: string "Alice",
        "lastName">: string "Smith",
        "age">: int32 30]))
    (tylam "t0" $ tyapp (inject (ref testTypePersonOrSomethingNameDef) "person"
      (record (ref testTypePersonNameDef) [
        "firstName">: string "Alice",
        "lastName">: string "Smith",
        "age">: int32 30])) (T.var "t0"))
    (T.forAll "t0" $ T.apply (Core.typeVariable $ ref testTypePersonOrSomethingNameDef) (T.var "t0")),
  checkTest "inject string into PersonOrSomething other variant" []
    (inject (ref testTypePersonOrSomethingNameDef) "other" (string "something else"))
    (tyapp (inject (ref testTypePersonOrSomethingNameDef) "other" (string "something else")) T.string)
    (T.apply (Core.typeVariable $ ref testTypePersonOrSomethingNameDef) T.string),
  checkTest "inject int into PersonOrSomething other variant" []
    (inject (ref testTypePersonOrSomethingNameDef) "other" (int32 42))
    (tyapp (inject (ref testTypePersonOrSomethingNameDef) "other" (int32 42)) T.int32)
    (T.apply (Core.typeVariable $ ref testTypePersonOrSomethingNameDef) T.int32)]

polymorphicRecursiveUnionInjectionsTests :: TTerm TestGroup
polymorphicRecursiveUnionInjectionsTests = subgroup "Polymorphic recursive union injections" [
  checkTest "inject boolean into UnionPolymorphicRecursive" []
    (inject (ref testTypeUnionPolymorphicRecursiveNameDef) "bool" (boolean True))
    (tylam "t0" $ tyapp (inject (ref testTypeUnionPolymorphicRecursiveNameDef) "bool" (boolean True)) (T.var "t0"))
    (T.forAll "t0" $ T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) (T.var "t0")),
  checkTest "inject string value into UnionPolymorphicRecursive" []
    (inject (ref testTypeUnionPolymorphicRecursiveNameDef) "value" (string "test"))
    (tyapp (inject (ref testTypeUnionPolymorphicRecursiveNameDef) "value" (string "test")) T.string)
    (T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) T.string),
  checkTest "inject int value into UnionPolymorphicRecursive" []
    (inject (ref testTypeUnionPolymorphicRecursiveNameDef) "value" (int32 123))
    (tyapp (inject (ref testTypeUnionPolymorphicRecursiveNameDef) "value" (int32 123)) T.int32)
    (T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) T.int32)]

polymorphicUnionsFromLambdaTests :: TTerm TestGroup
polymorphicUnionsFromLambdaTests = subgroup "Polymorphic unions from lambda" [
  checkTest "lambda creating PersonOrSomething other variant" []
    (lambda "x" $ inject (ref testTypePersonOrSomethingNameDef) "other" (var "x"))
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ tyapp (inject (ref testTypePersonOrSomethingNameDef) "other" (var "x")) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.var "t0") (T.apply (Core.typeVariable $ ref testTypePersonOrSomethingNameDef) (T.var "t0"))),
  checkTest "lambda creating UnionPolymorphicRecursive value variant" []
    (lambda "x" $ inject (ref testTypeUnionPolymorphicRecursiveNameDef) "value" (var "x"))
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ tyapp (inject (ref testTypeUnionPolymorphicRecursiveNameDef) "value" (var "x")) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.var "t0") (T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) (T.var "t0")))]

unionsInComplexContextsTests :: TTerm TestGroup
unionsInComplexContextsTests = subgroup "Unions in complex contexts" [
  noChange "union in tuple"
    (tuple [inject (ref testTypeNumberNameDef) "int" (int32 42),
            string "context"])
    (T.product [Core.typeVariable $ ref testTypeNumberNameDef, T.string]),
  noChange "union in list"
    (list [inject (ref testTypeNumberNameDef) "int" (int32 1),
           inject (ref testTypeNumberNameDef) "float" (float32 2.5)])
    (T.list $ Core.typeVariable $ ref testTypeNumberNameDef),
  checkTest "polymorphic union in let binding" []
    (lets ["value">: inject (ref testTypePersonOrSomethingNameDef) "other" (string "test")] $
          var "value")
    (letsTyped [("value", tyapp (inject (ref testTypePersonOrSomethingNameDef) "other" (string "test")) T.string,
                 T.mono $ T.apply (Core.typeVariable $ ref testTypePersonOrSomethingNameDef) T.string)] $
      var "value")
    (T.apply (Core.typeVariable $ ref testTypePersonOrSomethingNameDef) T.string)]

multiParameterPolymorphicInjectionsTests :: TTerm TestGroup
multiParameterPolymorphicInjectionsTests = subgroup "Multi-parameter polymorphic injections" [
  checkTest "either left with int" []
    (inject (ref testTypeEitherNameDef) "left" (int32 42))
    (tylam "t0" $ tyapps (inject (ref testTypeEitherNameDef) "left" (int32 42)) [T.int32, T.var "t0"])
    (T.forAll "t0" $ T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.int32, T.var "t0"]),
  checkTest "either right with string" []
    (inject (ref testTypeEitherNameDef) "right" (string "hello"))
    (tylam "t0" $ tyapps (inject (ref testTypeEitherNameDef) "right" (string "hello")) [T.var "t0", T.string])
    (T.forAll "t0" $ T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.var "t0", T.string]),
  checkTest "either containing LatLonPoly in list" []
    (inject (ref testTypeEitherNameDef) "right"
      (list [record (ref testTypeLatLonPolyNameDef) [
        "lat">: int32 40,
        "lon">: int32 (-74)]]))
    (tylam "t0" $ tyapps (inject (ref testTypeEitherNameDef) "right"
      (list [tyapp (record (ref testTypeLatLonPolyNameDef) [
        "lat">: int32 40,
        "lon">: int32 (-74)]) T.int32]))
      [T.var "t0", T.list (T.apply (Core.typeVariable $ ref testTypeLatLonPolyNameDef) T.int32)])
    (T.forAll "t0" $ T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.var "t0", T.list (T.apply (Core.typeVariable $ ref testTypeLatLonPolyNameDef) T.int32)]),
  checkTest "either in triple in map with shared type variables" []
    (lambda "x0" $ lambda "x1" $ lambda "x2" $
      MetaTerms.map $ Phantoms.map $ M.singleton (string "key") $
        record (ref testTypeTripleNameDef) [
          "first">: inject (ref testTypeEitherNameDef) "left" (var "x0"),
          "second">: inject (ref testTypeEitherNameDef) "left" (var "x0"),
          "third">: inject (ref testTypeEitherNameDef) "right" (var "x1")])
    (tylams ["t0", "t1", "t2", "t3", "t4", "t5"] $
      lambdaTyped "x0" (T.var "t0") $
      lambdaTyped "x1" (T.var "t1") $
      lambdaTyped "x2" (T.var "t2") $
      MetaTerms.map $ Phantoms.map $ M.singleton (string "key") $
        tyapps (record (ref testTypeTripleNameDef) [
          "first">: tyapps (inject (ref testTypeEitherNameDef) "left" (var "x0")) [T.var "t0", T.var "t3"],
          "second">: tyapps (inject (ref testTypeEitherNameDef) "left" (var "x0")) [T.var "t0", T.var "t4"],
          "third">: tyapps (inject (ref testTypeEitherNameDef) "right" (var "x1")) [T.var "t5", T.var "t1"]])
        [T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.var "t0", T.var "t3"],
         T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.var "t0", T.var "t4"],
         T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.var "t5", T.var "t1"]])
    (T.forAlls ["t0", "t1", "t2", "t3", "t4", "t5"] $
      T.function (T.var "t0") $
      T.function (T.var "t1") $
      T.function (T.var "t2") $
      T.map T.string $
        T.applys (Core.typeVariable $ ref testTypeTripleNameDef)
          [T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.var "t0", T.var "t3"],
           T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.var "t0", T.var "t4"],
           T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.var "t5", T.var "t1"]])]

------ Union eliminations ------

unionEliminationsTests :: TTerm TestGroup
unionEliminationsTests = supergroup "Union eliminations" [
  simpleUnitVariantEliminationsTests,
  unionEliminationsWithDataTests,
  polymorphicUnionEliminationsTests,
  unionEliminationsWithDefaultsTests,
  nestedUnionEliminationsTests,
  unionEliminationsInComplexContextsTests,
  multiParameterPolymorphicCaseStatementsTests,
  higherOrderUnionEliminationsTests,
  recursiveUnionEliminationsTests]

simpleUnitVariantEliminationsTests :: TTerm TestGroup
simpleUnitVariantEliminationsTests = subgroup "Simple unit inject eliminations" [
  checkTest "match Comparison with all cases" []
    (match (ref testTypeComparisonNameDef) nothing [
      "lessThan">: lambda "x" (string "less"),
      "equalTo">: lambda "x" (string "equal"),
      "greaterThan">: lambda "x" (string "greater")])
    (match (ref testTypeComparisonNameDef) nothing [
      "lessThan">: lambdaTyped "x" T.unit (string "less"),
      "equalTo">: lambdaTyped "x" T.unit (string "equal"),
      "greaterThan">: lambdaTyped "x" T.unit (string "greater")])
    (T.function (Core.typeVariable $ ref testTypeComparisonNameDef) T.string),
  checkTest "match Comparison returning int32" []
    (match (ref testTypeComparisonNameDef) nothing [
      "lessThan">: lambda "x" (int32 (-1)),
      "equalTo">: lambda "x" (int32 0),
      "greaterThan">: lambda "x" (int32 1)])
    (match (ref testTypeComparisonNameDef) nothing [
      "lessThan">: lambdaTyped "x" T.unit (int32 (-1)),
      "equalTo">: lambdaTyped "x" T.unit (int32 0),
      "greaterThan">: lambdaTyped "x" T.unit (int32 1)])
    (T.function (Core.typeVariable $ ref testTypeComparisonNameDef) T.int32),
  checkTest "match applied to Comparison variant" []
    (match (ref testTypeComparisonNameDef) nothing [
      "lessThan">: lambda "x" (string "less"),
      "equalTo">: lambda "x" (string "equal"),
      "greaterThan">: lambda "x" (string "greater")] @@
     injectUnit (ref testTypeComparisonNameDef) "equalTo")
    (match (ref testTypeComparisonNameDef) nothing [
      "lessThan">: lambdaTyped "x" T.unit (string "less"),
      "equalTo">: lambdaTyped "x" T.unit (string "equal"),
      "greaterThan">: lambdaTyped "x" T.unit (string "greater")] @@
     injectUnit (ref testTypeComparisonNameDef) "equalTo")
    T.string]

unionEliminationsWithDataTests :: TTerm TestGroup
unionEliminationsWithDataTests = subgroup "Union eliminations with data" [
  checkTest "match Number extracting int values" []
    (match (ref testTypeNumberNameDef) nothing [
      "int">: lambda "i" (var "i"),
      "float">: lambda "f" (int32 0)])
    (match (ref testTypeNumberNameDef) nothing [
      "int">: lambdaTyped "i" T.int32 (var "i"),
      "float">: lambdaTyped "f" T.float32 (int32 0)])
    (T.function (Core.typeVariable $ ref testTypeNumberNameDef) T.int32),
  checkTest "match Number converting to string" []
    (match (ref testTypeNumberNameDef) nothing [
      "int">: lambda "i" (primitive _literals_showInt32 @@ var "i"),
      "float">: lambda "f" (primitive _literals_showFloat32 @@ var "f")])
    (match (ref testTypeNumberNameDef) nothing [
      "int">: lambdaTyped "i" T.int32 (primitive _literals_showInt32 @@ var "i"),
      "float">: lambdaTyped "f" T.float32 (primitive _literals_showFloat32 @@ var "f")])
    (T.function (Core.typeVariable $ ref testTypeNumberNameDef) T.string),
  checkTest "match Number applied to int variant" []
    (match (ref testTypeNumberNameDef) nothing [
      "int">: lambda "i" (primitive _math_add @@ var "i" @@ int32 10),
      "float">: lambda "f" (int32 0)] @@
     inject (ref testTypeNumberNameDef) "int" (int32 42))
    (match (ref testTypeNumberNameDef) nothing [
      "int">: lambdaTyped "i" T.int32 (primitive _math_add @@ var "i" @@ int32 10),
      "float">: lambdaTyped "f" T.float32 (int32 0)] @@
     inject (ref testTypeNumberNameDef) "int" (int32 42))
    T.int32,
  checkTest "match Timestamp with mixed data types" []
    (match (ref testTypeTimestampNameDef) nothing [
      "unixTimeMillis">: lambda "millis" (primitive _literals_showUint64 @@ var "millis"),
      "date">: lambda "dateStr" (var "dateStr")])
    (match (ref testTypeTimestampNameDef) nothing [
      "unixTimeMillis">: lambdaTyped "millis" T.uint64 (primitive _literals_showUint64 @@ var "millis"),
      "date">: lambdaTyped "dateStr" T.string (var "dateStr")])
    (T.function (Core.typeVariable $ ref testTypeTimestampNameDef) T.string)]

polymorphicUnionEliminationsTests :: TTerm TestGroup
polymorphicUnionEliminationsTests = supergroup "Polymorphic union eliminations" [
  simplePolymorphicUnionTests,
  usingUnionPolymorphicRecursiveTests,
  usingKernelTypesTests]

simplePolymorphicUnionTests :: TTerm TestGroup
simplePolymorphicUnionTests = subgroup "Simple polymorphic unions" [
  checkTest "match PersonOrSomething with string" []
    (match (ref testTypePersonOrSomethingNameDef) nothing [
      "person">: lambda "p" (project (ref testTypePersonNameDef) (name "firstName") @@ var "p"),
      "other">: lambda "x" (var "x")])
    (tyapp (match (ref testTypePersonOrSomethingNameDef) nothing [
      "person">: lambdaTyped "p" (Core.typeVariable $ ref testTypePersonNameDef) (project (ref testTypePersonNameDef) (name "firstName") @@ var "p"),
      "other">: lambdaTyped "x" T.string (var "x")]) T.string)
    (T.function (T.apply (Core.typeVariable $ ref testTypePersonOrSomethingNameDef) T.string) T.string),
  checkTest "match PersonOrSomething instantiated with string" []
    (match (ref testTypePersonOrSomethingNameDef) nothing [
      "person">: lambda "p" (project (ref testTypePersonNameDef) (name "firstName") @@ var "p"),
      "other">: lambda "x" (var "x")] @@
     inject (ref testTypePersonOrSomethingNameDef) "other" (string "test"))
    (tyapp (match (ref testTypePersonOrSomethingNameDef) nothing [
      "person">: lambdaTyped "p" (Core.typeVariable $ ref testTypePersonNameDef) (project (ref testTypePersonNameDef) (name "firstName") @@ var "p"),
      "other">: lambdaTyped "x" T.string (var "x")]) T.string @@
     tyapp (inject (ref testTypePersonOrSomethingNameDef) "other" (string "test")) T.string)
    T.string]

usingUnionPolymorphicRecursiveTests :: TTerm TestGroup
usingUnionPolymorphicRecursiveTests = subgroup "using UnionPolymorphicRecursive" [
  checkTest "non-applied UnionPolymorphicRecursive" []
    (lets [
      "test">: (match (ref testTypeUnionPolymorphicRecursiveNameDef)
        (just $ string "other") [
        "value">: lambda "i" $ primitive _literals_showInt32 @@ var "i"])] $
      var "test")
    (letsTyped [
        ("test",
         tyapp (match (ref testTypeUnionPolymorphicRecursiveNameDef)
           (just $ string "other") [
           "value">: lambdaTyped "i" T.int32 $ primitive _literals_showInt32 @@ var "i"]) T.int32,
         T.mono $ T.function (T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) T.int32) T.string)] $
      var "test")
    (T.function (T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) T.int32) T.string),
  checkTest "applied UnionPolymorphicRecursive with int32" []
    (lets [
      "test">: (match (ref testTypeUnionPolymorphicRecursiveNameDef)
          (just $ string "other") [
          "value">: lambda "i" $ primitive _literals_showInt32 @@ var "i"])
        @@ (inject (ref testTypeUnionPolymorphicRecursiveNameDef) "value" $ int32 42)] $
      var "test")
    (letsTyped [
      ("test",
       tyapp (match (ref testTypeUnionPolymorphicRecursiveNameDef)
           (just $ string "other") [
           "value">: lambdaTyped "i" T.int32 $ primitive _literals_showInt32 @@ var "i"]) T.int32
         @@ tyapp (inject (ref testTypeUnionPolymorphicRecursiveNameDef) "value" $ int32 42) T.int32,
       T.mono T.string)] $
      var "test")
    T.string,
  checkTest "applied UnionPolymorphicRecursive with int32 in lambda" []
    (lets [
      "test">: lambda "x" $ match (ref testTypeUnionPolymorphicRecursiveNameDef)
          (just $ string "other") [
          "value">: lambda "i" $ primitive _literals_showInt32 @@ var "i"]
        @@ var "x"] $
      var "test")
    (letsTyped [
      ("test",
       lambdaTyped "x" (T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) T.int32) $
         tyapp (match (ref testTypeUnionPolymorphicRecursiveNameDef)
             (just $ string "other") [
             "value">: lambdaTyped "i" T.int32 $ primitive _literals_showInt32 @@ var "i"]) T.int32
           @@ var "x",
       T.mono $ T.function (T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) T.int32) T.string)] $
      var "test")
    (T.function (T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) T.int32) T.string),
  checkTest "applied generic UnionPolymorphicRecursive in lambda" []
    (lets [
      "test">: lambda "x" $ match (ref testTypeUnionPolymorphicRecursiveNameDef)
          (just $ string "other") [
          "value">: lambda "ignored" $ string "foo"]
        @@ var "x"] $
      var "test")
    (tylam "t0" $ letsTyped [
      ("test",
       tylam "t1" $ lambdaTyped "x" (T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) (T.var "t1")) $
         tyapp (match (ref testTypeUnionPolymorphicRecursiveNameDef)
             (just $ string "other") [
             "value">: lambdaTyped "ignored" (T.var "t1") $ string "foo"]) (T.var "t1")
           @@ var "x",
       T.poly ["t1"] $ T.function (T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) (T.var "t1")) T.string)] $
      tyapp (var "test") $ T.var "t0")
    (T.forAll "t0" $ T.function (T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) (T.var "t0")) T.string)]

usingKernelTypesTests :: TTerm TestGroup
usingKernelTypesTests = subgroup "Using kernel types" [
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

unionEliminationsWithDefaultsTests :: TTerm TestGroup
unionEliminationsWithDefaultsTests = subgroup "Union eliminations with defaults" [
  checkTest "match Comparison with default case" []
    (match (ref testTypeComparisonNameDef) (just (string "unknown")) [
      "lessThan">: lambda "x" (string "less"),
      "equalTo">: lambda "x" (string "equal")])
    (match (ref testTypeComparisonNameDef) (just (string "unknown")) [
      "lessThan">: lambdaTyped "x" T.unit (string "less"),
      "equalTo">: lambdaTyped "x" T.unit (string "equal")])
    (T.function (Core.typeVariable $ ref testTypeComparisonNameDef) T.string),
  checkTest "match Number with default case" []
    (match (ref testTypeNumberNameDef) (just (int32 (-1))) [
      "int">: lambda "i" (var "i")])
    (match (ref testTypeNumberNameDef) (just (int32 (-1))) [
      "int">: lambdaTyped "i" T.int32 (var "i")])
    (T.function (Core.typeVariable $ ref testTypeNumberNameDef) T.int32),
  checkTest "match UnionMonomorphic with default" []
    (match (ref testTypeUnionMonomorphicNameDef) (just (string "fallback")) [
      "bool">: lambda "b" (primitive _literals_showBoolean @@ var "b"),
      "string">: lambda "s" (var "s")])
    (match (ref testTypeUnionMonomorphicNameDef) (just (string "fallback")) [
      "bool">: lambdaTyped "b" T.boolean (primitive _literals_showBoolean @@ var "b"),
      "string">: lambdaTyped "s" T.string (var "s")])
    (T.function (Core.typeVariable $ ref testTypeUnionMonomorphicNameDef) T.string)]

nestedUnionEliminationsTests :: TTerm TestGroup
nestedUnionEliminationsTests = subgroup "Nested union eliminations" [
  checkTest "nested match statements" []
    (match (ref testTypePersonOrSomethingNameDef) nothing [
      "person">: lambda "p" (project (ref testTypePersonNameDef) (name "firstName") @@ var "p"),
      "other">: lambda "x" (
        match (ref testTypeNumberNameDef) nothing [
          "int">: lambda "i" (primitive _literals_showInt32 @@ var "i"),
          "float">: lambda "f" (primitive _literals_showFloat32 @@ var "f")] @@
        var "x")])
    (tyapp (match (ref testTypePersonOrSomethingNameDef) nothing [
      "person">: lambdaTyped "p" (Core.typeVariable $ ref testTypePersonNameDef) (project (ref testTypePersonNameDef) (name "firstName") @@ var "p"),
      "other">: lambdaTyped "x" (Core.typeVariable $ ref testTypeNumberNameDef) (
        match (ref testTypeNumberNameDef) nothing [
          "int">: lambdaTyped "i" T.int32 (primitive _literals_showInt32 @@ var "i"),
          "float">: lambdaTyped "f" T.float32 (primitive _literals_showFloat32 @@ var "f")] @@
        var "x")]) (Core.typeVariable $ ref testTypeNumberNameDef))
    (T.function (T.apply (Core.typeVariable $ ref testTypePersonOrSomethingNameDef) (Core.typeVariable $ ref testTypeNumberNameDef)) T.string),
  checkTest "match in tuple" []
    (tuple [
      match (ref testTypeComparisonNameDef) nothing [
        "lessThan">: lambda "x" (int32 1),
        "equalTo">: lambda "x" (int32 0),
        "greaterThan">: lambda "x" (int32 (-1))],
      string "context"])
    (tuple [
      match (ref testTypeComparisonNameDef) nothing [
        "lessThan">: lambdaTyped "x" T.unit (int32 1),
        "equalTo">: lambdaTyped "x" T.unit (int32 0),
        "greaterThan">: lambdaTyped "x" T.unit (int32 (-1))],
      string "context"])
    (T.product [T.function (Core.typeVariable $ ref testTypeComparisonNameDef) T.int32, T.string])]

unionEliminationsInComplexContextsTests :: TTerm TestGroup
unionEliminationsInComplexContextsTests = subgroup "Union eliminations in complex contexts" [
  checkTest "match in let binding" []
    (lets ["matcher">: match (ref testTypeComparisonNameDef) nothing [
             "lessThan">: lambda "x" (string "less"),
             "equalTo">: lambda "x" (string "equal"),
             "greaterThan">: lambda "x" (string "greater")]] $
          var "matcher")
    (letsTyped [("matcher", match (ref testTypeComparisonNameDef) nothing [
                   "lessThan">: lambdaTyped "x" T.unit (string "less"),
                   "equalTo">: lambdaTyped "x" T.unit (string "equal"),
                   "greaterThan">: lambdaTyped "x" T.unit (string "greater")],
                 T.mono $ T.function (Core.typeVariable $ ref testTypeComparisonNameDef) T.string)] $
      var "matcher")
    (T.function (Core.typeVariable $ ref testTypeComparisonNameDef) T.string),
  checkTest "match in record" []
    (record (ref testTypePersonNameDef) [
      "firstName">: (match (ref testTypePersonOrSomethingNameDef) nothing [
        "person">: lambda "p" (project (ref testTypePersonNameDef) (name "firstName") @@ var "p"),
        "other">: lambda "x" (var "x")] @@
       inject (ref testTypePersonOrSomethingNameDef) "other" (string "John")),
      "lastName">: (string "Doe"),
      "age">: (int32 30)])
    (record (ref testTypePersonNameDef) [
      "firstName">: (tyapp (match (ref testTypePersonOrSomethingNameDef) nothing [
        "person">: lambdaTyped "p" (Core.typeVariable $ ref testTypePersonNameDef) (project (ref testTypePersonNameDef) (name "firstName") @@ var "p"),
        "other">: lambdaTyped "x" T.string (var "x")]) T.string @@
       tyapp (inject (ref testTypePersonOrSomethingNameDef) "other" (string "John")) T.string),
      "lastName">: (string "Doe"),
      "age">: (int32 30)])
    (Core.typeVariable $ ref testTypePersonNameDef),
  checkTest "match with polymorphic result in list" []
    (list [
      match (ref testTypePersonOrSomethingNameDef) nothing [
        "person">: lambda "p" (project (ref testTypePersonNameDef) (name "age") @@ var "p"),
        "other">: lambda "x" (var "x")] @@
      inject (ref testTypePersonOrSomethingNameDef) "other" (int32 25),
      int32 30])
    (list [
      tyapp (match (ref testTypePersonOrSomethingNameDef) nothing [
        "person">: lambdaTyped "p" (Core.typeVariable $ ref testTypePersonNameDef) (project (ref testTypePersonNameDef) (name "age") @@ var "p"),
        "other">: lambdaTyped "x" T.int32 (var "x")]) T.int32 @@
      tyapp (inject (ref testTypePersonOrSomethingNameDef) "other" (int32 25)) T.int32,
      int32 30])
    (T.list T.int32)]

multiParameterPolymorphicCaseStatementsTests :: TTerm TestGroup
multiParameterPolymorphicCaseStatementsTests = subgroup "Multi-parameter polymorphic case statements" [
  checkTest "case Either converting both to string" []
    (match (ref testTypeEitherNameDef) nothing [
      "left">: lambda "x" $ primitive _literals_showInt32 @@ var "x",
      "right">: lambda "y" $ primitive _literals_showFloat32 @@ var "y"])
    (tyapps (match (ref testTypeEitherNameDef) nothing [
      "left">: lambdaTyped "x" T.int32 (primitive _literals_showInt32 @@ var "x"),
      "right">: lambdaTyped "y" T.float32 (primitive _literals_showFloat32 @@ var "y")]) [T.int32, T.float32])
    (T.function
      (T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.int32, T.float32])
      T.string),
  checkTest "case Either applied to injection" []
    (match (ref testTypeEitherNameDef) nothing [
      "left">: lambda "n" $ primitive _math_add @@ var "n" @@ int32 1,
      "right">: lambda "s" $ primitive _strings_length @@ var "s"] @@
     inject (ref testTypeEitherNameDef) "left" (int32 42))
    (tyapps (match (ref testTypeEitherNameDef) nothing [
      "left">: lambdaTyped "n" T.int32 (primitive _math_add @@ var "n" @@ int32 1),
      "right">: lambdaTyped "s" T.string (primitive _strings_length @@ var "s")]) [T.int32, T.string] @@
     tyapps (inject (ref testTypeEitherNameDef) "left" (int32 42)) [T.int32, T.string])
    T.int32,
  checkTest "case Either with Triple and nested projections" []
    (lambda "triple" $
      match (ref testTypeEitherNameDef) nothing [
        "left">: lambda "coords" $
          project (ref testTypeLatLonPolyNameDef) (name "lat") @@ var "coords",
        "right">: lambda "t" $
          project (ref testTypeTripleNameDef) (name "first") @@ var "t"] @@
      (project (ref testTypeTripleNameDef) (name "second") @@ var "triple"))
    (tylams ["t0", "t1", "t2", "t3", "t4"] $
      lambdaTyped "triple"
        (T.applys (Core.typeVariable $ ref testTypeTripleNameDef)
          [T.var "t0",
           T.applys (Core.typeVariable $ ref testTypeEitherNameDef)
             [T.apply (Core.typeVariable $ ref testTypeLatLonPolyNameDef) (T.var "t1"),
              T.applys (Core.typeVariable $ ref testTypeTripleNameDef) [T.var "t1", T.var "t2", T.var "t3"]],
           T.var "t4"]) $
      tyapps (match (ref testTypeEitherNameDef) nothing [
        "left">: lambdaTyped "coords" (T.apply (Core.typeVariable $ ref testTypeLatLonPolyNameDef) (T.var "t1")) $
          tyapp (project (ref testTypeLatLonPolyNameDef) (name "lat")) (T.var "t1") @@ var "coords",
        "right">: lambdaTyped "t" (T.applys (Core.typeVariable $ ref testTypeTripleNameDef) [T.var "t1", T.var "t2", T.var "t3"]) $
          tyapps (project (ref testTypeTripleNameDef) (name "first")) [T.var "t1", T.var "t2", T.var "t3"] @@ var "t"])
        [T.apply (Core.typeVariable $ ref testTypeLatLonPolyNameDef) (T.var "t1"),
         T.applys (Core.typeVariable $ ref testTypeTripleNameDef) [T.var "t1", T.var "t2", T.var "t3"]] @@
      (tyapps (project (ref testTypeTripleNameDef) (name "second"))
        [T.var "t0",
         T.applys (Core.typeVariable $ ref testTypeEitherNameDef)
           [T.apply (Core.typeVariable $ ref testTypeLatLonPolyNameDef) (T.var "t1"),
            T.applys (Core.typeVariable $ ref testTypeTripleNameDef) [T.var "t1", T.var "t2", T.var "t3"]],
         T.var "t4"] @@ var "triple"))
    (T.forAlls ["t0", "t1", "t2", "t3", "t4"] $
      T.function
        (T.applys (Core.typeVariable $ ref testTypeTripleNameDef)
          [T.var "t0",
           T.applys (Core.typeVariable $ ref testTypeEitherNameDef)
             [T.apply (Core.typeVariable $ ref testTypeLatLonPolyNameDef) (T.var "t1"),
              T.applys (Core.typeVariable $ ref testTypeTripleNameDef) [T.var "t1", T.var "t2", T.var "t3"]],
           T.var "t4"])
        (T.var "t1")),
  checkTest "case Either with polymorphic let bindings" []
    (lets ["makeLeft">: lambda "x" $ inject (ref testTypeEitherNameDef) "left" (var "x"),
           "makeRight">: lambda "y" $ inject (ref testTypeEitherNameDef) "right" (var "y")] $
      lambda "flag" $
        match (ref testTypeEitherNameDef) nothing [
          "left">: lambda "n" $ var "makeRight" @@ (primitive _math_add @@ var "n" @@ int32 10),
          "right">: lambda "s" $ var "makeLeft" @@ (primitive _strings_length @@ var "s")] @@
        var "flag")
    (letsTyped [("makeLeft", tylams ["t0", "t1"] $ lambdaTyped "x" (T.var "t0") $ tyapps (inject (ref testTypeEitherNameDef) "left" (var "x")) [T.var "t0", T.var "t1"],
                 T.poly ["t0", "t1"] $ T.function (T.var "t0") (T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.var "t0", T.var "t1"])),
                ("makeRight", tylams ["t0", "t1"] $ lambdaTyped "y" (T.var "t0") $ tyapps (inject (ref testTypeEitherNameDef) "right" (var "y")) [T.var "t1", T.var "t0"],
                 T.poly ["t0", "t1"] $ T.function (T.var "t0") (T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.var "t1", T.var "t0"]))] $
      lambdaTyped "flag" (T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.int32, T.string]) $
        tyapps (match (ref testTypeEitherNameDef) nothing [
          "left">: lambdaTyped "n" T.int32 $ tyapps (var "makeRight") [T.int32, T.int32] @@ (primitive _math_add @@ var "n" @@ int32 10),
          "right">: lambdaTyped "s" T.string $ tyapps (var "makeLeft") [T.int32, T.int32] @@ (primitive _strings_length @@ var "s")]) [T.int32, T.string] @@
        var "flag")
    (T.function (T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.int32, T.string]) (T.applys (Core.typeVariable $ ref testTypeEitherNameDef) [T.int32, T.int32]))]

higherOrderUnionEliminationsTests :: TTerm TestGroup
higherOrderUnionEliminationsTests = subgroup "Higher-order union eliminations" [
  checkTest "map match over list" []
    (primitive _lists_map @@
     (match (ref testTypeComparisonNameDef) nothing [
       "lessThan">: lambda "x" (string "less"),
       "equalTo">: lambda "x" (string "equal"),
       "greaterThan">: lambda "x" (string "greater")]) @@
     list [injectUnit (ref testTypeComparisonNameDef) "lessThan",
           injectUnit (ref testTypeComparisonNameDef) "equalTo"])
    (tyapps (primitive _lists_map) [Core.typeVariable $ ref testTypeComparisonNameDef, T.string] @@
     (match (ref testTypeComparisonNameDef) nothing [
       "lessThan">: lambdaTyped "x" T.unit (string "less"),
       "equalTo">: lambdaTyped "x" T.unit (string "equal"),
       "greaterThan">: lambdaTyped "x" T.unit (string "greater")]) @@
     list [injectUnit (ref testTypeComparisonNameDef) "lessThan",
           injectUnit (ref testTypeComparisonNameDef) "equalTo"])
    (T.list T.string),
  checkTest "compose match with other functions" []
    (lambda "comp" $
     primitive _strings_length @@
     (match (ref testTypeComparisonNameDef) nothing [
       "lessThan">: lambda "x" (string "less"),
       "equalTo">: lambda "x" (string "equal"),
       "greaterThan">: lambda "x" (string "greater")] @@
      var "comp"))
    (lambdaTyped "comp" (Core.typeVariable $ ref testTypeComparisonNameDef) $
     primitive _strings_length @@
     (match (ref testTypeComparisonNameDef) nothing [
       "lessThan">: lambdaTyped "x" T.unit (string "less"),
       "equalTo">: lambdaTyped "x" T.unit (string "equal"),
       "greaterThan">: lambdaTyped "x" T.unit (string "greater")] @@
      var "comp"))
    (T.function (Core.typeVariable $ ref testTypeComparisonNameDef) T.int32),
  checkTest "match in lambda body" []
    (lambda "unionValue" $
     match (ref testTypeNumberNameDef) nothing [
       "int">: lambda "i" (primitive _math_add @@ var "i" @@ int32 1),
       "float">: lambda "f" (int32 0)] @@
     var "unionValue")
    (lambdaTyped "unionValue" (Core.typeVariable $ ref testTypeNumberNameDef) $
     match (ref testTypeNumberNameDef) nothing [
       "int">: lambdaTyped "i" T.int32 (primitive _math_add @@ var "i" @@ int32 1),
       "float">: lambdaTyped "f" T.float32 (int32 0)] @@
     var "unionValue")
    (T.function (Core.typeVariable $ ref testTypeNumberNameDef) T.int32)]

recursiveUnionEliminationsTests :: TTerm TestGroup
recursiveUnionEliminationsTests = subgroup "Recursive union eliminations" [
  checkTest "match HydraType recursively" []
    (match (ref testTypeHydraTypeNameDef) nothing [
      "literal">: lambda "lit" (
        match (ref testTypeHydraLiteralTypeNameDef) nothing [
          "boolean">: lambda "b" (primitive _literals_showBoolean @@ var "b"),
          "string">: lambda "s" (var "s")] @@
        var "lit"),
      "list">: lambda "nested" (string "list")])
    (match (ref testTypeHydraTypeNameDef) nothing [
      "literal">: lambdaTyped "lit" (Core.typeVariable $ ref testTypeHydraLiteralTypeNameDef) (
        match (ref testTypeHydraLiteralTypeNameDef) nothing [
          "boolean">: lambdaTyped "b" T.boolean (primitive _literals_showBoolean @@ var "b"),
          "string">: lambdaTyped "s" T.string (var "s")] @@
        var "lit"),
      "list">: lambdaTyped "nested" (Core.typeVariable $ ref testTypeHydraTypeNameDef) (string "list")])
    (T.function (Core.typeVariable $ ref testTypeHydraTypeNameDef) T.string)]

------ Wrapped terms ------

wrappedTermsTests :: TTerm TestGroup
wrappedTermsTests = supergroup "Wrapped terms" [
  monomorphicWrappedTermsTests,
  polymorphicWrappedTermsTests,
  wrappedTermsInComplexContextsTests,
  nestedWrappedTermsTests,
  multipleWrappingLevelsTests,
  multiParameterPolymorphicWrappersTests]

monomorphicWrappedTermsTests :: TTerm TestGroup
monomorphicWrappedTermsTests = subgroup "Monomorphic wrapped terms" [
  noChange "string alias"
    (wrap (ref testTypeStringAliasNameDef) (string "hello"))
    (Core.typeVariable $ ref testTypeStringAliasNameDef),
  noChange "wrapped integer"
    (wrap (ref testTypeStringAliasNameDef) (string "wrapped"))
    (Core.typeVariable $ ref testTypeStringAliasNameDef),
  noChange "wrapped in tuple"
    (tuple [wrap (ref testTypeStringAliasNameDef) (string "first"),
            string "second"])
    (T.product [Core.typeVariable $ ref testTypeStringAliasNameDef, T.string])]

polymorphicWrappedTermsTests :: TTerm TestGroup
polymorphicWrappedTermsTests = subgroup "Polymorphic wrapped terms" [
  checkTest "polymorphic wrapper with int" []
    (wrap (ref testTypePolymorphicWrapperNameDef) (list [int32 1, int32 2]))
    (tyapp (wrap (ref testTypePolymorphicWrapperNameDef) (list [int32 1, int32 2])) T.int32)
    (T.apply (Core.typeVariable $ ref testTypePolymorphicWrapperNameDef) T.int32),
  checkTest "polymorphic wrapper with string" []
    (wrap (ref testTypePolymorphicWrapperNameDef) (list [string "a", string "b"]))
    (tyapp (wrap (ref testTypePolymorphicWrapperNameDef) (list [string "a", string "b"])) T.string)
    (T.apply (Core.typeVariable $ ref testTypePolymorphicWrapperNameDef) T.string),
  checkTest "polymorphic wrapper from lambda" []
    (lambda "x" $ wrap (ref testTypePolymorphicWrapperNameDef) (list [var "x"]))
    (tylam "t0" $ lambdaTyped "x" (T.var "t0") $ tyapp (wrap (ref testTypePolymorphicWrapperNameDef) (list [var "x"])) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.var "t0") (T.apply (Core.typeVariable $ ref testTypePolymorphicWrapperNameDef) (T.var "t0")))]

wrappedTermsInComplexContextsTests :: TTerm TestGroup
wrappedTermsInComplexContextsTests = subgroup "Wrapped terms in complex contexts" [
  noChange "wrapped in record"
    (record (ref testTypePersonNameDef) [
      "firstName">: (string "John"),
      "lastName">: (string "Doe"),
      "age">: (int32 30)])
    (Core.typeVariable $ ref testTypePersonNameDef),
  checkTest "wrapped in let binding" []
    (lets ["alias">: wrap (ref testTypeStringAliasNameDef) (string "test")] $
          var "alias")
    (letsTyped [("alias", wrap (ref testTypeStringAliasNameDef) (string "test"),
                 T.mono $ Core.typeVariable $ ref testTypeStringAliasNameDef)] $
      var "alias")
    (Core.typeVariable $ ref testTypeStringAliasNameDef),
  noChange "wrapped in list"
    (list [wrap (ref testTypeStringAliasNameDef) (string "first"),
           wrap (ref testTypeStringAliasNameDef) (string "second")])
    (T.list $ Core.typeVariable $ ref testTypeStringAliasNameDef)]

nestedWrappedTermsTests :: TTerm TestGroup
nestedWrappedTermsTests = subgroup "Nested wrapped terms" [
  checkTest "wrapped tuple" []
    (wrap (ref testTypePolymorphicWrapperNameDef) (list [tuple [int32 1, string "a"]]))
    (tyapp (wrap (ref testTypePolymorphicWrapperNameDef) (list [tuple [int32 1, string "a"]])) (T.product [T.int32, T.string]))
    (T.apply (Core.typeVariable $ ref testTypePolymorphicWrapperNameDef) (T.product [T.int32, T.string])),
  checkTest "wrapped optional" []
    (wrap (ref testTypePolymorphicWrapperNameDef) (list [Core.termMaybe $ just $ int32 42]))
    (tyapp (wrap (ref testTypePolymorphicWrapperNameDef) (list [Core.termMaybe $ just $ int32 42])) (T.optional T.int32))
    (T.apply (Core.typeVariable $ ref testTypePolymorphicWrapperNameDef) (T.optional T.int32)),
  checkTest "wrapped map" []
    (wrap (ref testTypePolymorphicWrapperNameDef) (list [mapTerm [(string "key", int32 42)]]))
    (tyapp (wrap (ref testTypePolymorphicWrapperNameDef) (list [mapTerm [(string "key", int32 42)]])) (T.map T.string T.int32))
    (T.apply (Core.typeVariable $ ref testTypePolymorphicWrapperNameDef) (T.map T.string T.int32))]

multipleWrappingLevelsTests :: TTerm TestGroup
multipleWrappingLevelsTests = subgroup "Multiple wrapping levels" [
  noChange "wrapped in optional"
    (Core.termMaybe $ just $ wrap (ref testTypeStringAliasNameDef) (string "wrapped"))
    (T.optional $ Core.typeVariable $ ref testTypeStringAliasNameDef),
  checkTest "list of wrapped polymorphic" []
    (list [wrap (ref testTypePolymorphicWrapperNameDef) (list [int32 1]),
           wrap (ref testTypePolymorphicWrapperNameDef) (list [int32 2])])
    (list [tyapp (wrap (ref testTypePolymorphicWrapperNameDef) (list [int32 1])) T.int32,
           tyapp (wrap (ref testTypePolymorphicWrapperNameDef) (list [int32 2])) T.int32])
    (T.list $ T.apply (Core.typeVariable $ ref testTypePolymorphicWrapperNameDef) T.int32)]

multiParameterPolymorphicWrappersTests :: TTerm TestGroup
multiParameterPolymorphicWrappersTests = subgroup "Multi-parameter polymorphic wrappers" [
  checkTest "symmetric triple wrapping simple types" []
    (wrap (ref testTypeSymmetricTripleNameDef) $
      record (ref testTypeTripleNameDef) [
        "first">: (int32 1),
        "second">: (string "edge"),
        "third">: (int32 2)])
    (tyapps (wrap (ref testTypeSymmetricTripleNameDef) $
      tyapps (record (ref testTypeTripleNameDef) [
        "first">: (int32 1),
        "second">: (string "edge"),
        "third">: (int32 2)]) [T.int32, T.string, T.int32])
      [T.int32, T.string])
    (T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.int32, T.string]),
  checkTest "symmetric triple from lambda" []
    (lambda "v1" $ lambda "e" $ lambda "v2" $
      wrap (ref testTypeSymmetricTripleNameDef) $
        record (ref testTypeTripleNameDef) [
          "first">: (var "v1"),
          "second">: (var "e"),
          "third">: (var "v2")])
    (tylams ["t0", "t1"] $
      lambdaTyped "v1" (T.var "t0") $
      lambdaTyped "e" (T.var "t1") $
      lambdaTyped "v2" (T.var "t0") $
      tyapps (wrap (ref testTypeSymmetricTripleNameDef) $
        tyapps (record (ref testTypeTripleNameDef) [
          "first">: (var "v1"),
          "second">: (var "e"),
          "third">: (var "v2")]) [T.var "t0", T.var "t1", T.var "t0"])
        [T.var "t0", T.var "t1"])
    (T.forAlls ["t0", "t1"] $
      T.function (T.var "t0") $
      T.function (T.var "t1") $
      T.function (T.var "t0") $
      T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.var "t0", T.var "t1"]),
  checkTest "symmetric triple with nested polymorphic types and foldl" []
    (lets ["sumList">: lambda "lst" $
            primitive _lists_foldl @@
            (lambda "acc" $ lambda "x" $ primitive _math_add @@ var "acc" @@ var "x") @@
            int32 0 @@
            var "lst"] $
      lambda "nums1" $ lambda "nums2" $
        wrap (ref testTypeSymmetricTripleNameDef) $
          record (ref testTypeTripleNameDef) [
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
        tyapps (wrap (ref testTypeSymmetricTripleNameDef) $
          tyapps (record (ref testTypeTripleNameDef) [
            "first">: (var "sumList" @@ var "nums1"),
            "second">: (list [var "nums1", var "nums2"]),
            "third">: (var "sumList" @@ var "nums2")])
            [T.int32, T.list (T.list T.int32), T.int32])
          [T.int32, T.list (T.list T.int32)])
    (T.function (T.list T.int32) $
      T.function (T.list T.int32) $
      T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.int32, T.list (T.list T.int32)])]

------ Wrap eliminations ------

wrapEliminationsTests :: TTerm TestGroup
wrapEliminationsTests = supergroup "Wrap eliminations" [
  monomorphicUnwrappingTests,
  polymorphicUnwrappingTests,
  unwrapEliminationsInApplicationsTests,
  unwrapInComplexContextsTests,
  multiParameterPolymorphicUnwrappersTests,
  chainedUnwrappingTests,
  multipleUnwrapOperationsTests]

monomorphicUnwrappingTests :: TTerm TestGroup
monomorphicUnwrappingTests = subgroup "Monomorphic unwrapping" [
  noChange "unwrap string alias"
    (unwrap (ref testTypeStringAliasNameDef))
    (T.function (Core.typeVariable $ ref testTypeStringAliasNameDef) T.string)]

polymorphicUnwrappingTests :: TTerm TestGroup
polymorphicUnwrappingTests = subgroup "Polymorphic unwrapping" [
  checkTest "unwrap polymorphic wrapper" []
    (unwrap (ref testTypePolymorphicWrapperNameDef))
    (tylam "t0" $ tyapp (unwrap (ref testTypePolymorphicWrapperNameDef)) (T.var "t0"))
    (T.forAll "t0" $ T.function (T.apply (Core.typeVariable $ ref testTypePolymorphicWrapperNameDef) (T.var "t0")) (T.list $ T.var "t0"))]

unwrapEliminationsInApplicationsTests :: TTerm TestGroup
unwrapEliminationsInApplicationsTests = subgroup "Unwrap eliminations in applications" [
  noChange "unwrap applied to wrapped term"
    (unwrap (ref testTypeStringAliasNameDef) @@ wrap (ref testTypeStringAliasNameDef) (string "hello"))
    T.string,
  checkTest "unwrap polymorphic applied" []
    (unwrap (ref testTypePolymorphicWrapperNameDef) @@ wrap (ref testTypePolymorphicWrapperNameDef) (list [int32 1, int32 2]))
    (tyapp (unwrap (ref testTypePolymorphicWrapperNameDef)) T.int32 @@ tyapp (wrap (ref testTypePolymorphicWrapperNameDef) (list [int32 1, int32 2])) T.int32)
    (T.list T.int32)]

unwrapInComplexContextsTests :: TTerm TestGroup
unwrapInComplexContextsTests = subgroup "Unwrap in complex contexts" [
  checkTest "unwrap in let binding" []
    (lets ["unwrapper" >: unwrap (ref testTypeStringAliasNameDef),
           "wrapped" >: wrap (ref testTypeStringAliasNameDef) (string "test")] $
          var "unwrapper" @@ var "wrapped")
    (letsTyped [
      ("unwrapper", unwrap (ref testTypeStringAliasNameDef), T.mono $ T.function (Core.typeVariable $ ref testTypeStringAliasNameDef) T.string),
      ("wrapped", wrap (ref testTypeStringAliasNameDef) (string "test"), T.mono $ Core.typeVariable $ ref testTypeStringAliasNameDef)] $
      var "unwrapper" @@ var "wrapped")
    T.string,
  noChange "unwrap in tuple"
    (tuple [unwrap (ref testTypeStringAliasNameDef), string "context"])
    (T.product [T.function (Core.typeVariable $ ref testTypeStringAliasNameDef) T.string, T.string]),
  checkTest "unwrap in lambda" []
    (lambda "wrapped" $ unwrap (ref testTypeStringAliasNameDef) @@ var "wrapped")
    (lambdaTyped "wrapped" (Core.typeVariable $ ref testTypeStringAliasNameDef) $ unwrap (ref testTypeStringAliasNameDef) @@ var "wrapped")
    (T.function (Core.typeVariable $ ref testTypeStringAliasNameDef) T.string)]

multiParameterPolymorphicUnwrappersTests :: TTerm TestGroup
multiParameterPolymorphicUnwrappersTests = subgroup "Multi-parameter polymorphic unwrappers" [
  checkTest "unwrap symmetric triple to tuple" []
    (lambda "st" $
      tuple [
        project (ref testTypeTripleNameDef) (name "first") @@ (unwrap (ref testTypeSymmetricTripleNameDef) @@ var "st"),
        project (ref testTypeTripleNameDef) (name "third") @@ (unwrap (ref testTypeSymmetricTripleNameDef) @@ var "st")])
    (tylams ["t0", "t1"] $
      lambdaTyped "st" (T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.var "t0", T.var "t1"]) $
      tuple [
        tyapps (project (ref testTypeTripleNameDef) (name "first")) [T.var "t0", T.var "t1", T.var "t0"] @@
          (tyapps (unwrap (ref testTypeSymmetricTripleNameDef)) [T.var "t0", T.var "t1"] @@ var "st"),
        tyapps (project (ref testTypeTripleNameDef) (name "third")) [T.var "t0", T.var "t1", T.var "t0"] @@
          (tyapps (unwrap (ref testTypeSymmetricTripleNameDef)) [T.var "t0", T.var "t1"] @@ var "st")])
    (T.forAlls ["t0", "t1"] $
      T.function
        (T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.var "t0", T.var "t1"])
        (T.product [T.var "t0", T.var "t0"])),
  checkTest "unwrap and collect edges in set" []
    (lets ["getEdge" >: lambda "st" $
            project (ref testTypeTripleNameDef) (name "second") @@ (unwrap (ref testTypeSymmetricTripleNameDef) @@ var "st")] $
      lambda "triples" $
        primitive _sets_map @@ var "getEdge" @@ var "triples")
    (tylams ["t0", "t1"] $
      letsTyped [("getEdge",
                 tylams ["t2", "t3"] $
                 lambdaTyped "st" (T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.var "t2", T.var "t3"]) $
                   tyapps (project (ref testTypeTripleNameDef) (name "second")) [T.var "t2", T.var "t3", T.var "t2"] @@
                   (tyapps (unwrap (ref testTypeSymmetricTripleNameDef)) [T.var "t2", T.var "t3"] @@ var "st"),
                 T.poly ["t2", "t3"] $ T.function
                   (T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.var "t2", T.var "t3"])
                   (T.var "t3"))] $
      lambdaTyped "triples" (T.set $ T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.var "t0", T.var "t1"]) $
        tyapps (primitive _sets_map) [T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.var "t0", T.var "t1"], T.var "t1"] @@
        (tyapps (var "getEdge") [T.var "t0", T.var "t1"]) @@
        var "triples")
    (T.forAlls ["t0", "t1"] $
      T.function
        (T.set $ T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.var "t0", T.var "t1"])
        (T.set $ T.var "t1")),

  checkTest "unwrap with maybe to handle optional symmetric triple" []
    (lambda "mst" $
      primitive _maybes_maybe @@
      (Core.termMaybe nothing) @@
      (lambda "st" $ Core.termMaybe $
        just $ project (ref testTypeTripleNameDef) (name "second") @@ (unwrap (ref testTypeSymmetricTripleNameDef) @@ var "st")) @@
      var "mst")
    (tylams ["t0", "t1"] $
      lambdaTyped "mst" (T.optional $ T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.var "t0", T.var "t1"]) $
      tyapps (primitive _maybes_maybe)
        [T.optional (T.var "t1"),
         T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.var "t0", T.var "t1"]] @@
      tyapp (Core.termMaybe nothing) (T.var "t1") @@
      (lambdaTyped "st" (T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.var "t0", T.var "t1"]) $
        Core.termMaybe $ just $
        (tyapps (project (ref testTypeTripleNameDef) (name "second")) [T.var "t0", T.var "t1", T.var "t0"] @@
         (tyapps (unwrap (ref testTypeSymmetricTripleNameDef)) [T.var "t0", T.var "t1"] @@ var "st"))) @@
      var "mst")
    (T.forAlls ["t0", "t1"] $
      T.function
        (T.optional $ T.applys (Core.typeVariable $ ref testTypeSymmetricTripleNameDef) [T.var "t0", T.var "t1"])
        (T.optional $ T.var "t1"))]

chainedUnwrappingTests :: TTerm TestGroup
chainedUnwrappingTests = subgroup "Chained unwrapping" [
  checkTest "unwrap then process" []
    (lambda "wrapped" $
      primitive _strings_cat2 @@ (unwrap (ref testTypeStringAliasNameDef) @@ var "wrapped") @@ string " suffix")
    (lambdaTyped "wrapped" (Core.typeVariable $ ref testTypeStringAliasNameDef) $
      primitive _strings_cat2 @@ (unwrap (ref testTypeStringAliasNameDef) @@ var "wrapped") @@ string " suffix")
    (T.function (Core.typeVariable $ ref testTypeStringAliasNameDef) T.string),
  checkTest "unwrap polymorphic then map" []
    (lambda "wrappedList" $
      primitive _lists_map @@ (primitive _math_add @@ int32 1) @@ (unwrap (ref testTypePolymorphicWrapperNameDef) @@ var "wrappedList"))
    (lambdaTyped "wrappedList" (T.apply (Core.typeVariable $ ref testTypePolymorphicWrapperNameDef) T.int32) $
      (tyapps (primitive _lists_map) [T.int32, T.int32]) @@ (primitive _math_add @@ int32 1) @@ (tyapp (unwrap (ref testTypePolymorphicWrapperNameDef)) T.int32 @@ var "wrappedList"))
    (T.function (T.apply (Core.typeVariable $ ref testTypePolymorphicWrapperNameDef) T.int32) (T.list T.int32))]

multipleUnwrapOperationsTests :: TTerm TestGroup
multipleUnwrapOperationsTests = subgroup "Multiple unwrap operations" [
  checkTest "unwrap different types" []
    (lambda "stringWrapped" $
      lambda "listWrapped" $
        tuple [
          unwrap (ref testTypeStringAliasNameDef) @@ var "stringWrapped",
          unwrap (ref testTypePolymorphicWrapperNameDef) @@ var "listWrapped"])
    (tylam "t0" $ lambdaTyped "stringWrapped" (Core.typeVariable $ ref testTypeStringAliasNameDef) $
      lambdaTyped "listWrapped" (T.apply (Core.typeVariable $ ref testTypePolymorphicWrapperNameDef) (T.var "t0")) $
        tuple [
          unwrap (ref testTypeStringAliasNameDef) @@ var "stringWrapped",
          tyapp (unwrap (ref testTypePolymorphicWrapperNameDef)) (T.var "t0") @@ var "listWrapped"])
    (T.forAll "t0" $ T.function (Core.typeVariable $ ref testTypeStringAliasNameDef)
      (T.function (T.apply (Core.typeVariable $ ref testTypePolymorphicWrapperNameDef) (T.var "t0"))
        (T.product [T.string, T.list $ T.var "t0"])))]

------ Eliminations parent group ------

eliminationsTests :: TTerm TestGroup
eliminationsTests = supergroup "Eliminations" [
  productEliminationsTests,
  recordEliminationsTests,
  unionEliminationsTests,
  wrapEliminationsTests]

-- Note: productEliminationsTests is defined in the original Checking.hs file and needs to be imported or duplicated
productEliminationsTests :: TTerm TestGroup
productEliminationsTests = supergroup "Product eliminations" [
  simpleTupleProjectionsTests,
  polymorphicTupleProjectionsTests,
  projectionsWithVariablesTests,
  projectionsInComplexContextsTests,
  projectionsWithMixedTypesTests,
  projectionsWithPrimitiveFunctionsTests]

simpleTupleProjectionsTests :: TTerm TestGroup
simpleTupleProjectionsTests = subgroup "Simple tuple projections" [
  noChange "projection from pair"
    (untuple 2 0 @@ tuple2 (int32 42) (string "hello"))
    T.int32,
  noChange "second projection from pair"
    (untuple 2 1 @@ tuple2 (int32 42) (string "hello"))
    T.string,
  noChange "projection from triple"
    (untuple 3 1 @@ triple (int32 1) (string "middle") (boolean True))
    T.string,
  noChange "first element of triple"
    (untuple 3 0 @@ triple (boolean False) (int32 100) (string "last"))
    T.boolean,
  noChange "last element of triple"
    (untuple 3 2 @@ triple (boolean False) (int32 100) (string "last"))
    T.string]

polymorphicTupleProjectionsTests :: TTerm TestGroup
polymorphicTupleProjectionsTests = subgroup "Polymorphic tuple projections" [
  checkTest "projection from polymorphic pair" []
    (lambda "x" $ lambda "y" $ untuple 2 0 @@ tuple2 (var "x") (var "y"))
    (tylams ["t0", "t1"] $ lambdaTyped "x" (T.var "t0") $ lambdaTyped "y" (T.var "t1") $
      tyapps (untuple 2 0) [T.var "t0", T.var "t1"] @@ tuple2 (var "x") (var "y"))
    (T.forAlls ["t0", "t1"] $ T.function (T.var "t0") $ T.function (T.var "t1") (T.var "t0")),
  checkTest "second projection from polymorphic pair" []
    (lambda "x" $ lambda "y" $ untuple 2 1 @@ tuple2 (var "x") (var "y"))
    (tylams ["t0", "t1"] $ lambdaTyped "x" (T.var "t0") $ lambdaTyped "y" (T.var "t1") $
      tyapps (untuple 2 1) [T.var "t0", T.var "t1"] @@ tuple2 (var "x") (var "y"))
    (T.forAlls ["t0", "t1"] $ T.function (T.var "t0") $ T.function (T.var "t1") (T.var "t1"))]

projectionsWithVariablesTests :: TTerm TestGroup
projectionsWithVariablesTests = subgroup "Projections with variables" [
  checkTest "projection from variable pair" []
    (lambda "pair" $ untuple 2 0 @@ var "pair")
    (tylams ["t0", "t1"] $ lambdaTyped "pair" (T.product [T.var "t0", T.var "t1"]) $
      tyapps (untuple 2 0) [T.var "t0", T.var "t1"] @@ var "pair")
    (T.forAlls ["t0", "t1"] $ T.function (T.product [T.var "t0", T.var "t1"]) (T.var "t0")),
  checkTest "both projections from variable pair" []
    (lambda "pair" $ tuple [untuple 2 0 @@ var "pair", untuple 2 1 @@ var "pair"])
    (tylams ["t0", "t1"] $ lambdaTyped "pair" (T.product [T.var "t0", T.var "t1"]) $
      tuple [tyapps (untuple 2 0) [T.var "t0", T.var "t1"] @@ var "pair",
             tyapps (untuple 2 1) [T.var "t0", T.var "t1"] @@ var "pair"])
    (T.forAlls ["t0", "t1"] $ T.function (T.product [T.var "t0", T.var "t1"]) (T.product [T.var "t0", T.var "t1"]))]

projectionsInComplexContextsTests :: TTerm TestGroup
projectionsInComplexContextsTests = subgroup "Projections in complex contexts" [
  checkTest "projection in let binding" []
    (lets ["pair">: tuple2 (int32 42) (string "hello"),
           "getFirst">: untuple 2 0] $
          var "getFirst" @@ var "pair")
    (letsTyped [("pair", tuple2 (int32 42) (string "hello"),
                 T.mono $ T.product [T.int32, T.string]),
                ("getFirst", tyapps (untuple 2 0) [T.int32, T.string],
                 T.mono $ T.function (T.product [T.int32, T.string]) T.int32)] $
      var "getFirst" @@ var "pair")
    T.int32,
  noChange "projection in tuple"
    (tuple [untuple 2 0, untuple 2 1])
    (T.product [
      T.forAlls ["t0", "t1"] $ T.function (T.product [T.var "t0", T.var "t1"]) (T.var "t0"),
      T.forAlls ["t0", "t1"] $ T.function (T.product [T.var "t0", T.var "t1"]) (T.var "t1")])]

projectionsWithMixedTypesTests :: TTerm TestGroup
projectionsWithMixedTypesTests = subgroup "Projections with mixed types" [
  noChange "projection from nested tuples"
    (untuple 2 0 @@ tuple2 (tuple2 (int32 1) (string "a")) (boolean True))
    (T.product [T.int32, T.string]),
  checkTest "nested projections" []
    (lambda "nested" $
      untuple 2 0 @@ (untuple 2 0 @@ var "nested"))
    (tylams ["t0", "t1", "t2"] $
      lambdaTyped "nested" (T.product [T.product [T.var "t0", T.var "t1"], T.var "t2"]) $
      tyapps (untuple 2 0) [T.var "t0", T.var "t1"] @@
        (tyapps (untuple 2 0) [T.product [T.var "t0", T.var "t1"], T.var "t2"] @@ var "nested"))
    (T.forAlls ["t0", "t1", "t2"] $
      T.function (T.product [T.product [T.var "t0", T.var "t1"], T.var "t2"]) (T.var "t0"))]

projectionsWithPrimitiveFunctionsTests :: TTerm TestGroup
projectionsWithPrimitiveFunctionsTests = subgroup "Projections with primitive functions" [
  checkTest "map projection over list of pairs" []
    (primitive _lists_map @@ (untuple 2 0) @@
     list [tuple2 (int32 1) (string "a"),
           tuple2 (int32 2) (string "b")])
    (tyapps (primitive _lists_map) [T.product [T.int32, T.string], T.int32] @@
     tyapps (untuple 2 0) [T.int32, T.string] @@
     list [tuple2 (int32 1) (string "a"),
           tuple2 (int32 2) (string "b")])
    (T.list T.int32),
  checkTest "compose projections" []
    (lambda "pairs" $
      primitive _lists_map @@
        (lambda "p" $ tuple2
          (untuple 2 1 @@ var "p")
          (untuple 2 0 @@ var "p")) @@
        var "pairs")
    (tylams ["t0", "t1"] $
      lambdaTyped "pairs" (T.list $ T.product [T.var "t0", T.var "t1"]) $
      tyapps (primitive _lists_map) [T.product [T.var "t0", T.var "t1"], T.product [T.var "t1", T.var "t0"]] @@
        (lambdaTyped "p" (T.product [T.var "t0", T.var "t1"]) $ tuple2
          (tyapps (untuple 2 1) [T.var "t0", T.var "t1"] @@ var "p")
          (tyapps (untuple 2 0) [T.var "t0", T.var "t1"] @@ var "p")) @@
        var "pairs")
    (T.forAlls ["t0", "t1"] $
      T.function (T.list $ T.product [T.var "t0", T.var "t1"]) (T.list $ T.product [T.var "t1", T.var "t0"]))]

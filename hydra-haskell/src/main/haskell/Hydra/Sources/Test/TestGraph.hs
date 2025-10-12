module Hydra.Sources.Test.TestGraph where

import Hydra.Kernel
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Module        as Module
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import qualified Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Types         as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Dsl.TTypes as T
import           Hydra.Dsl.TTerms as TTerms


testGraphDefinition :: String -> TTerm a -> TBinding a
testGraphDefinition = definitionInModule testGraphModule

testGraphModule :: Module
testGraphModule = Module (Namespace "hydra.test.testGraph") elements
    []
    kernelTypesModules $
    Just ("A module defining the graph used in the test suite.")
  where
    elements = [
      el testTypeLatLonNameDef,
      el testTypeLatLonPolyNameDef,
      el latlonRecordDef,
      el testTypeLatLonDef,
      el testTypeLatLonPolyDef,
      el testTypePolymorphicWrapperDef,
      el testTypePolymorphicWrapperNameDef,
      el testTypeStringAliasDef,
      el testTypeStringAliasNameDef,
      el testElementArthurDef,
      el testElementFirstNameDef,
--      el testGraphDef, -- TODO
      el testNamespaceDef,
--      el testSchemaGraphDef, -- TODO
      el testSchemaNamespaceDef,
      el testDataArthurDef,
      el testTypeBuddyListADef,
      el testTypeBuddyListANameDef,
      el testTypeBuddyListBDef,
      el testTypeBuddyListBNameDef,
      el testTypeComparisonDef,
      el testTypeComparisonNameDef,
      el testTypeEitherDef,
      el testTypeEitherNameDef,
      el testTypeIntListDef,
      el testTypeIntListNameDef,
      el testTypeHydraLiteralTypeDef,
      el testTypeHydraLiteralTypeNameDef,
      el testTypeHydraTypeDef,
      el testTypeHydraTypeNameDef,
      el testTypeListDef,
      el testTypeListNameDef,
      el testTypeNumberDef,
      el testTypeNumberNameDef,
      el testTypePersonDef,
      el testTypePersonNameDef,
      el testTypePersonOrSomethingDef,
      el testTypePersonOrSomethingNameDef,
      el testTypeSimpleNumberDef,
      el testTypeSimpleNumberNameDef,
      el testTypeSymmetricTripleDef,
      el testTypeSymmetricTripleNameDef,
      el testTypeTimestampDef,
      el testTypeTimestampNameDef,
      el testTypeTripleDef,
      el testTypeTripleNameDef,
      el testTypeUnionMonomorphicDef,
      el testTypeUnionMonomorphicNameDef,
      el testTypeUnionPolymorphicRecursiveDef,
      el testTypeUnionPolymorphicRecursiveNameDef,
      el testTypeUnitDef,
      el testTypeUnitNameDef]

testGraphType :: String -> TTerm Type -> TBinding Type
testGraphType name = testGraphDefinition name . firstClassType

testTypeLatLonNameDef :: TBinding Name
testTypeLatLonNameDef = testGraphDefinition "testTypeLatLonName" $
  name "LatLon"

testTypeLatLonPolyNameDef :: TBinding Name
testTypeLatLonPolyNameDef = testGraphDefinition "testTypeLatLonPolyName" $
  name "LatLonPoly"

latlonRecordDef :: TBinding (Float -> Float -> Term)
latlonRecordDef = testGraphDefinition "latlonRecord" $
  Phantoms.lambdas ["lat", "lon"] $ record (ref testTypeLatLonNameDef) [
    "lat">: float32Lift $ varPhantom "lat",
    "lon">: float32Lift $ varPhantom "lon"]

testTypeLatLonDef :: TBinding Type
testTypeLatLonDef = testGraphType "testTypeLatLon" $
  T.record (ref testTypeLatLonNameDef) [
    "lat">: T.float32,
    "lon">: T.float32]

testTypeLatLonPolyDef :: TBinding Type
testTypeLatLonPolyDef = testGraphType "testTypeLatLonPoly" $
  T.forAll "a" $ T.record (ref testTypeLatLonPolyNameDef) [
    "lat">: T.var "a",
    "lon">: T.var "a"]

testTypePolymorphicWrapperDef :: TBinding Type
testTypePolymorphicWrapperDef = testGraphType "testTypePolymorphicWrapper" $
  T.forAll "a" $ Core.typeWrap $ Core.wrappedType (ref testTypePolymorphicWrapperNameDef) (T.list $ T.var "a")

testTypePolymorphicWrapperNameDef :: TBinding Name
testTypePolymorphicWrapperNameDef = testGraphDefinition "testTypePolymorphicWrapperName" $
  name "PolymorphicWrapper"

testTypeStringAliasDef :: TBinding Type
testTypeStringAliasDef = testGraphType "testTypeStringAlias" $
  Core.typeWrap $ Core.wrappedType (ref testTypeStringAliasNameDef) T.string

testTypeStringAliasNameDef :: TBinding Name
testTypeStringAliasNameDef = testGraphDefinition "testTypeStringAliasName" $
  name "StringTypeAlias"

testElementArthurDef :: TBinding Binding
testElementArthurDef = testGraphDefinition "testElementArthur" $
  Core.binding
    (name "firstName")
    (ref testDataArthurDef)
    (Phantoms.just $ Core.typeScheme (Phantoms.list []) (Core.typeVariable $ ref testTypePersonNameDef))

testElementFirstNameDef :: TBinding Binding
testElementFirstNameDef = testGraphDefinition "testElementFirstName" $
  Core.binding
    (name "firstName")
    (project (ref testTypePersonNameDef) (name "firstName"))
    (Phantoms.just $ Core.typeScheme (Phantoms.list [])
      (Core.typeFunction $ Core.functionType (Core.typeVariable $ ref testTypePersonNameDef) T.string))

--testGraph :: Graph
--testGraph = elementsToGraph hydraCoreGraph (Just testSchemaGraph) [testElementArthur, testElementFirstName]

testNamespaceDef :: TBinding Namespace
testNamespaceDef = testGraphDefinition "testNamespace" $ Module.namespace $ Phantoms.string "testGraph"

--testSchemaGraph :: Graph
--testSchemaGraph = elementsToGraph hydraCoreGraph (Just hydraCoreGraph) [
--    def testTypeBuddyListAName testTypeBuddyListA,
--    def testTypeBuddyListBName testTypeBuddyListB,
--    def testTypeComparisonName testTypeComparison,
--    def testTypeIntListName testTypeIntList,
--    def testTypeHydraLiteralTypeName testTypeHydraLiteralType,
--    def testTypeHydraTypeName testTypeHydraType,
--    def testTypeLatLonName testTypeLatLon,
--    def testTypeLatLonPolyName testTypeLatLonPoly,
--    def testTypeListName testTypeList,
--    def testTypeNumberName testTypeNumber,
--    def testTypePersonName testTypePerson,
--    def testTypePersonOrSomethingName testTypePersonOrSomething,
--    def testTypeSimpleNumberName testTypeSimpleNumber,
--    def testTypeStringAliasName $ Ann.doc "An alias for the string type" testTypeStringAlias,
--    def testTypeTimestampName testTypeTimestamp,
--    def testTypeUnionMonomorphicName testTypeUnionMonomorphic,
--    def testTypeUnionPolymorphicRecursiveName testTypeUnionPolymorphicRecursive,
--    def testTypeUnitName testTypeUnit]
--  where
--    def = typeElement

testSchemaNamespaceDef :: TBinding Namespace
testSchemaNamespaceDef = testGraphDefinition "testSchemaNamespace" $ Module.namespace $ Phantoms.string "testSchemaGraph"

testDataArthurDef :: TBinding Term
testDataArthurDef = testGraphDefinition "testDataArthur" $
  record (ref testTypePersonNameDef) [
    "firstName">: string "Arthur",
    "lastName">: string "Dent",
    "age">: int32 42]

testTypeBuddyListADef :: TBinding Type
testTypeBuddyListADef = testGraphType "testTypeBuddyListA" $
  T.forAll "a" $ T.record (ref testTypeBuddyListANameDef) [
    "head">: T.var "a",
    "tail">: T.optional $
      T.apply (Core.typeVariable $ ref testTypeBuddyListBNameDef) (T.var "a")]

testTypeBuddyListANameDef :: TBinding Name
testTypeBuddyListANameDef = testGraphDefinition "testTypeBuddyListAName" $
  name "BuddyListA"

testTypeBuddyListBDef :: TBinding Type
testTypeBuddyListBDef = testGraphType "testTypeBuddyListB" $
  T.forAll "a" $ T.record (ref testTypeBuddyListBNameDef) [
    "head">: T.var "a",
    "tail">: T.optional $
      T.apply (Core.typeVariable $ ref testTypeBuddyListANameDef) (T.var "a")]

testTypeBuddyListBNameDef :: TBinding Name
testTypeBuddyListBNameDef = testGraphDefinition "testTypeBuddyListBName" $
  name "BuddyListB"

testTypeComparisonDef :: TBinding Type
testTypeComparisonDef = testGraphType "testTypeComparison" $
  T.union (ref testTypeComparisonNameDef) [
    "lessThan">: T.unit,
    "equalTo">: T.unit,
    "greaterThan">: T.unit]

testTypeComparisonNameDef :: TBinding Name
testTypeComparisonNameDef = testGraphDefinition "testTypeComparisonName" $
  name "Comparison"

testTypeEitherDef :: TBinding Type
testTypeEitherDef = testGraphType "testTypeEither" $
  T.forAlls ["a", "b"] $ T.union (ref testTypeEitherNameDef) [
    "left">: T.var "a",
    "right">: T.var "b"]

testTypeEitherNameDef :: TBinding Name
testTypeEitherNameDef = testGraphDefinition "testTypeEitherName" $
  name "Either"

testTypeIntListDef :: TBinding Type
testTypeIntListDef = testGraphType "testTypeIntList" $
  T.record (ref testTypeIntListNameDef) [
    "head">: T.int32,
    "tail">: T.optional $ Core.typeVariable (ref testTypeIntListNameDef)]

testTypeIntListNameDef :: TBinding Name
testTypeIntListNameDef = testGraphDefinition "testTypeIntListName" $
  name "IntList"

testTypeHydraLiteralTypeDef :: TBinding Type
testTypeHydraLiteralTypeDef = testGraphType "testTypeHydraLiteralType" $
  T.union (ref testTypeHydraLiteralTypeNameDef) [
    "boolean">: T.boolean,
    "string">: T.string]

testTypeHydraLiteralTypeNameDef :: TBinding Name
testTypeHydraLiteralTypeNameDef = testGraphDefinition "testTypeHydraLiteralTypeName" $
  name "HydraLiteralType"

testTypeHydraTypeDef :: TBinding Type
testTypeHydraTypeDef = testGraphType "testTypeHydraType" $
  T.union (ref testTypeHydraTypeNameDef) [
    "literal">: Core.typeVariable $ ref testTypeHydraLiteralTypeNameDef,
    "list">: Core.typeVariable $ ref testTypeHydraTypeNameDef]

testTypeHydraTypeNameDef :: TBinding Name
testTypeHydraTypeNameDef = testGraphDefinition "testTypeHydraTypeName" $
  name "HydraType"

testTypeListDef :: TBinding Type
testTypeListDef = testGraphType "testTypeList" $
  T.forAll "a" $ T.record (ref testTypeListNameDef) [
    "head">: T.var "a",
    "tail">: T.optional $
      T.apply (Core.typeVariable $ ref testTypeListNameDef) (T.var "a")]

testTypeListNameDef :: TBinding Name
testTypeListNameDef = testGraphDefinition "testTypeListName" $
  name "List"

testTypeNumberDef :: TBinding Type
testTypeNumberDef = testGraphType "testTypeNumber" $
  T.union (ref testTypeNumberNameDef) [
    "int">: T.int32,
    "float">: T.float32]

testTypeNumberNameDef :: TBinding Name
testTypeNumberNameDef = testGraphDefinition "testTypeNumberName" $
  name "Number"

testTypePersonDef :: TBinding Type
testTypePersonDef = testGraphType "testTypePerson" $
  T.record (ref testTypePersonNameDef) [
    "firstName">: T.string,
    "lastName">: T.string,
    "age">: T.int32]

testTypePersonNameDef :: TBinding Name
testTypePersonNameDef = testGraphDefinition "testTypePersonName" $
  name "Person"

testTypePersonOrSomethingDef :: TBinding Type
testTypePersonOrSomethingDef = testGraphType "testTypePersonOrSomething" $
  T.forAll "a" $ T.union (ref testTypePersonOrSomethingNameDef) [
    "person">: Core.typeVariable $ ref testTypePersonNameDef,
    "other">: T.var "a"]

testTypePersonOrSomethingNameDef :: TBinding Name
testTypePersonOrSomethingNameDef = testGraphDefinition "testTypePersonOrSomethingName" $
  name "PersonOrSomething"

testTypeSimpleNumberDef :: TBinding Type
testTypeSimpleNumberDef = testGraphType "testTypeSimpleNumber" $
  T.union (ref testTypeSimpleNumberNameDef) [
    "int">: T.int32,
    "float">: T.float32]

testTypeSimpleNumberNameDef :: TBinding Name
testTypeSimpleNumberNameDef = testGraphDefinition "testTypeSimpleNumberName" $
  name "SimpleNumber"

testTypeSymmetricTripleDef :: TBinding Type
testTypeSymmetricTripleDef = testGraphType "testTypeSymmetricTriple" $
  T.forAlls ["v", "e"] $ T.wrap (ref testTypeSymmetricTripleNameDef) $
    T.applys (Core.typeVariable $ ref testTypeTripleNameDef) [T.var "v", T.var "e", T.var "v"]

testTypeSymmetricTripleNameDef :: TBinding Name
testTypeSymmetricTripleNameDef = testGraphDefinition "testTypeSymmetricTripleName" $
  name "SymmetricTriple"

testTypeTimestampDef :: TBinding Type
testTypeTimestampDef = testGraphType "testTypeTimestamp" $
  T.union (ref testTypeTimestampNameDef) [
    "unixTimeMillis">: T.uint64,
    "date">: T.string]

testTypeTimestampNameDef :: TBinding Name
testTypeTimestampNameDef = testGraphDefinition "testTypeTimestampName" $
  name "Timestamp"

testTypeTripleDef :: TBinding Type
testTypeTripleDef = testGraphType "testTypeTriple" $
  T.forAlls ["a", "b", "c"] $ T.record (ref testTypeTripleNameDef) [
    "first">: T.var "a",
    "second">: T.var "b",
    "third">: T.var "c"]

testTypeTripleNameDef :: TBinding Name
testTypeTripleNameDef = testGraphDefinition "testTypeTripleName" $
  name "Triple"

testTypeUnionMonomorphicDef :: TBinding Type
testTypeUnionMonomorphicDef = testGraphType "testTypeUnionMonomorphic" $
  T.union (ref testTypeUnionMonomorphicNameDef) [
    "bool">: T.boolean,
    "string">: T.string,
    "unit">: T.unit]

testTypeUnionMonomorphicNameDef :: TBinding Name
testTypeUnionMonomorphicNameDef = testGraphDefinition "testTypeUnionMonomorphicName" $
  name "UnionMonomorphic"

testTypeUnionPolymorphicRecursiveDef :: TBinding Type
testTypeUnionPolymorphicRecursiveDef = testGraphType "testTypeUnionPolymorphicRecursive" $
  T.forAll "a" $ T.union (ref testTypeUnionPolymorphicRecursiveNameDef) [
    "bool">: T.boolean,
    "value">: T.var "a",
    "other">: T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) (T.var "a")]

testTypeUnionPolymorphicRecursiveNameDef :: TBinding Name
testTypeUnionPolymorphicRecursiveNameDef = testGraphDefinition "testTypeUnionPolymorphicRecursiveName" $
  name "UnionPolymorphicRecursive"

testTypeUnitDef :: TBinding Type
testTypeUnitDef = testGraphType "testTypeUnit" $
  T.record (ref testTypeUnitNameDef) []

testTypeUnitNameDef :: TBinding Name
testTypeUnitNameDef = testGraphDefinition "testTypeUnitName" $
  name "Unit"

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


testGraphDefinition :: String -> TTerm a -> TElement a
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
      el testTypeStringAliasDef,
      el testTypeStringAliasNameDef,
      el testTypePolymorphicWrapperDef,
      el testTypePolymorphicWrapperNameDef,
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
      el testTypeTimestampDef,
      el testTypeTimestampNameDef,
      el testTypeUnionMonomorphicDef,
      el testTypeUnionMonomorphicNameDef,
      el testTypeUnionPolymorphicRecursiveDef,
      el testTypeUnionPolymorphicRecursiveNameDef,
      el testTypeUnitDef,
      el testTypeUnitNameDef]

testGraphType :: String -> TTerm Type -> TElement Type
testGraphType name = testGraphDefinition name . firstClassType

testTypeLatLonNameDef :: TElement Name
testTypeLatLonNameDef = testGraphDefinition "testTypeLatLonName" $
  name "LatLon"

testTypeLatLonPolyNameDef :: TElement Name
testTypeLatLonPolyNameDef = testGraphDefinition "testTypeLatLonPolyName" $
  name "LatLonPoly"

latlonRecordDef :: TElement (Float -> Float -> Term)
latlonRecordDef = testGraphDefinition "latlonRecord" $
  Phantoms.lambdas ["lat", "lon"] $ record (ref testTypeLatLonNameDef) [
    "lat">: float32Lift $ varPhantom "lat",
    "lon">: float32Lift $ varPhantom "lon"]

testTypeLatLonDef :: TElement Type
testTypeLatLonDef = testGraphType "testTypeLatLon" $
  T.record (ref testTypeLatLonNameDef) [
    "lat">: T.float32,
    "lon">: T.float32]

testTypeLatLonPolyDef :: TElement Type
testTypeLatLonPolyDef = testGraphType "testTypeLatLonPoly" $
  T.forAll "a" $ T.record (ref testTypeLatLonPolyNameDef) [
    "lat">: T.var "a",
    "lon">: T.var "a"]

testTypeStringAliasDef :: TElement Type
testTypeStringAliasDef = testGraphType "testTypeStringAlias" $
  Core.typeWrap $ Core.wrappedType (ref testTypeStringAliasNameDef) T.string

testTypeStringAliasNameDef :: TElement Name
testTypeStringAliasNameDef = testGraphDefinition "testTypeStringAliasName" $
  name "StringTypeAlias"

testTypePolymorphicWrapperDef :: TElement Type
testTypePolymorphicWrapperDef = testGraphType "testTypePolymorphicWrapper" $
  T.forAll "a" $ Core.typeWrap $ Core.wrappedType (ref testTypePolymorphicWrapperNameDef) (T.list $ T.var "a")

testTypePolymorphicWrapperNameDef :: TElement Name
testTypePolymorphicWrapperNameDef = testGraphDefinition "testTypePolymorphicWrapperName" $
  name "PolymorphicWrapper"

testElementArthurDef :: TElement Element
testElementArthurDef = testGraphDefinition "testElementArthur" $
  Graph.element
    (name "firstName")
    (ref testDataArthurDef)
    (Phantoms.just $ Core.typeScheme (Phantoms.list []) (Core.typeVariable $ ref testTypePersonNameDef))

testElementFirstNameDef :: TElement Element
testElementFirstNameDef = testGraphDefinition "testElementFirstName" $
  Graph.element
    (name "firstName")
    (project (ref testTypePersonNameDef) (name "firstName"))
    (Phantoms.just $ Core.typeScheme (Phantoms.list [])
      (Core.typeFunction $ Core.functionType (Core.typeVariable $ ref testTypePersonNameDef) T.string))

--testGraph :: Graph
--testGraph = elementsToGraph hydraCoreGraph (Just testSchemaGraph) [testElementArthur, testElementFirstName]

testNamespaceDef :: TElement Namespace
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

testSchemaNamespaceDef :: TElement Namespace
testSchemaNamespaceDef = testGraphDefinition "testSchemaNamespace" $ Module.namespace $ Phantoms.string "testSchemaGraph"

testDataArthurDef :: TElement Term
testDataArthurDef = testGraphDefinition "testDataArthur" $
  record (ref testTypePersonNameDef) [
    "firstName">: string "Arthur",
    "lastName">: string "Dent",
    "age">: int32 42]

testTypeBuddyListADef :: TElement Type
testTypeBuddyListADef = testGraphType "testTypeBuddyListA" $
  T.forAll "a" $ T.record (ref testTypeBuddyListANameDef) [
    "head">: T.var "a",
    "tail">: T.optional $
      T.apply (Core.typeVariable $ ref testTypeBuddyListBNameDef) (T.var "a")]

testTypeBuddyListANameDef :: TElement Name
testTypeBuddyListANameDef = testGraphDefinition "testTypeBuddyListAName" $
  name "BuddyListA"

testTypeBuddyListBDef :: TElement Type
testTypeBuddyListBDef = testGraphType "testTypeBuddyListB" $
  T.forAll "a" $ T.record (ref testTypeBuddyListBNameDef) [
    "head">: T.var "a",
    "tail">: T.optional $
      T.apply (Core.typeVariable $ ref testTypeBuddyListANameDef) (T.var "a")]

testTypeBuddyListBNameDef :: TElement Name
testTypeBuddyListBNameDef = testGraphDefinition "testTypeBuddyListBName" $
  name "BuddyListB"

testTypeComparisonDef :: TElement Type
testTypeComparisonDef = testGraphType "testTypeComparison" $
  T.union (ref testTypeComparisonNameDef) [
    "lessThan">: T.unit,
    "equalTo">: T.unit,
    "greaterThan">: T.unit]

testTypeComparisonNameDef :: TElement Name
testTypeComparisonNameDef = testGraphDefinition "testTypeComparisonName" $
  name "Comparison"

testTypeIntListDef :: TElement Type
testTypeIntListDef = testGraphType "testTypeIntList" $
  T.record (ref testTypeIntListNameDef) [
    "head">: T.int32,
    "tail">: T.optional $ Core.typeVariable (ref testTypeIntListNameDef)]

testTypeIntListNameDef :: TElement Name
testTypeIntListNameDef = testGraphDefinition "testTypeIntListName" $
  name "IntList"

testTypeHydraLiteralTypeDef :: TElement Type
testTypeHydraLiteralTypeDef = testGraphType "testTypeHydraLiteralType" $
  T.union (ref testTypeHydraLiteralTypeNameDef) [
    "boolean">: T.boolean,
    "string">: T.string]

testTypeHydraLiteralTypeNameDef :: TElement Name
testTypeHydraLiteralTypeNameDef = testGraphDefinition "testTypeHydraLiteralTypeName" $
  name "HydraLiteralType"

testTypeHydraTypeDef :: TElement Type
testTypeHydraTypeDef = testGraphType "testTypeHydraType" $
  T.union (ref testTypeHydraTypeNameDef) [
    "literal">: Core.typeVariable $ ref testTypeHydraLiteralTypeNameDef,
    "list">: Core.typeVariable $ ref testTypeHydraTypeNameDef]

testTypeHydraTypeNameDef :: TElement Name
testTypeHydraTypeNameDef = testGraphDefinition "testTypeHydraTypeName" $
  name "HydraType"

testTypeListDef :: TElement Type
testTypeListDef = testGraphType "testTypeList" $
  T.forAll "a" $ T.record (ref testTypeListNameDef) [
    "head">: T.var "a",
    "tail">: T.optional $
      T.apply (Core.typeVariable $ ref testTypeListNameDef) (T.var "a")]

testTypeListNameDef :: TElement Name
testTypeListNameDef = testGraphDefinition "testTypeListName" $
  name "List"

testTypeNumberDef :: TElement Type
testTypeNumberDef = testGraphType "testTypeNumber" $
  T.union (ref testTypeNumberNameDef) [
    "int">: T.int32,
    "float">: T.float32]

testTypeNumberNameDef :: TElement Name
testTypeNumberNameDef = testGraphDefinition "testTypeNumberName" $
  name "Number"

testTypePersonDef :: TElement Type
testTypePersonDef = testGraphType "testTypePerson" $
  T.record (ref testTypePersonNameDef) [
    "firstName">: T.string,
    "lastName">: T.string,
    "age">: T.int32]

testTypePersonNameDef :: TElement Name
testTypePersonNameDef = testGraphDefinition "testTypePersonName" $
  name "Person"

testTypePersonOrSomethingDef :: TElement Type
testTypePersonOrSomethingDef = testGraphType "testTypePersonOrSomething" $
  T.forAll "a" $ T.union (ref testTypePersonOrSomethingNameDef) [
    "person">: Core.typeVariable $ ref testTypePersonNameDef,
    "other">: T.var "a"]

testTypePersonOrSomethingNameDef :: TElement Name
testTypePersonOrSomethingNameDef = testGraphDefinition "testTypePersonOrSomethingName" $
  name "PersonOrSomething"

testTypeSimpleNumberDef :: TElement Type
testTypeSimpleNumberDef = testGraphType "testTypeSimpleNumber" $
  T.union (ref testTypeSimpleNumberNameDef) [
    "int">: T.int32,
    "float">: T.float32]

testTypeSimpleNumberNameDef :: TElement Name
testTypeSimpleNumberNameDef = testGraphDefinition "testTypeSimpleNumberName" $
  name "SimpleNumber"

testTypeTimestampDef :: TElement Type
testTypeTimestampDef = testGraphType "testTypeTimestamp" $
  T.union (ref testTypeTimestampNameDef) [
    "unixTimeMillis">: T.uint64,
    "date">: T.string]

testTypeTimestampNameDef :: TElement Name
testTypeTimestampNameDef = testGraphDefinition "testTypeTimestampName" $
  name "Timestamp"

testTypeUnionMonomorphicDef :: TElement Type
testTypeUnionMonomorphicDef = testGraphType "testTypeUnionMonomorphic" $
  T.union (ref testTypeUnionMonomorphicNameDef) [
    "bool">: T.boolean,
    "string">: T.string,
    "unit">: T.unit]

testTypeUnionMonomorphicNameDef :: TElement Name
testTypeUnionMonomorphicNameDef = testGraphDefinition "testTypeUnionMonomorphicName" $
  name "UnionMonomorphic"

testTypeUnionPolymorphicRecursiveDef :: TElement Type
testTypeUnionPolymorphicRecursiveDef = testGraphType "testTypeUnionPolymorphicRecursive" $
  T.forAll "a" $ T.union (ref testTypeUnionPolymorphicRecursiveNameDef) [
    "bool">: T.boolean,
    "value">: T.var "a",
    "other">: T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) (T.var "a")]

testTypeUnionPolymorphicRecursiveNameDef :: TElement Name
testTypeUnionPolymorphicRecursiveNameDef = testGraphDefinition "testTypeUnionPolymorphicRecursiveName" $
  name "UnionPolymorphicRecursive"

testTypeUnitDef :: TElement Type
testTypeUnitDef = testGraphType "testTypeUnit" $
  T.record (ref testTypeUnitNameDef) []

testTypeUnitNameDef :: TElement Name
testTypeUnitNameDef = testGraphDefinition "testTypeUnitName" $
  name "Unit"

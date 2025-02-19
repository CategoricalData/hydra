module Hydra.Sources.Tier3.Test.TestGraph where

import qualified Hydra.Dsl.Base          as Base
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Module        as Module
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Io        as Io
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Types         as Types
import           Hydra.Sources.Tier2.All
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
testGraphModule = Module (Namespace "hydra.test.testGraph") elements [] [hydraGraphModule, hydraModuleModule] $
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
      el testTypeFoobarValueDef,
      el testTypeFoobarValueNameDef,
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
      el testTypeUnionPolymorphicRecursiveNameDef]

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
  Base.functionN [tFloat32, tFloat32, termT] $
  lambdas ["lat", "lon"] $ record (ref testTypeLatLonNameDef) [
    field "lat" $ float32Term $ variable "lat",
    field "lon" $ float32Term $ variable "lon"]

testTypeLatLonDef :: TElement Type
testTypeLatLonDef = testGraphType "testTypeLatLon" $
  T.record (ref testTypeLatLonNameDef) [
    T.field "lat" T.float32,
    T.field "lon" T.float32]

testTypeLatLonPolyDef :: TElement Type
testTypeLatLonPolyDef = testGraphType "testTypeLatLonPoly" $
  T.lambda "a" $ T.record (ref testTypeLatLonPolyNameDef) [
    T.field "lat" $ T.var "a",
    T.field "lon" $ T.var "a"]

testTypeStringAliasDef :: TElement Type
testTypeStringAliasDef = testGraphType "testTypeStringAlias" $
  Core.typeWrap $ Core.wrappedType (ref testTypeStringAliasNameDef) T.string

testTypeStringAliasNameDef :: TElement Name
testTypeStringAliasNameDef = testGraphDefinition "testTypeStringAliasName" $
  name "StringTypeAlias"

testElementArthurDef :: TElement Element
testElementArthurDef = testGraphDefinition "testElementArthur" $
  Graph.element
    (name "firstName")
    (ref testDataArthurDef)

testElementFirstNameDef :: TElement Element
testElementFirstNameDef = testGraphDefinition "testElementFirstName" $
  Graph.element
    (name "firstName")
    (project (ref testTypePersonNameDef) (name "firstName"))

--testGraph :: Graph
--testGraph = elementsToGraph hydraCoreGraph (Just testSchemaGraph) [testElementArthur, testElementFirstName]

testNamespaceDef :: TElement Namespace
testNamespaceDef = testGraphDefinition "testNamespace" $
  Module.namespace $ Base.string "testGraph"

--testSchemaGraph :: Graph
--testSchemaGraph = elementsToGraph hydraCoreGraph (Just hydraCoreGraph) [
--    def testTypeBuddyListAName testTypeBuddyListA,
--    def testTypeBuddyListBName testTypeBuddyListB,
--    def testTypeComparisonName testTypeComparison,
--    def testTypeFoobarValueName testTypeFoobarValue,
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
--    def testTypeUnionPolymorphicRecursiveName testTypeUnionPolymorphicRecursive]
--  where
--    def = typeElement

testSchemaNamespaceDef :: TElement Namespace
testSchemaNamespaceDef = testGraphDefinition "testSchemaNamespace" $
  Module.namespace $ Base.string "testSchemaGraph"

testDataArthurDef :: TElement Term
testDataArthurDef = testGraphDefinition "testDataArthur" $
  record (ref testTypePersonNameDef) [
    field "firstName" $ string "Arthur",
    field "lastName" $ string "Dent",
    field "age" $ int32 42]

testTypeBuddyListADef :: TElement Type
testTypeBuddyListADef = testGraphType "testTypeBuddyListA" $
  T.lambda "a" $ T.record (ref testTypeBuddyListANameDef) [
    T.field "head" $ T.var "a",
    T.field "tail" $ T.optional $
      T.apply (Core.typeVariable $ ref testTypeBuddyListBNameDef) (T.var "a")]

testTypeBuddyListANameDef :: TElement Name
testTypeBuddyListANameDef = testGraphDefinition "testTypeBuddyListAName" $
  name "BuddyListA"

testTypeBuddyListBDef :: TElement Type
testTypeBuddyListBDef = testGraphType "testTypeBuddyListB" $
  T.lambda "a" $ T.record (ref testTypeBuddyListBNameDef) [
    T.field "head" $ T.var "a",
    T.field "tail" $ T.optional $
      T.apply (Core.typeVariable $ ref testTypeBuddyListANameDef) (T.var "a")]

testTypeBuddyListBNameDef :: TElement Name
testTypeBuddyListBNameDef = testGraphDefinition "testTypeBuddyListBName" $
  name "BuddyListB"

testTypeComparisonDef :: TElement Type
testTypeComparisonDef = testGraphType "testTypeComparison" $
  T.union (ref testTypeComparisonNameDef) [
    T.field "lessThan" T.unit,
    T.field "equalTo" T.unit,
    T.field "greaterThan" T.unit]

testTypeComparisonNameDef :: TElement Name
testTypeComparisonNameDef = testGraphDefinition "testTypeComparisonName" $
  name "Comparison"

---- TODO: remove
testTypeFoobarValueDef :: TElement Type
testTypeFoobarValueDef = testGraphType "testTypeFoobarValue" $
  T.union (ref testTypeFoobarValueNameDef) [
    T.field "bool" T.boolean,
    T.field "string" T.string,
    T.field "unit" T.unit]
testTypeFoobarValueNameDef :: TElement Name
testTypeFoobarValueNameDef = testGraphDefinition "testTypeFoobarValueName" $
  name "FoobarValue"

testTypeIntListDef :: TElement Type
testTypeIntListDef = testGraphType "testTypeIntList" $
  T.record (ref testTypeIntListNameDef) [
    T.field "head" T.int32,
    T.field "tail" $ T.optional $ Core.typeVariable (ref testTypeIntListNameDef)]

testTypeIntListNameDef :: TElement Name
testTypeIntListNameDef = testGraphDefinition "testTypeIntListName" $
  name "IntList"

testTypeHydraLiteralTypeDef :: TElement Type
testTypeHydraLiteralTypeDef = testGraphType "testTypeHydraLiteralType" $
  T.union (ref testTypeHydraLiteralTypeNameDef) [
    T.field "boolean" T.boolean,
    T.field "string" T.string]

testTypeHydraLiteralTypeNameDef :: TElement Name
testTypeHydraLiteralTypeNameDef = testGraphDefinition "testTypeHydraLiteralTypeName" $
  name "HydraLiteralType"

testTypeHydraTypeDef :: TElement Type
testTypeHydraTypeDef = testGraphType "testTypeHydraType" $
  T.union (ref testTypeHydraTypeNameDef) [
    T.field "literal" $ Core.typeVariable $ ref testTypeHydraLiteralTypeNameDef,
    T.field "list" $ Core.typeVariable $ ref testTypeHydraTypeNameDef]

testTypeHydraTypeNameDef :: TElement Name
testTypeHydraTypeNameDef = testGraphDefinition "testTypeHydraTypeName" $
  name "HydraType"

testTypeListDef :: TElement Type
testTypeListDef = testGraphType "testTypeList" $
  T.lambda "a" $ T.record (ref testTypeListNameDef) [
    T.field "head" $ T.var "a",
    T.field "tail" $ T.optional $
      T.apply (Core.typeVariable $ ref testTypeListNameDef) (T.var "a")]

testTypeListNameDef :: TElement Name
testTypeListNameDef = testGraphDefinition "testTypeListName" $
  name "List"

testTypeNumberDef :: TElement Type
testTypeNumberDef = testGraphType "testTypeNumber" $
  T.union (ref testTypeNumberNameDef) [
    T.field "int" T.int32,
    T.field "float" T.float32]

testTypeNumberNameDef :: TElement Name
testTypeNumberNameDef = testGraphDefinition "testTypeNumberName" $
  name "Number"

testTypePersonDef :: TElement Type
testTypePersonDef = testGraphType "testTypePerson" $
  T.record (ref testTypePersonNameDef) [
    T.field "firstName" T.string,
    T.field "lastName" T.string,
    T.field "age" T.int32]

testTypePersonNameDef :: TElement Name
testTypePersonNameDef = testGraphDefinition "testTypePersonName" $
  name "Person"

testTypePersonOrSomethingDef :: TElement Type
testTypePersonOrSomethingDef = testGraphType "testTypePersonOrSomething" $
  T.lambda "a" $ T.union (ref testTypePersonOrSomethingNameDef) [
    T.field "person" $ Core.typeVariable $ ref testTypePersonNameDef,
    T.field "other" $ T.var "a"]

testTypePersonOrSomethingNameDef :: TElement Name
testTypePersonOrSomethingNameDef = testGraphDefinition "testTypePersonOrSomethingName" $
  name "PersonOrSomething"

testTypeSimpleNumberDef :: TElement Type
testTypeSimpleNumberDef = testGraphType "testTypeSimpleNumber" $
  T.union (ref testTypeSimpleNumberNameDef) [
    T.field "int" T.int32,
    T.field "float" T.float32]

testTypeSimpleNumberNameDef :: TElement Name
testTypeSimpleNumberNameDef = testGraphDefinition "testTypeSimpleNumberName" $
  name "SimpleNumber"

testTypeTimestampDef :: TElement Type
testTypeTimestampDef = testGraphType "testTypeTimestamp" $
  T.union (ref testTypeTimestampNameDef) [
    T.field "unixTimeMillis" T.uint64,
    T.field "date" T.string]

testTypeTimestampNameDef :: TElement Name
testTypeTimestampNameDef = testGraphDefinition "testTypeTimestampName" $
  name "Timestamp"

testTypeUnionMonomorphicDef :: TElement Type
testTypeUnionMonomorphicDef = testGraphType "testTypeUnionMonomorphic" $
  T.union (ref testTypeUnionMonomorphicNameDef) [
    T.field "bool" T.boolean,
    T.field "string" T.string,
    T.field "unit" T.unit]

testTypeUnionMonomorphicNameDef :: TElement Name
testTypeUnionMonomorphicNameDef = testGraphDefinition "testTypeUnionMonomorphicName" $
  name "UnionMonomorphic"

testTypeUnionPolymorphicRecursiveDef :: TElement Type
testTypeUnionPolymorphicRecursiveDef = testGraphType "testTypeUnionPolymorphicRecursive" $
  T.lambda "a" $ T.union (ref testTypeUnionPolymorphicRecursiveNameDef) [
    T.field "bool" T.boolean,
    T.field "value" $ T.var "a",
    T.field "other" $ T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) (T.var "a")]

testTypeUnionPolymorphicRecursiveNameDef :: TElement Name
testTypeUnionPolymorphicRecursiveNameDef = testGraphDefinition "testTypeUnionPolymorphicRecursiveName" $
  name "UnionPolymorphicRecursive"

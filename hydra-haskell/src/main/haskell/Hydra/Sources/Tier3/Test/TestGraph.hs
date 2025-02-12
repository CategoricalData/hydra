module Hydra.Sources.Tier3.Test.TestGraph where

import           Hydra.Dsl.Base          as Base
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


testGraphDefinition :: String -> TTerm a -> TElement a
testGraphDefinition = definitionInModule testGraphModule

testGraphModule :: Module
testGraphModule = Module (Namespace "hydra/test/testGraph") elements [] [hydraGraphModule, hydraModuleModule] $
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
--      el testGraphDef,
      el testNamespaceDef,
--      el testSchemaGraphDef,
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
  Core.name $ Name "LatLon"

testTypeLatLonPolyNameDef :: TElement Name
testTypeLatLonPolyNameDef = testGraphDefinition "testTypeLatLonPolyName" $
  Core.name $ Name "LatLonPoly"

latlonRecordDef :: TElement (Float -> Float -> Term)
latlonRecordDef = testGraphDefinition "latlonRecord" $
  functionN [tFloat32, tFloat32, termT] $
  lambdas ["lat", "lon"] $ Core.termRecord $ Core.record (ref testTypeLatLonNameDef) $ list [
    Core.field (Core.name $ Name "lat") $ Core.float32Term $ Core.var "lat",
    Core.field (Core.name $ Name "lon") $ Core.float32Term $ Core.var "lon"]

testTypeLatLonDef :: TElement Type
testTypeLatLonDef = testGraphType "testTypeLatLon" $
  Core.typeRecord $ Core.rowType (ref testTypeLatLonNameDef) $ list [
    Core.fieldType (Core.name $ Name "lat") $ Core.float32Type,
    Core.fieldType (Core.name $ Name "lon") $ Core.float32Type]

testTypeLatLonPolyDef :: TElement Type
testTypeLatLonPolyDef = testGraphType "testTypeLatLonPoly" $
  Core.typeLambda $ Core.lambdaType a $
    Core.typeRecord $ Core.rowType (ref testTypeLatLonPolyNameDef) $ list [
      Core.fieldType (Core.name $ Name "lat") $ Core.typeVariable a,
      Core.fieldType (Core.name $ Name "lon") $ Core.typeVariable a]
  where
    a = Core.name $ Name "a"

testTypeStringAliasDef :: TElement Type
testTypeStringAliasDef = testGraphType "testTypeStringAlias" $
  Core.typeWrap $ Core.wrappedType (ref testTypeStringAliasNameDef) Core.stringType

testTypeStringAliasNameDef :: TElement Name
testTypeStringAliasNameDef = testGraphDefinition "testTypeStringAliasName" $
  Core.name $ Name "StringTypeAlias"

testElementArthurDef :: TElement Element
testElementArthurDef = testGraphDefinition "testElementArthur" $
  Graph.element
    (Core.name $ Name "firstName")
    (ref testDataArthurDef)

testElementFirstNameDef :: TElement Element
testElementFirstNameDef = testGraphDefinition "testElementFirstName" $
  Graph.element
    (Core.name $ Name "firstName")
    (Core.project (ref testTypePersonNameDef) (Core.name $ Name "firstName"))

--testGraph :: Graph
--testGraph = elementsToGraph hydraCoreGraph (Just testSchemaGraph) [testElementArthur, testElementFirstName]

testNamespaceDef :: TElement Namespace
testNamespaceDef = testGraphDefinition "testNamespace" $
  Module.namespace $ string "testGraph"

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
  Module.namespace $ string "testSchemaGraph"

testDataArthurDef :: TElement Term
testDataArthurDef = testGraphDefinition "testDataArthur" $
  Core.termRecord $ Core.record (ref testTypePersonNameDef) $ list [
    Core.field (Core.name $ Name "firstName") $ Core.stringTerm $ string "Arthur",
    Core.field (Core.name $ Name "lastName") $ Core.stringTerm $ string "Dent",
    Core.field (Core.name $ Name "age") $ Core.int32Term $ int32 42]

testTypeBuddyListADef :: TElement Type
testTypeBuddyListADef = testGraphType "testTypeBuddyListA" $
  Core.typeLambda $ Core.lambdaType a $ Core.typeRecord $ Core.rowType (ref testTypeBuddyListANameDef) $ list [
    Core.fieldType (Core.name $ Name "head") $ Core.typeVariable a,
    Core.fieldType (Core.name $ Name "tail") $ Core.typeOptional $
      Core.typeApply (Core.typeVariable $ ref testTypeBuddyListBNameDef) (Core.typeVariable a)]
  where
    a = Core.name $ Name "a"

testTypeBuddyListANameDef :: TElement Name
testTypeBuddyListANameDef = testGraphDefinition "testTypeBuddyListAName" $
  Core.name $ Name "BuddyListA"

testTypeBuddyListBDef :: TElement Type
testTypeBuddyListBDef = testGraphType "testTypeBuddyListB" $
  Core.typeLambda $ Core.lambdaType a $ Core.typeRecord $ Core.rowType (ref testTypeBuddyListBNameDef) $ list [
    Core.fieldType (Core.name $ Name "head") $ Core.typeVariable a,
    Core.fieldType (Core.name $ Name "tail") $ Core.typeOptional $
      Core.typeApply (Core.typeVariable $ ref testTypeBuddyListANameDef) (Core.typeVariable a)]
  where
    a = Core.name $ Name "a"

testTypeBuddyListBNameDef :: TElement Name
testTypeBuddyListBNameDef = testGraphDefinition "testTypeBuddyListBName" $
  Core.name $ Name "BuddyListB"

testTypeComparisonDef :: TElement Type
testTypeComparisonDef = testGraphType "testTypeComparison" $
  Core.typeUnion $ Core.rowType (ref testTypeComparisonNameDef) $ list [
    Core.fieldType (Core.name $ Name "lessThan") Core.unitType,
    Core.fieldType (Core.name $ Name "equalTo") Core.unitType,
    Core.fieldType (Core.name $ Name "greaterThan") Core.unitType]

testTypeComparisonNameDef :: TElement Name
testTypeComparisonNameDef = testGraphDefinition "testTypeComparisonName" $
  Core.name $ Name "Comparison"

---- TODO: remove
testTypeFoobarValueDef :: TElement Type
testTypeFoobarValueDef = testGraphType "testTypeFoobarValue" $
  Core.typeUnion $ Core.rowType (ref testTypeFoobarValueNameDef) $ list [
   Core.fieldType (Core.name $ Name "bool") Core.booleanType,
   Core.fieldType (Core.name $ Name "string") Core.stringType,
   Core.fieldType (Core.name $ Name "unit") Core.unitType]
testTypeFoobarValueNameDef :: TElement Name
testTypeFoobarValueNameDef = testGraphDefinition "testTypeFoobarValueName" $
  Core.name $ Name "FoobarValue"

testTypeIntListDef :: TElement Type
testTypeIntListDef = testGraphType "testTypeIntList" $
  Core.typeRecord $ Core.rowType (ref testTypeIntListNameDef) $ list [
    Core.fieldType (Core.name $ Name "head") Core.int32Type,
    Core.fieldType (Core.name $ Name "tail") $ Core.typeOptional $ Core.typeVariable (ref testTypeIntListNameDef)]
    
testTypeIntListNameDef :: TElement Name
testTypeIntListNameDef = testGraphDefinition "testTypeIntListName" $
  Core.name $ Name "IntList"

testTypeHydraLiteralTypeDef :: TElement Type
testTypeHydraLiteralTypeDef = testGraphType "testTypeHydraLiteralType" $
  Core.typeUnion $ Core.rowType (ref testTypeHydraLiteralTypeNameDef) $ list [
    Core.fieldType (Core.name $ Name "boolean") Core.booleanType,
    Core.fieldType (Core.name $ Name "string") Core.stringType]

testTypeHydraLiteralTypeNameDef :: TElement Name
testTypeHydraLiteralTypeNameDef = testGraphDefinition "testTypeHydraLiteralTypeName" $
  Core.name $ Name "HydraLiteralType"

testTypeHydraTypeDef :: TElement Type
testTypeHydraTypeDef = testGraphType "testTypeHydraType" $
  Core.typeUnion $ Core.rowType (ref testTypeHydraTypeNameDef) $ list [
    Core.fieldType (Core.name $ Name "literal") $ Core.typeVariable $ ref testTypeHydraLiteralTypeNameDef,
    Core.fieldType (Core.name $ Name "list") $ Core.typeVariable $ ref testTypeHydraTypeNameDef]

testTypeHydraTypeNameDef :: TElement Name
testTypeHydraTypeNameDef = testGraphDefinition "testTypeHydraTypeName" $
  Core.name $ Name "HydraType"

--testTypeList :: Type
--testTypeList = Types.lambda "a" $ TypeRecord $ RowType testTypeListName [
--  Types.field "head" $ Types.var "a",
--  Types.field "tail" $ Types.optional $ Types.apply (TypeVariable testTypeListName) (Types.var "a")]

testTypeListDef :: TElement Type
testTypeListDef = testGraphType "testTypeList" $
  Core.typeLambda $ Core.lambdaType a $ Core.typeRecord $ Core.rowType (ref testTypeListNameDef) $ list [
    Core.fieldType (Core.name $ Name "head") $ Core.typeVariable a,
    Core.fieldType (Core.name $ Name "tail") $ Core.typeOptional $
      Core.typeApply (Core.typeVariable $ ref testTypeListNameDef) (Core.typeVariable a)]
  where
    a = Core.name $ Name "a"

testTypeListNameDef :: TElement Name
testTypeListNameDef = testGraphDefinition "testTypeListName" $
  Core.name $ Name "List"

--testTypeNumber :: Type
--testTypeNumber = TypeUnion $ RowType testTypeNumberName [
--  Types.field "int" Types.int32,
--  Types.field "float" Types.float32]

testTypeNumberDef :: TElement Type
testTypeNumberDef = testGraphType "testTypeNumber" $
  Core.typeUnion $ Core.rowType (ref testTypeNumberNameDef) $ list [
    Core.fieldType (Core.name $ Name "int") Core.int32Type,
    Core.fieldType (Core.name $ Name "float") Core.float32Type]

testTypeNumberNameDef :: TElement Name
testTypeNumberNameDef = testGraphDefinition "testTypeNumberName" $
  Core.name $ Name "Number"

--testTypePerson :: Type
--testTypePerson = TypeRecord $ RowType testTypePersonName [
--  Types.field "firstName" Types.string,
--  Types.field "lastName" Types.string,
--  Types.field "age" Types.int32]

testTypePersonDef :: TElement Type
testTypePersonDef = testGraphType "testTypePerson" $
  Core.typeRecord $ Core.rowType (ref testTypePersonNameDef) $ list [
    Core.fieldType (Core.name $ Name "firstName") Core.stringType,
    Core.fieldType (Core.name $ Name "lastName") Core.stringType,
    Core.fieldType (Core.name $ Name "age") Core.int32Type]

testTypePersonNameDef :: TElement Name
testTypePersonNameDef = testGraphDefinition "testTypePersonName" $
  Core.name $ Name "Person"

--testTypePersonOrSomething :: Type
--testTypePersonOrSomething = Types.lambda "a" $ TypeUnion $ RowType testTypePersonOrSomethingName [
--  Types.field "person" testTypePerson,
--  Types.field "other" $ Types.var "a"]

testTypePersonOrSomethingDef :: TElement Type
testTypePersonOrSomethingDef = testGraphType "testTypePersonOrSomething" $
  Core.typeLambda $ Core.lambdaType a $ Core.typeUnion $ Core.rowType (ref testTypePersonOrSomethingNameDef) $ list [
    Core.fieldType (Core.name $ Name "person") $ Core.typeVariable $ ref testTypePersonNameDef,
    Core.fieldType (Core.name $ Name "other") $ Core.typeVariable a]
  where
    a = Core.name $ Name "a"

testTypePersonOrSomethingNameDef :: TElement Name
testTypePersonOrSomethingNameDef = testGraphDefinition "testTypePersonOrSomethingName" $
  Core.name $ Name "PersonOrSomething"

--testTypeSimpleNumber :: Type
--testTypeSimpleNumber = TypeUnion $ RowType testTypeSimpleNumberName [
--  Types.field "int" Types.int32,
--  Types.field "float" Types.float32]

testTypeSimpleNumberDef :: TElement Type
testTypeSimpleNumberDef = testGraphType "testTypeSimpleNumber" $
  Core.typeUnion $ Core.rowType (ref testTypeSimpleNumberNameDef) $ list [
    Core.fieldType (Core.name $ Name "int") Core.int32Type,
    Core.fieldType (Core.name $ Name "float") Core.float32Type]

testTypeSimpleNumberNameDef :: TElement Name
testTypeSimpleNumberNameDef = testGraphDefinition "testTypeSimpleNumberName" $
  Core.name $ Name "SimpleNumber"

--testTypeTimestamp :: Type
--testTypeTimestamp = TypeUnion $ RowType testTypeTimestampName [
--  FieldType (Name "unixTimeMillis") Types.uint64,
--  FieldType (Name "date") Types.string]

testTypeTimestampDef :: TElement Type
testTypeTimestampDef = testGraphType "testTypeTimestamp" $
  Core.typeUnion $ Core.rowType (ref testTypeTimestampNameDef) $ list [
    Core.fieldType (Core.name $ Name "unixTimeMillis") Core.uint64Type,
    Core.fieldType (Core.name $ Name "date") Core.stringType]

testTypeTimestampNameDef :: TElement Name
testTypeTimestampNameDef = testGraphDefinition "testTypeTimestampName" $
  Core.name $ Name "Timestamp"

--testTypeUnionMonomorphic :: Type
--testTypeUnionMonomorphic = TypeUnion $ RowType testTypeUnionMonomorphicName [
--  Types.field "bool" Types.boolean,
--  Types.field "string" Types.string,
--  Types.field "unit" Types.unit]

testTypeUnionMonomorphicDef :: TElement Type
testTypeUnionMonomorphicDef = testGraphType "testTypeUnionMonomorphic" $
  Core.typeUnion $ Core.rowType (ref testTypeUnionMonomorphicNameDef) $ list [
    Core.fieldType (Core.name $ Name "bool") Core.booleanType,
    Core.fieldType (Core.name $ Name "string") Core.stringType,
    Core.fieldType (Core.name $ Name "unit") Core.unitType]

testTypeUnionMonomorphicNameDef :: TElement Name
testTypeUnionMonomorphicNameDef = testGraphDefinition "testTypeUnionMonomorphicName" $
  Core.name $ Name "UnionMonomorphic"

--testTypeUnionPolymorphicRecursive :: Type
--testTypeUnionPolymorphicRecursive = Types.lambda "a" $ TypeUnion $ RowType testTypeUnionPolymorphicRecursiveName [
--  Types.field "bool" Types.boolean,
--  Types.field "value" $ Types.var "a",
--  Types.field "other" $ Types.apply (TypeVariable testTypeUnionPolymorphicRecursiveName) (Types.var "a")]

testTypeUnionPolymorphicRecursiveDef :: TElement Type
testTypeUnionPolymorphicRecursiveDef = testGraphType "testTypeUnionPolymorphicRecursive" $
  Core.typeLambda $ Core.lambdaType a $ Core.typeUnion $ Core.rowType (ref testTypeUnionPolymorphicRecursiveNameDef) $ list [
    Core.fieldType (Core.name $ Name "bool") Core.booleanType,
    Core.fieldType (Core.name $ Name "value") $ Core.typeVariable a,
    Core.fieldType (Core.name $ Name "other") $ Core.typeApply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) (Core.typeVariable a)]
  where
    a = Core.name $ Name "a"

testTypeUnionPolymorphicRecursiveNameDef :: TElement Name
testTypeUnionPolymorphicRecursiveNameDef = testGraphDefinition "testTypeUnionPolymorphicRecursiveName" $
  Core.name $ Name "UnionPolymorphicRecursive"

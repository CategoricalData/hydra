module Hydra.Sources.Test.TestTypes where

import Hydra.Kernel
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Module        as DModule
import qualified Hydra.Dsl.Types         as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.List               as L

import qualified Hydra.Dsl.Meta.Types as T
import           Hydra.Dsl.Meta.Terms as MetaTerms


module_ :: Module
module_ = Module (Namespace "hydra.test.testTypes") elements
    []
    kernelTypesModules
    (Just "Type definitions for the test suite")
  where
    elements = [
      el testTypeBuddyListADef,
      el testTypeBuddyListANameDef,
      el testTypeBuddyListBDef,
      el testTypeBuddyListBNameDef,
      el testTypeComparisonDef,
      el testTypeComparisonNameDef,
      el testTypeEitherDef,
      el testTypeEitherNameDef,
      el testTypeFlowDef,
      el testTypeFlowNameDef,
      el testTypeFlowStateDef,
      el testTypeFlowStateNameDef,
      el testTypeHydraLiteralTypeDef,
      el testTypeHydraLiteralTypeNameDef,
      el testTypeHydraTypeDef,
      el testTypeHydraTypeNameDef,
      el testTypeIntListDef,
      el testTypeIntListNameDef,
      el testTypeLatLonDef,
      el testTypeLatLonNameDef,
      el testTypeLatLonPolyDef,
      el testTypeLatLonPolyNameDef,
      el testTypeListDef,
      el testTypeListNameDef,
      el testTypeNumberDef,
      el testTypeNumberNameDef,
      el testTypePersonDef,
      el testTypePersonNameDef,
      el testTypePersonOrSomethingDef,
      el testTypePersonOrSomethingNameDef,
      el testTypePolymorphicWrapperDef,
      el testTypePolymorphicWrapperNameDef,
      el testTypeSimpleNumberDef,
      el testTypeSimpleNumberNameDef,
      el testTypeStringAliasDef,
      el testTypeStringAliasNameDef,
      el testTypeSymmetricTripleDef,
      el testTypeSymmetricTripleNameDef,
      el testTypeTimestampDef,
      el testTypeTimestampNameDef,
      el testTypeTraceDef,
      el testTypeTraceNameDef,
      el testTypeTripleDef,
      el testTypeTripleNameDef,
      el testTypeUnionMonomorphicDef,
      el testTypeUnionMonomorphicNameDef,
      el testTypeUnionPolymorphicRecursiveDef,
      el testTypeUnionPolymorphicRecursiveNameDef,
      el testTypeUnitDef,
      el testTypeUnitNameDef,
      -- Additional utility types
      el concatTypeDef,
      el compareStringsTypeDef,
      el eitherStringOrInt8TypeNameDef,
      el eitherStringOrInt8TypeDef,
      el exampleProjectionTypeDef,
      el listOfInt8sTypeDef,
      el listOfInt16sTypeDef,
      el listOfListsOfStringsTypeDef,
      el listOfSetOfStringsTypeDef,
      el listOfStringsTypeDef,
      el mapOfStringsToIntsTypeDef,
      el optionalInt8TypeDef,
      el optionalInt16TypeDef,
      el optionalStringTypeDef,
      el setOfStringsTypeDef,
      el stringOrIntNameDef,
      el stringOrIntTypeDef,
      el testTypeNameDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

defineType :: String -> TTerm Type -> TBinding Type
defineType name = define name . firstClassType

-- Helper to create fully qualified names within this module
testTypesName :: String -> TTerm Name
--testTypesName localName = name $ "test." <> localName -- TODO: restore the test namespace, which is distinct from the test sources namespace. There are still tests with raw references to test types, e.g. (var "Person").
testTypesName localName = name localName

-- Type name definitions
testTypeBuddyListANameDef :: TBinding Name
testTypeBuddyListANameDef = define "testTypeBuddyListAName" $
  testTypesName "BuddyListA"

testTypeBuddyListBNameDef :: TBinding Name
testTypeBuddyListBNameDef = define "testTypeBuddyListBName" $
  testTypesName "BuddyListB"

testTypeComparisonNameDef :: TBinding Name
testTypeComparisonNameDef = define "testTypeComparisonName" $
  testTypesName "Comparison"

testTypeEitherNameDef :: TBinding Name
testTypeEitherNameDef = define "testTypeEitherName" $
  testTypesName "Either"

testTypeFlowNameDef :: TBinding Name
testTypeFlowNameDef = define "testTypeFlowName" $
  name "hydra.compute.Flow"

testTypeFlowStateNameDef :: TBinding Name
testTypeFlowStateNameDef = define "testTypeFlowStateName" $
  name "hydra.compute.FlowState"

testTypeHydraLiteralTypeNameDef :: TBinding Name
testTypeHydraLiteralTypeNameDef = define "testTypeHydraLiteralTypeName" $
  testTypesName "HydraLiteralType"

testTypeHydraTypeNameDef :: TBinding Name
testTypeHydraTypeNameDef = define "testTypeHydraTypeName" $
  testTypesName "HydraType"

testTypeIntListNameDef :: TBinding Name
testTypeIntListNameDef = define "testTypeIntListName" $
  testTypesName "IntList"

testTypeLatLonNameDef :: TBinding Name
testTypeLatLonNameDef = define "testTypeLatLonName" $
  testTypesName "LatLon"

testTypeLatLonPolyNameDef :: TBinding Name
testTypeLatLonPolyNameDef = define "testTypeLatLonPolyName" $
  testTypesName "LatLonPoly"

testTypeListNameDef :: TBinding Name
testTypeListNameDef = define "testTypeListName" $
  testTypesName "List"

testTypeNumberNameDef :: TBinding Name
testTypeNumberNameDef = define "testTypeNumberName" $
  testTypesName "Number"

testTypePersonNameDef :: TBinding Name
testTypePersonNameDef = define "testTypePersonName" $
  testTypesName "Person"

testTypePersonOrSomethingNameDef :: TBinding Name
testTypePersonOrSomethingNameDef = define "testTypePersonOrSomethingName" $
  testTypesName "PersonOrSomething"

testTypePolymorphicWrapperNameDef :: TBinding Name
testTypePolymorphicWrapperNameDef = define "testTypePolymorphicWrapperName" $
  testTypesName "PolymorphicWrapper"

testTypeSimpleNumberNameDef :: TBinding Name
testTypeSimpleNumberNameDef = define "testTypeSimpleNumberName" $
  testTypesName "SimpleNumber"

testTypeStringAliasNameDef :: TBinding Name
testTypeStringAliasNameDef = define "testTypeStringAliasName" $
  testTypesName "StringAlias"

testTypeSymmetricTripleNameDef :: TBinding Name
testTypeSymmetricTripleNameDef = define "testTypeSymmetricTripleName" $
  testTypesName "SymmetricTriple"

testTypeTimestampNameDef :: TBinding Name
testTypeTimestampNameDef = define "testTypeTimestampName" $
  testTypesName "Timestamp"

testTypeTraceNameDef :: TBinding Name
testTypeTraceNameDef = define "testTypeTraceName" $
  name "hydra.compute.Trace"

testTypeTripleNameDef :: TBinding Name
testTypeTripleNameDef = define "testTypeTripleName" $
  testTypesName "Triple"

testTypeUnionMonomorphicNameDef :: TBinding Name
testTypeUnionMonomorphicNameDef = define "testTypeUnionMonomorphicName" $
  testTypesName "UnionMonomorphic"

testTypeUnionPolymorphicRecursiveNameDef :: TBinding Name
testTypeUnionPolymorphicRecursiveNameDef = define "testTypeUnionPolymorphicRecursiveName" $
  testTypesName "UnionPolymorphicRecursive"

testTypeUnitNameDef :: TBinding Name
testTypeUnitNameDef = define "testTypeUnitName" $
  testTypesName "Unit"

-- Type definitions
testTypeBuddyListADef :: TBinding Type
testTypeBuddyListADef = defineType "testTypeBuddyListA" $
  T.forAll "a" $ T.record (ref testTypeBuddyListANameDef) [
    "head">: T.variable "a",
    "tail">: T.maybe $
      T.apply (Core.typeVariable $ ref testTypeBuddyListBNameDef) (T.variable "a")]

testTypeBuddyListBDef :: TBinding Type
testTypeBuddyListBDef = defineType "testTypeBuddyListB" $
  T.forAll "a" $ T.record (ref testTypeBuddyListBNameDef) [
    "head">: T.variable "a",
    "tail">: T.maybe $
      T.apply (Core.typeVariable $ ref testTypeBuddyListANameDef) (T.variable "a")]

testTypeComparisonDef :: TBinding Type
testTypeComparisonDef = defineType "testTypeComparison" $
  T.union (ref testTypeComparisonNameDef) [
    "lessThan">: T.unit,
    "equalTo">: T.unit,
    "greaterThan">: T.unit]

testTypeEitherDef :: TBinding Type
testTypeEitherDef = defineType "testTypeEither" $
  T.forAll "a" $ T.forAll "b" $ T.union (ref testTypeEitherNameDef) [
    "left">: T.variable "a",
    "right">: T.variable "b"]

testTypeFlowDef :: TBinding Type
testTypeFlowDef = defineType "testTypeFlow" $
  T.forAll "s" $ T.forAll "a" $ T.record (ref testTypeFlowNameDef) [
    "value">: T.function (T.variable "s") (T.apply (T.apply (Core.typeVariable $ ref testTypeFlowStateNameDef) (T.variable "s")) (T.variable "a"))]

testTypeFlowStateDef :: TBinding Type
testTypeFlowStateDef = defineType "testTypeFlowState" $
  T.forAll "s" $ T.forAll "a" $ T.record (ref testTypeFlowStateNameDef) [
    "value">: T.maybe (T.variable "a"),
    "state">: T.variable "s",
    "trace">: Core.typeVariable $ ref testTypeTraceNameDef]

testTypeHydraLiteralTypeDef :: TBinding Type
testTypeHydraLiteralTypeDef = defineType "testTypeHydraLiteralType" $
  T.union (ref testTypeHydraLiteralTypeNameDef) [
    "boolean">: T.boolean,
    "string">: T.string]

testTypeHydraTypeDef :: TBinding Type
testTypeHydraTypeDef = defineType "testTypeHydraType" $
  T.union (ref testTypeHydraTypeNameDef) [
    "literal">: Core.typeVariable $ ref testTypeHydraLiteralTypeNameDef,
    "list">: Core.typeVariable $ ref testTypeHydraTypeNameDef]

testTypeIntListDef :: TBinding Type
testTypeIntListDef = defineType "testTypeIntList" $
  T.record (ref testTypeIntListNameDef) [
    "head">: T.int32,
    "tail">: T.maybe $ Core.typeVariable $ ref testTypeIntListNameDef]

testTypeLatLonDef :: TBinding Type
testTypeLatLonDef = defineType "testTypeLatLon" $
  T.record (ref testTypeLatLonNameDef) [
    "lat">: T.float32,
    "lon">: T.float32]

testTypeLatLonPolyDef :: TBinding Type
testTypeLatLonPolyDef = defineType "testTypeLatLonPoly" $
  T.forAll "a" $ T.record (ref testTypeLatLonPolyNameDef) [
    "lat">: T.variable "a",
    "lon">: T.variable "a"]

testTypeListDef :: TBinding Type
testTypeListDef = defineType "testTypeList" $
  T.forAll "a" $ T.record (ref testTypeListNameDef) [
    "head">: T.variable "a",
    "tail">: T.maybe $
      T.apply (Core.typeVariable $ ref testTypeListNameDef) (T.variable "a")]

testTypeNumberDef :: TBinding Type
testTypeNumberDef = defineType "testTypeNumber" $
  T.union (ref testTypeNumberNameDef) [
    "int">: T.int32,
    "float">: T.float32]

testTypePersonDef :: TBinding Type
testTypePersonDef = defineType "testTypePerson" $
  T.record (ref testTypePersonNameDef) [
    "firstName">: T.string,
    "lastName">: T.string,
    "age">: T.int32]

testTypePersonOrSomethingDef :: TBinding Type
testTypePersonOrSomethingDef = defineType "testTypePersonOrSomething" $
  T.forAll "a" $ T.union (ref testTypePersonOrSomethingNameDef) [
    "person">: Core.typeVariable $ ref testTypePersonNameDef,
    "other">: T.variable "a"]

testTypePolymorphicWrapperDef :: TBinding Type
testTypePolymorphicWrapperDef = defineType "testTypePolymorphicWrapper" $
  T.forAll "a" $ Core.typeWrap $ Core.wrappedType (ref testTypePolymorphicWrapperNameDef) (T.list $ T.variable "a")

testTypeSimpleNumberDef :: TBinding Type
testTypeSimpleNumberDef = defineType "testTypeSimpleNumber" $
  T.union (ref testTypeSimpleNumberNameDef) [
    "int">: T.int32,
    "float">: T.float32]

testTypeStringAliasDef :: TBinding Type
testTypeStringAliasDef = defineType "testTypeStringAlias" $
  Core.typeWrap $ Core.wrappedType (ref testTypeStringAliasNameDef) T.string

testTypeSymmetricTripleDef :: TBinding Type
testTypeSymmetricTripleDef = defineType "testTypeSymmetricTriple" $
  T.forAlls ["v", "e"] $ T.wrap (ref testTypeSymmetricTripleNameDef) $
    T.applys (Core.typeVariable $ ref testTypeTripleNameDef) [T.variable "v", T.variable "e", T.variable "v"]

testTypeTimestampDef :: TBinding Type
testTypeTimestampDef = defineType "testTypeTimestamp" $
  T.union (ref testTypeTimestampNameDef) [
    "unixTimeMillis">: T.uint64,
    "date">: T.string]

testTypeTraceDef :: TBinding Type
testTypeTraceDef = defineType "testTypeTrace" $
  T.record (ref testTypeTraceNameDef) [
    "stack">: T.list T.string,
    "messages">: T.list T.string,
    "other">: T.map T.string T.string]

testTypeTripleDef :: TBinding Type
testTypeTripleDef = defineType "testTypeTriple" $
  T.forAll "a" $ T.forAll "b" $ T.forAll "c" $ T.record (ref testTypeTripleNameDef) [
    "first">: T.variable "a",
    "second">: T.variable "b",
    "third">: T.variable "c"]

testTypeUnionMonomorphicDef :: TBinding Type
testTypeUnionMonomorphicDef = defineType "testTypeUnionMonomorphic" $
  T.union (ref testTypeUnionMonomorphicNameDef) [
    "bool">: T.boolean,
    "string">: T.string,
    "unit">: T.unit]

testTypeUnionPolymorphicRecursiveDef :: TBinding Type
testTypeUnionPolymorphicRecursiveDef = defineType "testTypeUnionPolymorphicRecursive" $
  T.forAll "a" $ T.union (ref testTypeUnionPolymorphicRecursiveNameDef) [
    "bool">: T.boolean,
    "value">: T.variable "a",
    "other">: T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) (T.variable "a")]

testTypeUnitDef :: TBinding Type
testTypeUnitDef = defineType "testTypeUnit" $
  T.record (ref testTypeUnitNameDef) []

-- Additional type utilities for tests

concatTypeDef :: TBinding Type
concatTypeDef = defineType "concatType" $
  T.function T.string $ T.function T.string T.string

compareStringsTypeDef :: TBinding Type
compareStringsTypeDef = defineType "compareStringsType" $
  T.function T.string T.string

eitherStringOrInt8TypeNameDef :: TBinding Name
eitherStringOrInt8TypeNameDef = define "eitherStringOrInt8TypeName" $
  testTypesName "EitherStringOrInt8"

eitherStringOrInt8TypeDef :: TBinding Type
eitherStringOrInt8TypeDef = defineType "eitherStringOrInt8Type" $
  T.union (ref eitherStringOrInt8TypeNameDef) [
    "left">: T.string,
    "right">: Core.typeLiteral $ Core.literalTypeInteger Core.integerTypeInt8]

exampleProjectionTypeDef :: TBinding Type
exampleProjectionTypeDef = defineType "exampleProjectionType" $
  T.function (Core.typeVariable $ ref testTypePersonNameDef) T.string

listOfInt8sTypeDef :: TBinding Type
listOfInt8sTypeDef = defineType "listOfInt8sType" $
  T.list (Core.typeLiteral $ Core.literalTypeInteger Core.integerTypeInt8)

listOfInt16sTypeDef :: TBinding Type
listOfInt16sTypeDef = defineType "listOfInt16sType" $
  T.list T.int16

listOfListsOfStringsTypeDef :: TBinding Type
listOfListsOfStringsTypeDef = defineType "listOfListsOfStringsType" $
  T.list $ T.list T.string

listOfSetOfStringsTypeDef :: TBinding Type
listOfSetOfStringsTypeDef = defineType "listOfSetOfStringsType" $
  T.list $ T.set T.string

listOfStringsTypeDef :: TBinding Type
listOfStringsTypeDef = defineType "listOfStringsType" $
  T.list T.string

mapOfStringsToIntsTypeDef :: TBinding Type
mapOfStringsToIntsTypeDef = defineType "mapOfStringsToIntsType" $
  T.map T.string T.int32

optionalInt8TypeDef :: TBinding Type
optionalInt8TypeDef = defineType "optionalInt8Type" $
  T.maybe (Core.typeLiteral $ Core.literalTypeInteger Core.integerTypeInt8)

optionalInt16TypeDef :: TBinding Type
optionalInt16TypeDef = defineType "optionalInt16Type" $
  T.maybe T.int16

optionalStringTypeDef :: TBinding Type
optionalStringTypeDef = defineType "optionalStringType" $
  T.maybe T.string

setOfStringsTypeDef :: TBinding Type
setOfStringsTypeDef = defineType "setOfStringsType" $
  T.set T.string

stringOrIntNameDef :: TBinding Name
stringOrIntNameDef = define "stringOrIntName" $
  testTypesName "StringOrInt"

stringOrIntTypeDef :: TBinding Type
stringOrIntTypeDef = defineType "stringOrIntType" $
  T.union (ref stringOrIntNameDef) [
    "left">: T.string,
    "right">: T.int32]

testTypeNameDef :: TBinding Name
testTypeNameDef = define "testTypeName" $
  testTypesName "Test"

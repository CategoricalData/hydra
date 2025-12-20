module Hydra.Sources.Test.TestTypes where

import Hydra.Kernel
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Module        as DModule
import qualified Hydra.Dsl.Types         as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.List               as L

import qualified Hydra.Dsl.Meta.Types as T
import           Hydra.Dsl.Meta.Phantoms (toBinding, definitionInModule, firstClassType)
import           Hydra.Dsl.Meta.Base (name, (>:))


ns :: Namespace
ns = Namespace "hydra.test.testTypes"

module_ :: Module
module_ = Module ns elements
    []
    kernelTypesNamespaces
    (Just "Type definitions for the test suite")
  where
    elements = [
      toBinding testTypeBuddyListA,
      toBinding testTypeBuddyListAName,
      toBinding testTypeBuddyListB,
      toBinding testTypeBuddyListBName,
      toBinding testTypeComparison,
      toBinding testTypeComparisonName,
      toBinding testTypeEither,
      toBinding testTypeEitherName,
      toBinding testTypeFlow,
      toBinding testTypeFlowName,
      toBinding testTypeFlowState,
      toBinding testTypeFlowStateName,
      toBinding testTypeHydraLiteralType,
      toBinding testTypeHydraLiteralTypeName,
      toBinding testTypeHydraType,
      toBinding testTypeHydraTypeName,
      toBinding testTypeIntList,
      toBinding testTypeIntListName,
      toBinding testTypeLatLon,
      toBinding testTypeLatLonName,
      toBinding testTypeLatLonPoly,
      toBinding testTypeLatLonPolyName,
      toBinding testTypeList,
      toBinding testTypeListName,
      toBinding testTypeNumber,
      toBinding testTypeNumberName,
      toBinding testTypePerson,
      toBinding testTypePersonName,
      toBinding testTypePersonOrSomething,
      toBinding testTypePersonOrSomethingName,
      toBinding testTypePolymorphicWrapper,
      toBinding testTypePolymorphicWrapperName,
      toBinding testTypeSimpleNumber,
      toBinding testTypeSimpleNumberName,
      toBinding testTypeStringAlias,
      toBinding testTypeStringAliasName,
      toBinding testTypeSymmetricTriple,
      toBinding testTypeSymmetricTripleName,
      toBinding testTypeTimestamp,
      toBinding testTypeTimestampName,
      toBinding testTypeTrace,
      toBinding testTypeTraceName,
      toBinding testTypeTriple,
      toBinding testTypeTripleName,
      toBinding testTypeUnionMonomorphic,
      toBinding testTypeUnionMonomorphicName,
      toBinding testTypeUnionPolymorphicRecursive,
      toBinding testTypeUnionPolymorphicRecursiveName,
      toBinding testTypeUnit,
      toBinding testTypeUnitName,
      -- Additional utility types
      toBinding concatType,
      toBinding compareStringsType,
      toBinding eitherStringOrInt8TypeName,
      toBinding eitherStringOrInt8Type,
      toBinding exampleProjectionType,
      toBinding listOfInt8sType,
      toBinding listOfInt16sType,
      toBinding listOfListsOfStringsType,
      toBinding listOfSetOfStringsType,
      toBinding listOfStringsType,
      toBinding mapOfStringsToIntsType,
      toBinding optionalInt8Type,
      toBinding optionalInt16Type,
      toBinding optionalStringType,
      toBinding setOfStringsType,
      toBinding stringOrIntName,
      toBinding stringOrIntType,
      toBinding testTypeName]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

defineType :: String -> TTerm Type -> TBinding Type
defineType name = define name . firstClassType

-- Helper to create fully qualified names within this module
testTypesName :: String -> TTerm Name
--testTypesName localName = name $ "test." <> localName -- TODO: restore the test namespace, which is distinct from the test sources namespace. There are still tests with raw references to test types, e.g. (var "Person").
testTypesName localName = name localName

-- Type name definitions
testTypeBuddyListAName :: TBinding Name
testTypeBuddyListAName = define "testTypeBuddyListAName" $
  testTypesName "BuddyListA"

testTypeBuddyListBName :: TBinding Name
testTypeBuddyListBName = define "testTypeBuddyListBName" $
  testTypesName "BuddyListB"

testTypeComparisonName :: TBinding Name
testTypeComparisonName = define "testTypeComparisonName" $
  testTypesName "Comparison"

testTypeEitherName :: TBinding Name
testTypeEitherName = define "testTypeEitherName" $
  testTypesName "Either"

testTypeFlowName :: TBinding Name
testTypeFlowName = define "testTypeFlowName" $
  name "hydra.compute.Flow"

testTypeFlowStateName :: TBinding Name
testTypeFlowStateName = define "testTypeFlowStateName" $
  name "hydra.compute.FlowState"

testTypeHydraLiteralTypeName :: TBinding Name
testTypeHydraLiteralTypeName = define "testTypeHydraLiteralTypeName" $
  testTypesName "HydraLiteralType"

testTypeHydraTypeName :: TBinding Name
testTypeHydraTypeName = define "testTypeHydraTypeName" $
  testTypesName "HydraType"

testTypeIntListName :: TBinding Name
testTypeIntListName = define "testTypeIntListName" $
  testTypesName "IntList"

testTypeLatLonName :: TBinding Name
testTypeLatLonName = define "testTypeLatLonName" $
  testTypesName "LatLon"

testTypeLatLonPolyName :: TBinding Name
testTypeLatLonPolyName = define "testTypeLatLonPolyName" $
  testTypesName "LatLonPoly"

testTypeListName :: TBinding Name
testTypeListName = define "testTypeListName" $
  testTypesName "List"

testTypeNumberName :: TBinding Name
testTypeNumberName = define "testTypeNumberName" $
  testTypesName "Number"

testTypePersonName :: TBinding Name
testTypePersonName = define "testTypePersonName" $
  testTypesName "Person"

testTypePersonOrSomethingName :: TBinding Name
testTypePersonOrSomethingName = define "testTypePersonOrSomethingName" $
  testTypesName "PersonOrSomething"

testTypePolymorphicWrapperName :: TBinding Name
testTypePolymorphicWrapperName = define "testTypePolymorphicWrapperName" $
  testTypesName "PolymorphicWrapper"

testTypeSimpleNumberName :: TBinding Name
testTypeSimpleNumberName = define "testTypeSimpleNumberName" $
  testTypesName "SimpleNumber"

testTypeStringAliasName :: TBinding Name
testTypeStringAliasName = define "testTypeStringAliasName" $
  testTypesName "StringAlias"

testTypeSymmetricTripleName :: TBinding Name
testTypeSymmetricTripleName = define "testTypeSymmetricTripleName" $
  testTypesName "SymmetricTriple"

testTypeTimestampName :: TBinding Name
testTypeTimestampName = define "testTypeTimestampName" $
  testTypesName "Timestamp"

testTypeTraceName :: TBinding Name
testTypeTraceName = define "testTypeTraceName" $
  name "hydra.compute.Trace"

testTypeTripleName :: TBinding Name
testTypeTripleName = define "testTypeTripleName" $
  testTypesName "Triple"

testTypeUnionMonomorphicName :: TBinding Name
testTypeUnionMonomorphicName = define "testTypeUnionMonomorphicName" $
  testTypesName "UnionMonomorphic"

testTypeUnionPolymorphicRecursiveName :: TBinding Name
testTypeUnionPolymorphicRecursiveName = define "testTypeUnionPolymorphicRecursiveName" $
  testTypesName "UnionPolymorphicRecursive"

testTypeUnitName :: TBinding Name
testTypeUnitName = define "testTypeUnitName" $
  testTypesName "Unit"

-- Type definitions
testTypeBuddyListA :: TBinding Type
testTypeBuddyListA = defineType "testTypeBuddyListA" $
  T.forAll "a" $ T.record (testTypeBuddyListAName) [
    "head">: T.variable "a",
    "tail">: T.maybe $
      T.apply (Core.typeVariable $ testTypeBuddyListBName) (T.variable "a")]

testTypeBuddyListB :: TBinding Type
testTypeBuddyListB = defineType "testTypeBuddyListB" $
  T.forAll "a" $ T.record (testTypeBuddyListBName) [
    "head">: T.variable "a",
    "tail">: T.maybe $
      T.apply (Core.typeVariable $ testTypeBuddyListAName) (T.variable "a")]

testTypeComparison :: TBinding Type
testTypeComparison = defineType "testTypeComparison" $
  T.union (testTypeComparisonName) [
    "lessThan">: T.unit,
    "equalTo">: T.unit,
    "greaterThan">: T.unit]

testTypeEither :: TBinding Type
testTypeEither = defineType "testTypeEither" $
  T.forAll "a" $ T.forAll "b" $ T.union testTypeEitherName [
    "left">: T.variable "a",
    "right">: T.variable "b"]

testTypeFlow :: TBinding Type
testTypeFlow = defineType "testTypeFlow" $
  T.forAll "s" $ T.forAll "a" $ T.record testTypeFlowName [
    "value">: T.function (T.variable "s") (T.apply (T.apply (Core.typeVariable testTypeFlowStateName) (T.variable "s")) (T.variable "a"))]

testTypeFlowState :: TBinding Type
testTypeFlowState = defineType "testTypeFlowState" $
  T.forAll "s" $ T.forAll "a" $ T.record testTypeFlowStateName [
    "value">: T.maybe (T.variable "a"),
    "state">: T.variable "s",
    "trace">: Core.typeVariable testTypeTraceName]

testTypeHydraLiteralType :: TBinding Type
testTypeHydraLiteralType = defineType "testTypeHydraLiteralType" $
  T.union testTypeHydraLiteralTypeName [
    "boolean">: T.boolean,
    "string">: T.string]

testTypeHydraType :: TBinding Type
testTypeHydraType = defineType "testTypeHydraType" $
  T.union testTypeHydraTypeName [
    "literal">: Core.typeVariable testTypeHydraLiteralTypeName,
    "list">: Core.typeVariable testTypeHydraTypeName]

testTypeIntList :: TBinding Type
testTypeIntList = defineType "testTypeIntList" $
  T.record testTypeIntListName [
    "head">: T.int32,
    "tail">: T.maybe $ Core.typeVariable testTypeIntListName]

testTypeLatLon :: TBinding Type
testTypeLatLon = defineType "testTypeLatLon" $
  T.record testTypeLatLonName [
    "lat">: T.float32,
    "lon">: T.float32]

testTypeLatLonPoly :: TBinding Type
testTypeLatLonPoly = defineType "testTypeLatLonPoly" $
  T.forAll "a" $ T.record testTypeLatLonPolyName [
    "lat">: T.variable "a",
    "lon">: T.variable "a"]

testTypeList :: TBinding Type
testTypeList = defineType "testTypeList" $
  T.forAll "a" $ T.record testTypeListName [
    "head">: T.variable "a",
    "tail">: T.maybe $
      T.apply (Core.typeVariable testTypeListName) (T.variable "a")]

testTypeNumber :: TBinding Type
testTypeNumber = defineType "testTypeNumber" $
  T.union testTypeNumberName [
    "int">: T.int32,
    "float">: T.float32]

testTypePerson :: TBinding Type
testTypePerson = defineType "testTypePerson" $
  T.record testTypePersonName [
    "firstName">: T.string,
    "lastName">: T.string,
    "age">: T.int32]

testTypePersonOrSomething :: TBinding Type
testTypePersonOrSomething = defineType "testTypePersonOrSomething" $
  T.forAll "a" $ T.union testTypePersonOrSomethingName [
    "person">: Core.typeVariable testTypePersonName,
    "other">: T.variable "a"]

testTypePolymorphicWrapper :: TBinding Type
testTypePolymorphicWrapper = defineType "testTypePolymorphicWrapper" $
  T.forAll "a" $ Core.typeWrap $ Core.wrappedType testTypePolymorphicWrapperName (T.list $ T.variable "a")

testTypeSimpleNumber :: TBinding Type
testTypeSimpleNumber = defineType "testTypeSimpleNumber" $
  T.union testTypeSimpleNumberName [
    "int">: T.int32,
    "float">: T.float32]

testTypeStringAlias :: TBinding Type
testTypeStringAlias = defineType "testTypeStringAlias" $
  Core.typeWrap $ Core.wrappedType testTypeStringAliasName T.string

testTypeSymmetricTriple :: TBinding Type
testTypeSymmetricTriple = defineType "testTypeSymmetricTriple" $
  T.forAlls ["v", "e"] $ T.wrap testTypeSymmetricTripleName $
    T.applys (Core.typeVariable testTypeTripleName) [T.variable "v", T.variable "e", T.variable "v"]

testTypeTimestamp :: TBinding Type
testTypeTimestamp = defineType "testTypeTimestamp" $
  T.union testTypeTimestampName [
    "unixTimeMillis">: T.uint64,
    "date">: T.string]

testTypeTrace :: TBinding Type
testTypeTrace = defineType "testTypeTrace" $
  T.record testTypeTraceName [
    "stack">: T.list T.string,
    "messages">: T.list T.string,
    "other">: T.map T.string T.string]

testTypeTriple :: TBinding Type
testTypeTriple = defineType "testTypeTriple" $
  T.forAll "a" $ T.forAll "b" $ T.forAll "c" $ T.record testTypeTripleName [
    "first">: T.variable "a",
    "second">: T.variable "b",
    "third">: T.variable "c"]

testTypeUnionMonomorphic :: TBinding Type
testTypeUnionMonomorphic = defineType "testTypeUnionMonomorphic" $
  T.union testTypeUnionMonomorphicName [
    "bool">: T.boolean,
    "string">: T.string,
    "unit">: T.unit]

testTypeUnionPolymorphicRecursive :: TBinding Type
testTypeUnionPolymorphicRecursive = defineType "testTypeUnionPolymorphicRecursive" $
  T.forAll "a" $ T.union testTypeUnionPolymorphicRecursiveName [
    "bool">: T.boolean,
    "value">: T.variable "a",
    "other">: T.apply (Core.typeVariable testTypeUnionPolymorphicRecursiveName) (T.variable "a")]

testTypeUnit :: TBinding Type
testTypeUnit = defineType "testTypeUnit" $
  T.record testTypeUnitName []

-- Additional type utilities for tests

concatType :: TBinding Type
concatType = defineType "concatType" $
  T.function T.string $ T.function T.string T.string

compareStringsType :: TBinding Type
compareStringsType = defineType "compareStringsType" $
  T.function T.string T.string

eitherStringOrInt8TypeName :: TBinding Name
eitherStringOrInt8TypeName = define "eitherStringOrInt8TypeName" $
  testTypesName "EitherStringOrInt8"

eitherStringOrInt8Type :: TBinding Type
eitherStringOrInt8Type = defineType "eitherStringOrInt8Type" $
  T.union eitherStringOrInt8TypeName [
    "left">: T.string,
    "right">: Core.typeLiteral $ Core.literalTypeInteger Core.integerTypeInt8]

exampleProjectionType :: TBinding Type
exampleProjectionType = defineType "exampleProjectionType" $
  T.function (Core.typeVariable testTypePersonName) T.string

listOfInt8sType :: TBinding Type
listOfInt8sType = defineType "listOfInt8sType" $
  T.list (Core.typeLiteral $ Core.literalTypeInteger Core.integerTypeInt8)

listOfInt16sType :: TBinding Type
listOfInt16sType = defineType "listOfInt16sType" $
  T.list T.int16

listOfListsOfStringsType :: TBinding Type
listOfListsOfStringsType = defineType "listOfListsOfStringsType" $
  T.list $ T.list T.string

listOfSetOfStringsType :: TBinding Type
listOfSetOfStringsType = defineType "listOfSetOfStringsType" $
  T.list $ T.set T.string

listOfStringsType :: TBinding Type
listOfStringsType = defineType "listOfStringsType" $
  T.list T.string

mapOfStringsToIntsType :: TBinding Type
mapOfStringsToIntsType = defineType "mapOfStringsToIntsType" $
  T.map T.string T.int32

optionalInt8Type :: TBinding Type
optionalInt8Type = defineType "optionalInt8Type" $
  T.maybe (Core.typeLiteral $ Core.literalTypeInteger Core.integerTypeInt8)

optionalInt16Type :: TBinding Type
optionalInt16Type = defineType "optionalInt16Type" $
  T.maybe T.int16

optionalStringType :: TBinding Type
optionalStringType = defineType "optionalStringType" $
  T.maybe T.string

setOfStringsType :: TBinding Type
setOfStringsType = defineType "setOfStringsType" $
  T.set T.string

stringOrIntName :: TBinding Name
stringOrIntName = define "stringOrIntName" $
  testTypesName "StringOrInt"

stringOrIntType :: TBinding Type
stringOrIntType = defineType "stringOrIntType" $
  T.union stringOrIntName [
    "left">: T.string,
    "right">: T.int32]

testTypeName :: TBinding Name
testTypeName = define "testTypeName" $
  testTypesName "Test"

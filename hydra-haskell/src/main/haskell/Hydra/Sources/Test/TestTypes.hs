{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Test.TestTypes where

import Hydra.Kernel
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Module        as DModule
import qualified Hydra.Dsl.Types         as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.List               as L

import qualified Hydra.Dsl.TTypes as T
import           Hydra.Dsl.TTerms as TTerms


testTypesModule :: Module
testTypesModule = Module (Namespace "hydra.test.testTypes") elements
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

testTypesDefinition :: String -> TTerm a -> TBinding a
testTypesDefinition = definitionInModule testTypesModule

testTypesType :: String -> TTerm Type -> TBinding Type
testTypesType name = testTypesDefinition name . firstClassType

-- Helper to create fully qualified names within this module
testTypesName :: String -> TTerm Name
--testTypesName localName = name $ "test." <> localName -- TODO: restore the test namespace, which is distinct from the test sources namespace. There are still tests with raw references to test types, e.g. (var "Person").
testTypesName localName = name localName

-- Type name definitions
testTypeBuddyListANameDef :: TBinding Name
testTypeBuddyListANameDef = testTypesDefinition "testTypeBuddyListAName" $
  testTypesName "BuddyListA"

testTypeBuddyListBNameDef :: TBinding Name
testTypeBuddyListBNameDef = testTypesDefinition "testTypeBuddyListBName" $
  testTypesName "BuddyListB"

testTypeComparisonNameDef :: TBinding Name
testTypeComparisonNameDef = testTypesDefinition "testTypeComparisonName" $
  testTypesName "Comparison"

testTypeEitherNameDef :: TBinding Name
testTypeEitherNameDef = testTypesDefinition "testTypeEitherName" $
  testTypesName "Either"

testTypeFlowNameDef :: TBinding Name
testTypeFlowNameDef = testTypesDefinition "testTypeFlowName" $
  name "hydra.compute.Flow"

testTypeFlowStateNameDef :: TBinding Name
testTypeFlowStateNameDef = testTypesDefinition "testTypeFlowStateName" $
  name "hydra.compute.FlowState"

testTypeHydraLiteralTypeNameDef :: TBinding Name
testTypeHydraLiteralTypeNameDef = testTypesDefinition "testTypeHydraLiteralTypeName" $
  testTypesName "HydraLiteralType"

testTypeHydraTypeNameDef :: TBinding Name
testTypeHydraTypeNameDef = testTypesDefinition "testTypeHydraTypeName" $
  testTypesName "HydraType"

testTypeIntListNameDef :: TBinding Name
testTypeIntListNameDef = testTypesDefinition "testTypeIntListName" $
  testTypesName "IntList"

testTypeLatLonNameDef :: TBinding Name
testTypeLatLonNameDef = testTypesDefinition "testTypeLatLonName" $
  testTypesName "LatLon"

testTypeLatLonPolyNameDef :: TBinding Name
testTypeLatLonPolyNameDef = testTypesDefinition "testTypeLatLonPolyName" $
  testTypesName "LatLonPoly"

testTypeListNameDef :: TBinding Name
testTypeListNameDef = testTypesDefinition "testTypeListName" $
  testTypesName "List"

testTypeNumberNameDef :: TBinding Name
testTypeNumberNameDef = testTypesDefinition "testTypeNumberName" $
  testTypesName "Number"

testTypePersonNameDef :: TBinding Name
testTypePersonNameDef = testTypesDefinition "testTypePersonName" $
  testTypesName "Person"

testTypePersonOrSomethingNameDef :: TBinding Name
testTypePersonOrSomethingNameDef = testTypesDefinition "testTypePersonOrSomethingName" $
  testTypesName "PersonOrSomething"

testTypePolymorphicWrapperNameDef :: TBinding Name
testTypePolymorphicWrapperNameDef = testTypesDefinition "testTypePolymorphicWrapperName" $
  testTypesName "PolymorphicWrapper"

testTypeSimpleNumberNameDef :: TBinding Name
testTypeSimpleNumberNameDef = testTypesDefinition "testTypeSimpleNumberName" $
  testTypesName "SimpleNumber"

testTypeStringAliasNameDef :: TBinding Name
testTypeStringAliasNameDef = testTypesDefinition "testTypeStringAliasName" $
  testTypesName "StringAlias"

testTypeSymmetricTripleNameDef :: TBinding Name
testTypeSymmetricTripleNameDef = testTypesDefinition "testTypeSymmetricTripleName" $
  testTypesName "SymmetricTriple"

testTypeTimestampNameDef :: TBinding Name
testTypeTimestampNameDef = testTypesDefinition "testTypeTimestampName" $
  testTypesName "Timestamp"

testTypeTraceNameDef :: TBinding Name
testTypeTraceNameDef = testTypesDefinition "testTypeTraceName" $
  name "hydra.compute.Trace"

testTypeTripleNameDef :: TBinding Name
testTypeTripleNameDef = testTypesDefinition "testTypeTripleName" $
  testTypesName "Triple"

testTypeUnionMonomorphicNameDef :: TBinding Name
testTypeUnionMonomorphicNameDef = testTypesDefinition "testTypeUnionMonomorphicName" $
  testTypesName "UnionMonomorphic"

testTypeUnionPolymorphicRecursiveNameDef :: TBinding Name
testTypeUnionPolymorphicRecursiveNameDef = testTypesDefinition "testTypeUnionPolymorphicRecursiveName" $
  testTypesName "UnionPolymorphicRecursive"

testTypeUnitNameDef :: TBinding Name
testTypeUnitNameDef = testTypesDefinition "testTypeUnitName" $
  testTypesName "Unit"

-- Type definitions
testTypeBuddyListADef :: TBinding Type
testTypeBuddyListADef = testTypesType "testTypeBuddyListA" $
  T.forAll "a" $ T.record (ref testTypeBuddyListANameDef) [
    "head">: T.var "a",
    "tail">: T.optional $
      T.apply (Core.typeVariable $ ref testTypeBuddyListBNameDef) (T.var "a")]

testTypeBuddyListBDef :: TBinding Type
testTypeBuddyListBDef = testTypesType "testTypeBuddyListB" $
  T.forAll "a" $ T.record (ref testTypeBuddyListBNameDef) [
    "head">: T.var "a",
    "tail">: T.optional $
      T.apply (Core.typeVariable $ ref testTypeBuddyListANameDef) (T.var "a")]

testTypeComparisonDef :: TBinding Type
testTypeComparisonDef = testTypesType "testTypeComparison" $
  T.union (ref testTypeComparisonNameDef) [
    "lessThan">: T.unit,
    "equalTo">: T.unit,
    "greaterThan">: T.unit]

testTypeEitherDef :: TBinding Type
testTypeEitherDef = testTypesType "testTypeEither" $
  T.forAll "a" $ T.forAll "b" $ T.union (ref testTypeEitherNameDef) [
    "left">: T.var "a",
    "right">: T.var "b"]

testTypeFlowDef :: TBinding Type
testTypeFlowDef = testTypesType "testTypeFlow" $
  T.forAll "s" $ T.forAll "a" $ T.record (ref testTypeFlowNameDef) [
    "value">: T.function (T.var "s") (T.apply (T.apply (Core.typeVariable $ ref testTypeFlowStateNameDef) (T.var "s")) (T.var "a"))]

testTypeFlowStateDef :: TBinding Type
testTypeFlowStateDef = testTypesType "testTypeFlowState" $
  T.forAll "s" $ T.forAll "a" $ T.record (ref testTypeFlowStateNameDef) [
    "value">: T.optional (T.var "a"),
    "state">: T.var "s",
    "trace">: Core.typeVariable $ ref testTypeTraceNameDef]

testTypeHydraLiteralTypeDef :: TBinding Type
testTypeHydraLiteralTypeDef = testTypesType "testTypeHydraLiteralType" $
  T.union (ref testTypeHydraLiteralTypeNameDef) [
    "boolean">: T.boolean,
    "string">: T.string]

testTypeHydraTypeDef :: TBinding Type
testTypeHydraTypeDef = testTypesType "testTypeHydraType" $
  T.union (ref testTypeHydraTypeNameDef) [
    "literal">: Core.typeVariable $ ref testTypeHydraLiteralTypeNameDef,
    "list">: Core.typeVariable $ ref testTypeHydraTypeNameDef]

testTypeIntListDef :: TBinding Type
testTypeIntListDef = testTypesType "testTypeIntList" $
  T.record (ref testTypeIntListNameDef) [
    "head">: T.int32,
    "tail">: T.optional $ Core.typeVariable $ ref testTypeIntListNameDef]

testTypeLatLonDef :: TBinding Type
testTypeLatLonDef = testTypesType "testTypeLatLon" $
  T.record (ref testTypeLatLonNameDef) [
    "lat">: T.float32,
    "lon">: T.float32]

testTypeLatLonPolyDef :: TBinding Type
testTypeLatLonPolyDef = testTypesType "testTypeLatLonPoly" $
  T.forAll "a" $ T.record (ref testTypeLatLonPolyNameDef) [
    "lat">: T.var "a",
    "lon">: T.var "a"]

testTypeListDef :: TBinding Type
testTypeListDef = testTypesType "testTypeList" $
  T.forAll "a" $ T.record (ref testTypeListNameDef) [
    "head">: T.var "a",
    "tail">: T.optional $
      T.apply (Core.typeVariable $ ref testTypeListNameDef) (T.var "a")]

testTypeNumberDef :: TBinding Type
testTypeNumberDef = testTypesType "testTypeNumber" $
  T.union (ref testTypeNumberNameDef) [
    "int">: T.int32,
    "float">: T.float32]

testTypePersonDef :: TBinding Type
testTypePersonDef = testTypesType "testTypePerson" $
  T.record (ref testTypePersonNameDef) [
    "firstName">: T.string,
    "lastName">: T.string,
    "age">: T.int32]

testTypePersonOrSomethingDef :: TBinding Type
testTypePersonOrSomethingDef = testTypesType "testTypePersonOrSomething" $
  T.forAll "a" $ T.union (ref testTypePersonOrSomethingNameDef) [
    "person">: Core.typeVariable $ ref testTypePersonNameDef,
    "other">: T.var "a"]

testTypePolymorphicWrapperDef :: TBinding Type
testTypePolymorphicWrapperDef = testTypesType "testTypePolymorphicWrapper" $
  T.forAll "a" $ Core.typeWrap $ Core.wrappedType (ref testTypePolymorphicWrapperNameDef) (T.list $ T.var "a")

testTypeSimpleNumberDef :: TBinding Type
testTypeSimpleNumberDef = testTypesType "testTypeSimpleNumber" $
  T.union (ref testTypeSimpleNumberNameDef) [
    "int">: T.int32,
    "float">: T.float32]

testTypeStringAliasDef :: TBinding Type
testTypeStringAliasDef = testTypesType "testTypeStringAlias" $
  Core.typeWrap $ Core.wrappedType (ref testTypeStringAliasNameDef) T.string

testTypeSymmetricTripleDef :: TBinding Type
testTypeSymmetricTripleDef = testTypesType "testTypeSymmetricTriple" $
  T.forAlls ["v", "e"] $ T.wrap (ref testTypeSymmetricTripleNameDef) $
    T.applys (Core.typeVariable $ ref testTypeTripleNameDef) [T.var "v", T.var "e", T.var "v"]

testTypeTimestampDef :: TBinding Type
testTypeTimestampDef = testTypesType "testTypeTimestamp" $
  T.union (ref testTypeTimestampNameDef) [
    "unixTimeMillis">: T.uint64,
    "date">: T.string]

testTypeTraceDef :: TBinding Type
testTypeTraceDef = testTypesType "testTypeTrace" $
  T.record (ref testTypeTraceNameDef) [
    "stack">: T.list T.string,
    "messages">: T.list T.string,
    "other">: T.map T.string T.string]

testTypeTripleDef :: TBinding Type
testTypeTripleDef = testTypesType "testTypeTriple" $
  T.forAll "a" $ T.forAll "b" $ T.forAll "c" $ T.record (ref testTypeTripleNameDef) [
    "first">: T.var "a",
    "second">: T.var "b",
    "third">: T.var "c"]

testTypeUnionMonomorphicDef :: TBinding Type
testTypeUnionMonomorphicDef = testTypesType "testTypeUnionMonomorphic" $
  T.union (ref testTypeUnionMonomorphicNameDef) [
    "bool">: T.boolean,
    "string">: T.string,
    "unit">: T.unit]

testTypeUnionPolymorphicRecursiveDef :: TBinding Type
testTypeUnionPolymorphicRecursiveDef = testTypesType "testTypeUnionPolymorphicRecursive" $
  T.forAll "a" $ T.union (ref testTypeUnionPolymorphicRecursiveNameDef) [
    "bool">: T.boolean,
    "value">: T.var "a",
    "other">: T.apply (Core.typeVariable $ ref testTypeUnionPolymorphicRecursiveNameDef) (T.var "a")]

testTypeUnitDef :: TBinding Type
testTypeUnitDef = testTypesType "testTypeUnit" $
  T.record (ref testTypeUnitNameDef) []

-- Additional type utilities for tests

concatTypeDef :: TBinding Type
concatTypeDef = testTypesType "concatType" $
  T.function T.string $ T.function T.string T.string

compareStringsTypeDef :: TBinding Type
compareStringsTypeDef = testTypesType "compareStringsType" $
  T.function T.string T.string

eitherStringOrInt8TypeNameDef :: TBinding Name
eitherStringOrInt8TypeNameDef = testTypesDefinition "eitherStringOrInt8TypeName" $
  testTypesName "EitherStringOrInt8"

eitherStringOrInt8TypeDef :: TBinding Type
eitherStringOrInt8TypeDef = testTypesType "eitherStringOrInt8Type" $
  T.union (ref eitherStringOrInt8TypeNameDef) [
    "left">: T.string,
    "right">: Core.typeLiteral $ Core.literalTypeInteger Core.integerTypeInt8]

exampleProjectionTypeDef :: TBinding Type
exampleProjectionTypeDef = testTypesType "exampleProjectionType" $
  T.function (Core.typeVariable $ ref testTypePersonNameDef) T.string

listOfInt8sTypeDef :: TBinding Type
listOfInt8sTypeDef = testTypesType "listOfInt8sType" $
  T.list (Core.typeLiteral $ Core.literalTypeInteger Core.integerTypeInt8)

listOfInt16sTypeDef :: TBinding Type
listOfInt16sTypeDef = testTypesType "listOfInt16sType" $
  T.list T.int16

listOfListsOfStringsTypeDef :: TBinding Type
listOfListsOfStringsTypeDef = testTypesType "listOfListsOfStringsType" $
  T.list $ T.list T.string

listOfSetOfStringsTypeDef :: TBinding Type
listOfSetOfStringsTypeDef = testTypesType "listOfSetOfStringsType" $
  T.list $ T.set T.string

listOfStringsTypeDef :: TBinding Type
listOfStringsTypeDef = testTypesType "listOfStringsType" $
  T.list T.string

mapOfStringsToIntsTypeDef :: TBinding Type
mapOfStringsToIntsTypeDef = testTypesType "mapOfStringsToIntsType" $
  T.map T.string T.int32

optionalInt8TypeDef :: TBinding Type
optionalInt8TypeDef = testTypesType "optionalInt8Type" $
  T.optional (Core.typeLiteral $ Core.literalTypeInteger Core.integerTypeInt8)

optionalInt16TypeDef :: TBinding Type
optionalInt16TypeDef = testTypesType "optionalInt16Type" $
  T.optional T.int16

optionalStringTypeDef :: TBinding Type
optionalStringTypeDef = testTypesType "optionalStringType" $
  T.optional T.string

setOfStringsTypeDef :: TBinding Type
setOfStringsTypeDef = testTypesType "setOfStringsType" $
  T.set T.string

stringOrIntNameDef :: TBinding Name
stringOrIntNameDef = testTypesDefinition "stringOrIntName" $
  testTypesName "StringOrInt"

stringOrIntTypeDef :: TBinding Type
stringOrIntTypeDef = testTypesType "stringOrIntType" $
  T.union (ref stringOrIntNameDef) [
    "left">: T.string,
    "right">: T.int32]

testTypeNameDef :: TBinding Name
testTypeNameDef = testTypesDefinition "testTypeName" $
  testTypesName "Test"

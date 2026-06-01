module Hydra.Sources.Test.TestTypes where

-- Standard imports for kernel test fixtures
import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Phantoms                as Phantoms hiding ((++), (>:))
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Data.List                    as L
import qualified Data.Map                     as M

import Hydra.Dsl.Meta.Base (name, (>:))
import qualified Hydra.Dsl.Packaging        as DPackaging
import qualified Hydra.Dsl.Types              as Types
import           Prelude hiding ((++))


ns :: ModuleName
ns = ModuleName "hydra.test.testTypes"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> (kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata ((Just "Type definitions for the test suite"))}
  where
    definitions = [
      toDefinition testTypeBuddyListA,
      toDefinition testTypeBuddyListAName,
      toDefinition testTypeBuddyListB,
      toDefinition testTypeBuddyListBName,
      toDefinition testTypeComparison,
      toDefinition testTypeComparisonName,
      toDefinition testTypeEither,
      toDefinition testTypeEitherName,
      toDefinition testTypeHydraLiteralType,
      toDefinition testTypeHydraLiteralTypeName,
      toDefinition testTypeHydraType,
      toDefinition testTypeHydraTypeName,
      toDefinition testTypeIntList,
      toDefinition testTypeIntListName,
      toDefinition testTypeLatLon,
      toDefinition testTypeLatLonName,
      toDefinition testTypeLatLonPoly,
      toDefinition testTypeLatLonPolyName,
      toDefinition testTypeList,
      toDefinition testTypeListName,
      toDefinition testTypeNumber,
      toDefinition testTypeNumberName,
      toDefinition testTypePerson,
      toDefinition testTypePersonName,
      toDefinition testTypePersonOrSomething,
      toDefinition testTypePersonOrSomethingName,
      toDefinition testTypePolymorphicWrapper,
      toDefinition testTypePolymorphicWrapperName,
      toDefinition testTypeSimpleNumber,
      toDefinition testTypeSimpleNumberName,
      toDefinition testTypeStringAlias,
      toDefinition testTypeStringAliasName,
      toDefinition testTypeSymmetricTriple,
      toDefinition testTypeSymmetricTripleName,
      toDefinition testTypeTimestamp,
      toDefinition testTypeTimestampName,
      toDefinition testTypeTriple,
      toDefinition testTypeTripleName,
      toDefinition testTypeUnionMonomorphic,
      toDefinition testTypeUnionMonomorphicName,
      toDefinition testTypeUnionPolymorphicRecursive,
      toDefinition testTypeUnionPolymorphicRecursiveName,
      toDefinition testTypeUnit,
      toDefinition testTypeUnitName,
      -- Additional utility types
      toDefinition concatType,
      toDefinition compareStringsType,
      toDefinition eitherStringOrInt8TypeName,
      toDefinition eitherStringOrInt8Type,
      toDefinition exampleProjectionType,
      toDefinition listOfInt8sType,
      toDefinition listOfInt16sType,
      toDefinition listOfListsOfStringsType,
      toDefinition listOfSetOfStringsType,
      toDefinition listOfStringsType,
      toDefinition mapOfStringsToIntsType,
      toDefinition optionalInt8Type,
      toDefinition optionalInt16Type,
      toDefinition optionalStringType,
      toDefinition setOfStringsType,
      toDefinition stringOrIntName,
      toDefinition stringOrIntType,
      toDefinition testTypeName]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

defineType :: String -> TypedTerm Type -> TypedTermDefinition Type
defineType name = define name . firstClassType

-- Type definitions
testTypeBuddyListA :: TypedTermDefinition Type
testTypeBuddyListA = defineType "testTypeBuddyListA" $
  T.forAll "a" $ T.record (testTypeBuddyListAName) [
    "head">: T.variable "a",
    "tail">: T.maybe $
      T.apply (Core.typeVariable $ testTypeBuddyListBName) (T.variable "a")]

-- Type name definitions
testTypeBuddyListAName :: TypedTermDefinition Name
testTypeBuddyListAName = define "testTypeBuddyListAName" $
  testTypesName "BuddyListA"

testTypeBuddyListB :: TypedTermDefinition Type
testTypeBuddyListB = defineType "testTypeBuddyListB" $
  T.forAll "a" $ T.record (testTypeBuddyListBName) [
    "head">: T.variable "a",
    "tail">: T.maybe $
      T.apply (Core.typeVariable $ testTypeBuddyListAName) (T.variable "a")]

testTypeBuddyListBName :: TypedTermDefinition Name
testTypeBuddyListBName = define "testTypeBuddyListBName" $
  testTypesName "BuddyListB"

testTypeComparison :: TypedTermDefinition Type
testTypeComparison = defineType "testTypeComparison" $
  T.union (testTypeComparisonName) [
    "lessThan">: T.unit,
    "equalTo">: T.unit,
    "greaterThan">: T.unit]

testTypeComparisonName :: TypedTermDefinition Name
testTypeComparisonName = define "testTypeComparisonName" $
  testTypesName "Comparison"

testTypeEither :: TypedTermDefinition Type
testTypeEither = defineType "testTypeEither" $
  T.forAll "a" $ T.forAll "b" $ T.union testTypeEitherName [
    "left">: T.variable "a",
    "right">: T.variable "b"]

testTypeEitherName :: TypedTermDefinition Name
testTypeEitherName = define "testTypeEitherName" $
  testTypesName "Either"

testTypeHydraLiteralType :: TypedTermDefinition Type
testTypeHydraLiteralType = defineType "testTypeHydraLiteralType" $
  T.union testTypeHydraLiteralTypeName [
    "boolean">: T.boolean,
    "string">: T.string]

testTypeHydraLiteralTypeName :: TypedTermDefinition Name
testTypeHydraLiteralTypeName = define "testTypeHydraLiteralTypeName" $
  testTypesName "HydraLiteralType"

testTypeHydraType :: TypedTermDefinition Type
testTypeHydraType = defineType "testTypeHydraType" $
  T.union testTypeHydraTypeName [
    "literal">: Core.typeVariable testTypeHydraLiteralTypeName,
    "list">: Core.typeVariable testTypeHydraTypeName]

testTypeHydraTypeName :: TypedTermDefinition Name
testTypeHydraTypeName = define "testTypeHydraTypeName" $
  testTypesName "HydraType"

testTypeIntList :: TypedTermDefinition Type
testTypeIntList = defineType "testTypeIntList" $
  T.record testTypeIntListName [
    "head">: T.int32,
    "tail">: T.maybe $ Core.typeVariable testTypeIntListName]

testTypeIntListName :: TypedTermDefinition Name
testTypeIntListName = define "testTypeIntListName" $
  testTypesName "IntList"

testTypeLatLon :: TypedTermDefinition Type
testTypeLatLon = defineType "testTypeLatLon" $
  T.record testTypeLatLonName [
    "lat">: T.float32,
    "lon">: T.float32]

testTypeLatLonName :: TypedTermDefinition Name
testTypeLatLonName = define "testTypeLatLonName" $
  testTypesName "LatLon"

testTypeLatLonPoly :: TypedTermDefinition Type
testTypeLatLonPoly = defineType "testTypeLatLonPoly" $
  T.forAll "a" $ T.record testTypeLatLonPolyName [
    "lat">: T.variable "a",
    "lon">: T.variable "a"]

testTypeLatLonPolyName :: TypedTermDefinition Name
testTypeLatLonPolyName = define "testTypeLatLonPolyName" $
  testTypesName "LatLonPoly"

testTypeList :: TypedTermDefinition Type
testTypeList = defineType "testTypeList" $
  T.forAll "a" $ T.record testTypeListName [
    "head">: T.variable "a",
    "tail">: T.maybe $
      T.apply (Core.typeVariable testTypeListName) (T.variable "a")]

testTypeListName :: TypedTermDefinition Name
testTypeListName = define "testTypeListName" $
  testTypesName "List"

testTypeNumber :: TypedTermDefinition Type
testTypeNumber = defineType "testTypeNumber" $
  T.union testTypeNumberName [
    "int">: T.int32,
    "float">: T.float32]

testTypeNumberName :: TypedTermDefinition Name
testTypeNumberName = define "testTypeNumberName" $
  testTypesName "Number"

testTypePerson :: TypedTermDefinition Type
testTypePerson = defineType "testTypePerson" $
  T.record testTypePersonName [
    "firstName">: T.string,
    "lastName">: T.string,
    "age">: T.int32]

testTypePersonName :: TypedTermDefinition Name
testTypePersonName = define "testTypePersonName" $
  testTypesName "Person"

testTypePersonOrSomething :: TypedTermDefinition Type
testTypePersonOrSomething = defineType "testTypePersonOrSomething" $
  T.forAll "a" $ T.union testTypePersonOrSomethingName [
    "person">: Core.typeVariable testTypePersonName,
    "other">: T.variable "a"]

testTypePersonOrSomethingName :: TypedTermDefinition Name
testTypePersonOrSomethingName = define "testTypePersonOrSomethingName" $
  testTypesName "PersonOrSomething"

testTypePolymorphicWrapper :: TypedTermDefinition Type
testTypePolymorphicWrapper = defineType "testTypePolymorphicWrapper" $
  T.forAll "a" $ Core.typeWrap (T.list $ T.variable "a")

testTypePolymorphicWrapperName :: TypedTermDefinition Name
testTypePolymorphicWrapperName = define "testTypePolymorphicWrapperName" $
  testTypesName "PolymorphicWrapper"

testTypeSimpleNumber :: TypedTermDefinition Type
testTypeSimpleNumber = defineType "testTypeSimpleNumber" $
  T.union testTypeSimpleNumberName [
    "int">: T.int32,
    "float">: T.float32]

testTypeSimpleNumberName :: TypedTermDefinition Name
testTypeSimpleNumberName = define "testTypeSimpleNumberName" $
  testTypesName "SimpleNumber"

testTypeStringAlias :: TypedTermDefinition Type
testTypeStringAlias = defineType "testTypeStringAlias" $
  Core.typeWrap T.string

testTypeStringAliasName :: TypedTermDefinition Name
testTypeStringAliasName = define "testTypeStringAliasName" $
  testTypesName "StringAlias"

testTypeSymmetricTriple :: TypedTermDefinition Type
testTypeSymmetricTriple = defineType "testTypeSymmetricTriple" $
  T.forAlls ["v", "e"] $ T.wrap testTypeSymmetricTripleName $
    T.applys (Core.typeVariable testTypeTripleName) [T.variable "v", T.variable "e", T.variable "v"]

testTypeSymmetricTripleName :: TypedTermDefinition Name
testTypeSymmetricTripleName = define "testTypeSymmetricTripleName" $
  testTypesName "SymmetricTriple"

testTypeTimestamp :: TypedTermDefinition Type
testTypeTimestamp = defineType "testTypeTimestamp" $
  T.union testTypeTimestampName [
    "unixTimeMillis">: T.uint64,
    "date">: T.string]

testTypeTimestampName :: TypedTermDefinition Name
testTypeTimestampName = define "testTypeTimestampName" $
  testTypesName "Timestamp"

testTypeTriple :: TypedTermDefinition Type
testTypeTriple = defineType "testTypeTriple" $
  T.forAll "a" $ T.forAll "b" $ T.forAll "c" $ T.record testTypeTripleName [
    "first">: T.variable "a",
    "second">: T.variable "b",
    "third">: T.variable "c"]

testTypeTripleName :: TypedTermDefinition Name
testTypeTripleName = define "testTypeTripleName" $
  testTypesName "Triple"

testTypeUnionMonomorphic :: TypedTermDefinition Type
testTypeUnionMonomorphic = defineType "testTypeUnionMonomorphic" $
  T.union testTypeUnionMonomorphicName [
    "bool">: T.boolean,
    "string">: T.string,
    "unit">: T.unit]

testTypeUnionMonomorphicName :: TypedTermDefinition Name
testTypeUnionMonomorphicName = define "testTypeUnionMonomorphicName" $
  testTypesName "UnionMonomorphic"

testTypeUnionPolymorphicRecursive :: TypedTermDefinition Type
testTypeUnionPolymorphicRecursive = defineType "testTypeUnionPolymorphicRecursive" $
  T.forAll "a" $ T.union testTypeUnionPolymorphicRecursiveName [
    "bool">: T.boolean,
    "value">: T.variable "a",
    "other">: T.apply (Core.typeVariable testTypeUnionPolymorphicRecursiveName) (T.variable "a")]

testTypeUnionPolymorphicRecursiveName :: TypedTermDefinition Name
testTypeUnionPolymorphicRecursiveName = define "testTypeUnionPolymorphicRecursiveName" $
  testTypesName "UnionPolymorphicRecursive"

testTypeUnit :: TypedTermDefinition Type
testTypeUnit = defineType "testTypeUnit" $
  T.record testTypeUnitName []

testTypeUnitName :: TypedTermDefinition Name
testTypeUnitName = define "testTypeUnitName" $
  testTypesName "Unit"

-- Helper to create fully qualified names within this module
testTypesName :: String -> TypedTerm Name
--testTypesName localName = name $ "test." <> localName -- TODO: restore the test namespace, which is distinct from the test sources namespace. There are still tests with raw references to test types, e.g. (var "Person").
testTypesName localName = name localName

-- Additional type utilities for tests

compareStringsType :: TypedTermDefinition Type
compareStringsType = defineType "compareStringsType" $
  T.function T.string T.string

concatType :: TypedTermDefinition Type
concatType = defineType "concatType" $
  T.function T.string $ T.function T.string T.string

eitherStringOrInt8Type :: TypedTermDefinition Type
eitherStringOrInt8Type = defineType "eitherStringOrInt8Type" $
  T.union eitherStringOrInt8TypeName [
    "left">: T.string,
    "right">: Core.typeLiteral $ Core.literalTypeInteger Core.integerTypeInt8]

eitherStringOrInt8TypeName :: TypedTermDefinition Name
eitherStringOrInt8TypeName = define "eitherStringOrInt8TypeName" $
  testTypesName "EitherStringOrInt8"

exampleProjectionType :: TypedTermDefinition Type
exampleProjectionType = defineType "exampleProjectionType" $
  T.function (Core.typeVariable testTypePersonName) T.string

listOfInt16sType :: TypedTermDefinition Type
listOfInt16sType = defineType "listOfInt16sType" $
  T.list T.int16

listOfInt8sType :: TypedTermDefinition Type
listOfInt8sType = defineType "listOfInt8sType" $
  T.list (Core.typeLiteral $ Core.literalTypeInteger Core.integerTypeInt8)

listOfListsOfStringsType :: TypedTermDefinition Type
listOfListsOfStringsType = defineType "listOfListsOfStringsType" $
  T.list $ T.list T.string

listOfSetOfStringsType :: TypedTermDefinition Type
listOfSetOfStringsType = defineType "listOfSetOfStringsType" $
  T.list $ T.set T.string

listOfStringsType :: TypedTermDefinition Type
listOfStringsType = defineType "listOfStringsType" $
  T.list T.string

mapOfStringsToIntsType :: TypedTermDefinition Type
mapOfStringsToIntsType = defineType "mapOfStringsToIntsType" $
  T.map T.string T.int32

optionalInt16Type :: TypedTermDefinition Type
optionalInt16Type = defineType "optionalInt16Type" $
  T.maybe T.int16

optionalInt8Type :: TypedTermDefinition Type
optionalInt8Type = defineType "optionalInt8Type" $
  T.maybe (Core.typeLiteral $ Core.literalTypeInteger Core.integerTypeInt8)

optionalStringType :: TypedTermDefinition Type
optionalStringType = defineType "optionalStringType" $
  T.maybe T.string

setOfStringsType :: TypedTermDefinition Type
setOfStringsType = defineType "setOfStringsType" $
  T.set T.string

stringOrIntName :: TypedTermDefinition Name
stringOrIntName = define "stringOrIntName" $
  testTypesName "StringOrInt"

stringOrIntType :: TypedTermDefinition Type
stringOrIntType = defineType "stringOrIntType" $
  T.union stringOrIntName [
    "left">: T.string,
    "right">: T.int32]

testTypeName :: TypedTermDefinition Name
testTypeName = define "testTypeName" $
  testTypesName "Test"

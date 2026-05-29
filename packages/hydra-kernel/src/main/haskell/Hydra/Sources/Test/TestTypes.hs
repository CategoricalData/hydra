module Hydra.Sources.Test.TestTypes where

-- Standard imports for kernel test fixtures
import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep)
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
            moduleDescription = (Just "Type definitions for the test suite")}
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

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

defineType :: String -> TTerm Type -> TTermDefinition Type
defineType name = define name . firstClassType

-- Type definitions
testTypeBuddyListA :: TTermDefinition Type
testTypeBuddyListA = defineType "testTypeBuddyListA" $
  T.forAll "a" $ T.record (testTypeBuddyListAName) [
    "head">: T.variable "a",
    "tail">: T.maybe $
      T.apply (Core.typeVariable $ testTypeBuddyListBName) (T.variable "a")]

-- Type name definitions
testTypeBuddyListAName :: TTermDefinition Name
testTypeBuddyListAName = define "testTypeBuddyListAName" $
  testTypesName "BuddyListA"

testTypeBuddyListB :: TTermDefinition Type
testTypeBuddyListB = defineType "testTypeBuddyListB" $
  T.forAll "a" $ T.record (testTypeBuddyListBName) [
    "head">: T.variable "a",
    "tail">: T.maybe $
      T.apply (Core.typeVariable $ testTypeBuddyListAName) (T.variable "a")]

testTypeBuddyListBName :: TTermDefinition Name
testTypeBuddyListBName = define "testTypeBuddyListBName" $
  testTypesName "BuddyListB"

testTypeComparison :: TTermDefinition Type
testTypeComparison = defineType "testTypeComparison" $
  T.union (testTypeComparisonName) [
    "lessThan">: T.unit,
    "equalTo">: T.unit,
    "greaterThan">: T.unit]

testTypeComparisonName :: TTermDefinition Name
testTypeComparisonName = define "testTypeComparisonName" $
  testTypesName "Comparison"

testTypeEither :: TTermDefinition Type
testTypeEither = defineType "testTypeEither" $
  T.forAll "a" $ T.forAll "b" $ T.union testTypeEitherName [
    "left">: T.variable "a",
    "right">: T.variable "b"]

testTypeEitherName :: TTermDefinition Name
testTypeEitherName = define "testTypeEitherName" $
  testTypesName "Either"

testTypeHydraLiteralType :: TTermDefinition Type
testTypeHydraLiteralType = defineType "testTypeHydraLiteralType" $
  T.union testTypeHydraLiteralTypeName [
    "boolean">: T.boolean,
    "string">: T.string]

testTypeHydraLiteralTypeName :: TTermDefinition Name
testTypeHydraLiteralTypeName = define "testTypeHydraLiteralTypeName" $
  testTypesName "HydraLiteralType"

testTypeHydraType :: TTermDefinition Type
testTypeHydraType = defineType "testTypeHydraType" $
  T.union testTypeHydraTypeName [
    "literal">: Core.typeVariable testTypeHydraLiteralTypeName,
    "list">: Core.typeVariable testTypeHydraTypeName]

testTypeHydraTypeName :: TTermDefinition Name
testTypeHydraTypeName = define "testTypeHydraTypeName" $
  testTypesName "HydraType"

testTypeIntList :: TTermDefinition Type
testTypeIntList = defineType "testTypeIntList" $
  T.record testTypeIntListName [
    "head">: T.int32,
    "tail">: T.maybe $ Core.typeVariable testTypeIntListName]

testTypeIntListName :: TTermDefinition Name
testTypeIntListName = define "testTypeIntListName" $
  testTypesName "IntList"

testTypeLatLon :: TTermDefinition Type
testTypeLatLon = defineType "testTypeLatLon" $
  T.record testTypeLatLonName [
    "lat">: T.float32,
    "lon">: T.float32]

testTypeLatLonName :: TTermDefinition Name
testTypeLatLonName = define "testTypeLatLonName" $
  testTypesName "LatLon"

testTypeLatLonPoly :: TTermDefinition Type
testTypeLatLonPoly = defineType "testTypeLatLonPoly" $
  T.forAll "a" $ T.record testTypeLatLonPolyName [
    "lat">: T.variable "a",
    "lon">: T.variable "a"]

testTypeLatLonPolyName :: TTermDefinition Name
testTypeLatLonPolyName = define "testTypeLatLonPolyName" $
  testTypesName "LatLonPoly"

testTypeList :: TTermDefinition Type
testTypeList = defineType "testTypeList" $
  T.forAll "a" $ T.record testTypeListName [
    "head">: T.variable "a",
    "tail">: T.maybe $
      T.apply (Core.typeVariable testTypeListName) (T.variable "a")]

testTypeListName :: TTermDefinition Name
testTypeListName = define "testTypeListName" $
  testTypesName "List"

testTypeNumber :: TTermDefinition Type
testTypeNumber = defineType "testTypeNumber" $
  T.union testTypeNumberName [
    "int">: T.int32,
    "float">: T.float32]

testTypeNumberName :: TTermDefinition Name
testTypeNumberName = define "testTypeNumberName" $
  testTypesName "Number"

testTypePerson :: TTermDefinition Type
testTypePerson = defineType "testTypePerson" $
  T.record testTypePersonName [
    "firstName">: T.string,
    "lastName">: T.string,
    "age">: T.int32]

testTypePersonName :: TTermDefinition Name
testTypePersonName = define "testTypePersonName" $
  testTypesName "Person"

testTypePersonOrSomething :: TTermDefinition Type
testTypePersonOrSomething = defineType "testTypePersonOrSomething" $
  T.forAll "a" $ T.union testTypePersonOrSomethingName [
    "person">: Core.typeVariable testTypePersonName,
    "other">: T.variable "a"]

testTypePersonOrSomethingName :: TTermDefinition Name
testTypePersonOrSomethingName = define "testTypePersonOrSomethingName" $
  testTypesName "PersonOrSomething"

testTypePolymorphicWrapper :: TTermDefinition Type
testTypePolymorphicWrapper = defineType "testTypePolymorphicWrapper" $
  T.forAll "a" $ Core.typeWrap (T.list $ T.variable "a")

testTypePolymorphicWrapperName :: TTermDefinition Name
testTypePolymorphicWrapperName = define "testTypePolymorphicWrapperName" $
  testTypesName "PolymorphicWrapper"

testTypeSimpleNumber :: TTermDefinition Type
testTypeSimpleNumber = defineType "testTypeSimpleNumber" $
  T.union testTypeSimpleNumberName [
    "int">: T.int32,
    "float">: T.float32]

testTypeSimpleNumberName :: TTermDefinition Name
testTypeSimpleNumberName = define "testTypeSimpleNumberName" $
  testTypesName "SimpleNumber"

testTypeStringAlias :: TTermDefinition Type
testTypeStringAlias = defineType "testTypeStringAlias" $
  Core.typeWrap T.string

testTypeStringAliasName :: TTermDefinition Name
testTypeStringAliasName = define "testTypeStringAliasName" $
  testTypesName "StringAlias"

testTypeSymmetricTriple :: TTermDefinition Type
testTypeSymmetricTriple = defineType "testTypeSymmetricTriple" $
  T.forAlls ["v", "e"] $ T.wrap testTypeSymmetricTripleName $
    T.applys (Core.typeVariable testTypeTripleName) [T.variable "v", T.variable "e", T.variable "v"]

testTypeSymmetricTripleName :: TTermDefinition Name
testTypeSymmetricTripleName = define "testTypeSymmetricTripleName" $
  testTypesName "SymmetricTriple"

testTypeTimestamp :: TTermDefinition Type
testTypeTimestamp = defineType "testTypeTimestamp" $
  T.union testTypeTimestampName [
    "unixTimeMillis">: T.uint64,
    "date">: T.string]

testTypeTimestampName :: TTermDefinition Name
testTypeTimestampName = define "testTypeTimestampName" $
  testTypesName "Timestamp"

testTypeTriple :: TTermDefinition Type
testTypeTriple = defineType "testTypeTriple" $
  T.forAll "a" $ T.forAll "b" $ T.forAll "c" $ T.record testTypeTripleName [
    "first">: T.variable "a",
    "second">: T.variable "b",
    "third">: T.variable "c"]

testTypeTripleName :: TTermDefinition Name
testTypeTripleName = define "testTypeTripleName" $
  testTypesName "Triple"

testTypeUnionMonomorphic :: TTermDefinition Type
testTypeUnionMonomorphic = defineType "testTypeUnionMonomorphic" $
  T.union testTypeUnionMonomorphicName [
    "bool">: T.boolean,
    "string">: T.string,
    "unit">: T.unit]

testTypeUnionMonomorphicName :: TTermDefinition Name
testTypeUnionMonomorphicName = define "testTypeUnionMonomorphicName" $
  testTypesName "UnionMonomorphic"

testTypeUnionPolymorphicRecursive :: TTermDefinition Type
testTypeUnionPolymorphicRecursive = defineType "testTypeUnionPolymorphicRecursive" $
  T.forAll "a" $ T.union testTypeUnionPolymorphicRecursiveName [
    "bool">: T.boolean,
    "value">: T.variable "a",
    "other">: T.apply (Core.typeVariable testTypeUnionPolymorphicRecursiveName) (T.variable "a")]

testTypeUnionPolymorphicRecursiveName :: TTermDefinition Name
testTypeUnionPolymorphicRecursiveName = define "testTypeUnionPolymorphicRecursiveName" $
  testTypesName "UnionPolymorphicRecursive"

testTypeUnit :: TTermDefinition Type
testTypeUnit = defineType "testTypeUnit" $
  T.record testTypeUnitName []

testTypeUnitName :: TTermDefinition Name
testTypeUnitName = define "testTypeUnitName" $
  testTypesName "Unit"

-- Helper to create fully qualified names within this module
testTypesName :: String -> TTerm Name
--testTypesName localName = name $ "test." <> localName -- TODO: restore the test namespace, which is distinct from the test sources namespace. There are still tests with raw references to test types, e.g. (var "Person").
testTypesName localName = name localName

-- Additional type utilities for tests

compareStringsType :: TTermDefinition Type
compareStringsType = defineType "compareStringsType" $
  T.function T.string T.string

concatType :: TTermDefinition Type
concatType = defineType "concatType" $
  T.function T.string $ T.function T.string T.string

eitherStringOrInt8Type :: TTermDefinition Type
eitherStringOrInt8Type = defineType "eitherStringOrInt8Type" $
  T.union eitherStringOrInt8TypeName [
    "left">: T.string,
    "right">: Core.typeLiteral $ Core.literalTypeInteger Core.integerTypeInt8]

eitherStringOrInt8TypeName :: TTermDefinition Name
eitherStringOrInt8TypeName = define "eitherStringOrInt8TypeName" $
  testTypesName "EitherStringOrInt8"

exampleProjectionType :: TTermDefinition Type
exampleProjectionType = defineType "exampleProjectionType" $
  T.function (Core.typeVariable testTypePersonName) T.string

listOfInt16sType :: TTermDefinition Type
listOfInt16sType = defineType "listOfInt16sType" $
  T.list T.int16

listOfInt8sType :: TTermDefinition Type
listOfInt8sType = defineType "listOfInt8sType" $
  T.list (Core.typeLiteral $ Core.literalTypeInteger Core.integerTypeInt8)

listOfListsOfStringsType :: TTermDefinition Type
listOfListsOfStringsType = defineType "listOfListsOfStringsType" $
  T.list $ T.list T.string

listOfSetOfStringsType :: TTermDefinition Type
listOfSetOfStringsType = defineType "listOfSetOfStringsType" $
  T.list $ T.set T.string

listOfStringsType :: TTermDefinition Type
listOfStringsType = defineType "listOfStringsType" $
  T.list T.string

mapOfStringsToIntsType :: TTermDefinition Type
mapOfStringsToIntsType = defineType "mapOfStringsToIntsType" $
  T.map T.string T.int32

optionalInt16Type :: TTermDefinition Type
optionalInt16Type = defineType "optionalInt16Type" $
  T.maybe T.int16

optionalInt8Type :: TTermDefinition Type
optionalInt8Type = defineType "optionalInt8Type" $
  T.maybe (Core.typeLiteral $ Core.literalTypeInteger Core.integerTypeInt8)

optionalStringType :: TTermDefinition Type
optionalStringType = defineType "optionalStringType" $
  T.maybe T.string

setOfStringsType :: TTermDefinition Type
setOfStringsType = defineType "setOfStringsType" $
  T.set T.string

stringOrIntName :: TTermDefinition Name
stringOrIntName = define "stringOrIntName" $
  testTypesName "StringOrInt"

stringOrIntType :: TTermDefinition Type
stringOrIntType = defineType "stringOrIntType" $
  T.union stringOrIntName [
    "left">: T.string,
    "right">: T.int32]

testTypeName :: TTermDefinition Name
testTypeName = define "testTypeName" $
  testTypesName "Test"

-- | A module defining the graph used in the test suite.

module Hydra.Test.TestGraph where

import qualified Hydra.Core as Core
import qualified Hydra.Module as Module
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

testTypeLatLonName :: Core.Name
testTypeLatLonName = (Core.Name "LatLon")

testTypeLatLonPolyName :: Core.Name
testTypeLatLonPolyName = (Core.Name "LatLonPoly")

latlonRecord :: (Float -> Float -> Core.Term)
latlonRecord lat lon = (Core.TermRecord (Core.Record {
  Core.recordTypeName = testTypeLatLonName,
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "lat"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 lat)))},
    Core.Field {
      Core.fieldName = (Core.Name "lon"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 lon)))}]}))

testTypeLatLon :: Core.Type
testTypeLatLon = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = testTypeLatLonName,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lat"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lon"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32))}]}))

testTypeLatLonPoly :: Core.Type
testTypeLatLonPoly = (Core.TypeForall (Core.ForallType {
  Core.forallTypeParameter = (Core.Name "a"),
  Core.forallTypeBody = (Core.TypeRecord (Core.RowType {
    Core.rowTypeTypeName = testTypeLatLonPolyName,
    Core.rowTypeFields = [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "lat"),
        Core.fieldTypeType = (Core.TypeVariable (Core.Name "a"))},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "lon"),
        Core.fieldTypeType = (Core.TypeVariable (Core.Name "a"))}]}))}))

testTypeStringAlias :: Core.Type
testTypeStringAlias = (Core.TypeWrap (Core.WrappedType {
  Core.wrappedTypeTypeName = testTypeStringAliasName,
  Core.wrappedTypeObject = (Core.TypeLiteral Core.LiteralTypeString)}))

testTypeStringAliasName :: Core.Name
testTypeStringAliasName = (Core.Name "StringTypeAlias")

testTypePolymorphicWrapper :: Core.Type
testTypePolymorphicWrapper = (Core.TypeForall (Core.ForallType {
  Core.forallTypeParameter = (Core.Name "a"),
  Core.forallTypeBody = (Core.TypeWrap (Core.WrappedType {
    Core.wrappedTypeTypeName = testTypePolymorphicWrapperName,
    Core.wrappedTypeObject = (Core.TypeList (Core.TypeVariable (Core.Name "a")))}))}))

testTypePolymorphicWrapperName :: Core.Name
testTypePolymorphicWrapperName = (Core.Name "PolymorphicWrapper")

testElementArthur :: Core.Binding
testElementArthur = Core.Binding {
  Core.bindingName = (Core.Name "firstName"),
  Core.bindingTerm = testDataArthur,
  Core.bindingType = (Just (Core.TypeScheme {
    Core.typeSchemeVariables = [],
    Core.typeSchemeType = (Core.TypeVariable testTypePersonName)}))}

testElementFirstName :: Core.Binding
testElementFirstName = Core.Binding {
  Core.bindingName = (Core.Name "firstName"),
  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = testTypePersonName,
    Core.projectionField = (Core.Name "firstName")})))),
  Core.bindingType = (Just (Core.TypeScheme {
    Core.typeSchemeVariables = [],
    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
      Core.functionTypeDomain = (Core.TypeVariable testTypePersonName),
      Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}))}

testNamespace :: Module.Namespace
testNamespace = (Module.Namespace "testGraph")

testSchemaNamespace :: Module.Namespace
testSchemaNamespace = (Module.Namespace "testSchemaGraph")

testDataArthur :: Core.Term
testDataArthur = (Core.TermRecord (Core.Record {
  Core.recordTypeName = testTypePersonName,
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "firstName"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Arthur"))},
    Core.Field {
      Core.fieldName = (Core.Name "lastName"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Dent"))},
    Core.Field {
      Core.fieldName = (Core.Name "age"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}]}))

testTypeBuddyListA :: Core.Type
testTypeBuddyListA = (Core.TypeForall (Core.ForallType {
  Core.forallTypeParameter = (Core.Name "a"),
  Core.forallTypeBody = (Core.TypeRecord (Core.RowType {
    Core.rowTypeTypeName = testTypeBuddyListAName,
    Core.rowTypeFields = [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "head"),
        Core.fieldTypeType = (Core.TypeVariable (Core.Name "a"))},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "tail"),
        Core.fieldTypeType = (Core.TypeOptional (Core.TypeApplication (Core.ApplicationType {
          Core.applicationTypeFunction = (Core.TypeVariable testTypeBuddyListBName),
          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "a"))})))}]}))}))

testTypeBuddyListAName :: Core.Name
testTypeBuddyListAName = (Core.Name "BuddyListA")

testTypeBuddyListB :: Core.Type
testTypeBuddyListB = (Core.TypeForall (Core.ForallType {
  Core.forallTypeParameter = (Core.Name "a"),
  Core.forallTypeBody = (Core.TypeRecord (Core.RowType {
    Core.rowTypeTypeName = testTypeBuddyListBName,
    Core.rowTypeFields = [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "head"),
        Core.fieldTypeType = (Core.TypeVariable (Core.Name "a"))},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "tail"),
        Core.fieldTypeType = (Core.TypeOptional (Core.TypeApplication (Core.ApplicationType {
          Core.applicationTypeFunction = (Core.TypeVariable testTypeBuddyListAName),
          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "a"))})))}]}))}))

testTypeBuddyListBName :: Core.Name
testTypeBuddyListBName = (Core.Name "BuddyListB")

testTypeComparison :: Core.Type
testTypeComparison = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = testTypeComparisonName,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lessThan"),
      Core.fieldTypeType = Core.TypeUnit},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "equalTo"),
      Core.fieldTypeType = Core.TypeUnit},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "greaterThan"),
      Core.fieldTypeType = Core.TypeUnit}]}))

testTypeComparisonName :: Core.Name
testTypeComparisonName = (Core.Name "Comparison")

testTypeIntList :: Core.Type
testTypeIntList = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = testTypeIntListName,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "head"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "tail"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeVariable testTypeIntListName))}]}))

testTypeIntListName :: Core.Name
testTypeIntListName = (Core.Name "IntList")

testTypeHydraLiteralType :: Core.Type
testTypeHydraLiteralType = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = testTypeHydraLiteralTypeName,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "boolean"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)}]}))

testTypeHydraLiteralTypeName :: Core.Name
testTypeHydraLiteralTypeName = (Core.Name "HydraLiteralType")

testTypeHydraType :: Core.Type
testTypeHydraType = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = testTypeHydraTypeName,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "literal"),
      Core.fieldTypeType = (Core.TypeVariable testTypeHydraLiteralTypeName)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "list"),
      Core.fieldTypeType = (Core.TypeVariable testTypeHydraTypeName)}]}))

testTypeHydraTypeName :: Core.Name
testTypeHydraTypeName = (Core.Name "HydraType")

testTypeList :: Core.Type
testTypeList = (Core.TypeForall (Core.ForallType {
  Core.forallTypeParameter = (Core.Name "a"),
  Core.forallTypeBody = (Core.TypeRecord (Core.RowType {
    Core.rowTypeTypeName = testTypeListName,
    Core.rowTypeFields = [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "head"),
        Core.fieldTypeType = (Core.TypeVariable (Core.Name "a"))},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "tail"),
        Core.fieldTypeType = (Core.TypeOptional (Core.TypeApplication (Core.ApplicationType {
          Core.applicationTypeFunction = (Core.TypeVariable testTypeListName),
          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "a"))})))}]}))}))

testTypeListName :: Core.Name
testTypeListName = (Core.Name "List")

testTypeNumber :: Core.Type
testTypeNumber = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = testTypeNumberName,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "int"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "float"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32))}]}))

testTypeNumberName :: Core.Name
testTypeNumberName = (Core.Name "Number")

testTypePerson :: Core.Type
testTypePerson = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = testTypePersonName,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "firstName"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lastName"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "age"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}]}))

testTypePersonName :: Core.Name
testTypePersonName = (Core.Name "Person")

testTypePersonOrSomething :: Core.Type
testTypePersonOrSomething = (Core.TypeForall (Core.ForallType {
  Core.forallTypeParameter = (Core.Name "a"),
  Core.forallTypeBody = (Core.TypeUnion (Core.RowType {
    Core.rowTypeTypeName = testTypePersonOrSomethingName,
    Core.rowTypeFields = [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "person"),
        Core.fieldTypeType = (Core.TypeVariable testTypePersonName)},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "other"),
        Core.fieldTypeType = (Core.TypeVariable (Core.Name "a"))}]}))}))

testTypePersonOrSomethingName :: Core.Name
testTypePersonOrSomethingName = (Core.Name "PersonOrSomething")

testTypeSimpleNumber :: Core.Type
testTypeSimpleNumber = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = testTypeSimpleNumberName,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "int"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "float"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32))}]}))

testTypeSimpleNumberName :: Core.Name
testTypeSimpleNumberName = (Core.Name "SimpleNumber")

testTypeTimestamp :: Core.Type
testTypeTimestamp = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = testTypeTimestampName,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "unixTimeMillis"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint64))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "date"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)}]}))

testTypeTimestampName :: Core.Name
testTypeTimestampName = (Core.Name "Timestamp")

testTypeUnionMonomorphic :: Core.Type
testTypeUnionMonomorphic = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = testTypeUnionMonomorphicName,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "bool"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "unit"),
      Core.fieldTypeType = Core.TypeUnit}]}))

testTypeUnionMonomorphicName :: Core.Name
testTypeUnionMonomorphicName = (Core.Name "UnionMonomorphic")

testTypeUnionPolymorphicRecursive :: Core.Type
testTypeUnionPolymorphicRecursive = (Core.TypeForall (Core.ForallType {
  Core.forallTypeParameter = (Core.Name "a"),
  Core.forallTypeBody = (Core.TypeUnion (Core.RowType {
    Core.rowTypeTypeName = testTypeUnionPolymorphicRecursiveName,
    Core.rowTypeFields = [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "bool"),
        Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "value"),
        Core.fieldTypeType = (Core.TypeVariable (Core.Name "a"))},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "other"),
        Core.fieldTypeType = (Core.TypeApplication (Core.ApplicationType {
          Core.applicationTypeFunction = (Core.TypeVariable testTypeUnionPolymorphicRecursiveName),
          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "a"))}))}]}))}))

testTypeUnionPolymorphicRecursiveName :: Core.Name
testTypeUnionPolymorphicRecursiveName = (Core.Name "UnionPolymorphicRecursive")

testTypeUnit :: Core.Type
testTypeUnit = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = testTypeUnitName,
  Core.rowTypeFields = []}))

testTypeUnitName :: Core.Name
testTypeUnitName = (Core.Name "Unit")

"""A module defining the graph used in the test suite."""

from __future__ import annotations
import hydra.core
import hydra.graph
import hydra.module

testTypeLatLonName = hydra.core.Name("LatLon")

testTypeLatLonPolyName = hydra.core.Name("LatLonPoly")

def latlonRecord(lat, lon) :
    hydra.core.TermRecord(hydra.core.Record("testTypeLatLonName", [
      hydra.core.Field(hydra.core.Name("lat"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(lat)))),
      hydra.core.Field(hydra.core.Name("lon"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(lon))))]))

testTypeLatLon = hydra.core.TypeRecord(hydra.core.RowType("testTypeLatLonName", [
  hydra.core.FieldType(hydra.core.Name("lat"), hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32))),
  hydra.core.FieldType(hydra.core.Name("lon"), hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32)))]))

testTypeLatLonPoly = hydra.core.TypeLambda(hydra.core.LambdaType(hydra.core.Name("a"), hydra.core.TypeRecord(hydra.core.RowType("testTypeLatLonPolyName", [
  hydra.core.FieldType(hydra.core.Name("lat"), hydra.core.TypeVariable(hydra.core.Name("a"))),
  hydra.core.FieldType(hydra.core.Name("lon"), hydra.core.TypeVariable(hydra.core.Name("a")))]))))

testTypeStringAlias = hydra.core.TypeWrap(hydra.core.WrappedType("testTypeStringAliasName", hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))))

testTypeStringAliasName = hydra.core.Name("StringTypeAlias")

testElementArthur = hydra.graph.Element(hydra.core.Name("firstName"), "testDataArthur", hydra.core.TypeScheme([], hydra.core.TypeVariable("testTypePersonName")))

testElementFirstName = hydra.graph.Element(hydra.core.Name("firstName"), hydra.core.TermFunction(hydra.core.FunctionElimination(hydra.core.EliminationRecord(hydra.core.Projection("testTypePersonName", hydra.core.Name("firstName"))))), hydra.core.TypeScheme([], hydra.core.TypeFunction(hydra.core.FunctionType(hydra.core.TypeVariable("testTypePersonName"), hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))))))

testNamespace = hydra.module.Namespace("testGraph")

testSchemaNamespace = hydra.module.Namespace("testSchemaGraph")

testDataArthur = hydra.core.TermRecord(hydra.core.Record("testTypePersonName", [
  hydra.core.Field(hydra.core.Name("firstName"), hydra.core.TermLiteral(hydra.core.LiteralString("Arthur"))),
  hydra.core.Field(hydra.core.Name("lastName"), hydra.core.TermLiteral(hydra.core.LiteralString("Dent"))),
  hydra.core.Field(hydra.core.Name("age"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(42))))]))

testTypeBuddyListA = hydra.core.TypeLambda(hydra.core.LambdaType(hydra.core.Name("a"), hydra.core.TypeRecord(hydra.core.RowType("testTypeBuddyListAName", [
  hydra.core.FieldType(hydra.core.Name("head"), hydra.core.TypeVariable(hydra.core.Name("a"))),
  hydra.core.FieldType(hydra.core.Name("tail"), hydra.core.TypeOptional(hydra.core.TypeApplication(hydra.core.ApplicationType(hydra.core.TypeVariable("testTypeBuddyListBName"), hydra.core.TypeVariable(hydra.core.Name("a"))))))]))))

testTypeBuddyListAName = hydra.core.Name("BuddyListA")

testTypeBuddyListB = hydra.core.TypeLambda(hydra.core.LambdaType(hydra.core.Name("a"), hydra.core.TypeRecord(hydra.core.RowType("testTypeBuddyListBName", [
  hydra.core.FieldType(hydra.core.Name("head"), hydra.core.TypeVariable(hydra.core.Name("a"))),
  hydra.core.FieldType(hydra.core.Name("tail"), hydra.core.TypeOptional(hydra.core.TypeApplication(hydra.core.ApplicationType(hydra.core.TypeVariable("testTypeBuddyListAName"), hydra.core.TypeVariable(hydra.core.Name("a"))))))]))))

testTypeBuddyListBName = hydra.core.Name("BuddyListB")

testTypeComparison = hydra.core.TypeUnion(hydra.core.RowType("testTypeComparisonName", [
  hydra.core.FieldType(hydra.core.Name("lessThan"), hydra.core.TypeRecord(hydra.core.RowType(hydra.core.Name("hydra.core.Unit"), []))),
  hydra.core.FieldType(hydra.core.Name("equalTo"), hydra.core.TypeRecord(hydra.core.RowType(hydra.core.Name("hydra.core.Unit"), []))),
  hydra.core.FieldType(hydra.core.Name("greaterThan"), hydra.core.TypeRecord(hydra.core.RowType(hydra.core.Name("hydra.core.Unit"), [])))]))

testTypeComparisonName = hydra.core.Name("Comparison")

testTypeFoobarValue = hydra.core.TypeUnion(hydra.core.RowType("testTypeFoobarValueName", [
  hydra.core.FieldType(hydra.core.Name("bool"), hydra.core.TypeLiteral(hydra.core.LiteralTypeBoolean(hydra.core.Unit()))),
  hydra.core.FieldType(hydra.core.Name("string"), hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))),
  hydra.core.FieldType(hydra.core.Name("unit"), hydra.core.TypeRecord(hydra.core.RowType(hydra.core.Name("hydra.core.Unit"), [])))]))

testTypeFoobarValueName = hydra.core.Name("FoobarValue")

testTypeIntList = hydra.core.TypeRecord(hydra.core.RowType("testTypeIntListName", [
  hydra.core.FieldType(hydra.core.Name("head"), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))),
  hydra.core.FieldType(hydra.core.Name("tail"), hydra.core.TypeOptional(hydra.core.TypeVariable("testTypeIntListName")))]))

testTypeIntListName = hydra.core.Name("IntList")

testTypeHydraLiteralType = hydra.core.TypeUnion(hydra.core.RowType("testTypeHydraLiteralTypeName", [
  hydra.core.FieldType(hydra.core.Name("boolean"), hydra.core.TypeLiteral(hydra.core.LiteralTypeBoolean(hydra.core.Unit()))),
  hydra.core.FieldType(hydra.core.Name("string"), hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit())))]))

testTypeHydraLiteralTypeName = hydra.core.Name("HydraLiteralType")

testTypeHydraType = hydra.core.TypeUnion(hydra.core.RowType("testTypeHydraTypeName", [
  hydra.core.FieldType(hydra.core.Name("literal"), hydra.core.TypeVariable("testTypeHydraLiteralTypeName")),
  hydra.core.FieldType(hydra.core.Name("list"), hydra.core.TypeVariable("testTypeHydraTypeName"))]))

testTypeHydraTypeName = hydra.core.Name("HydraType")

testTypeList = hydra.core.TypeLambda(hydra.core.LambdaType(hydra.core.Name("a"), hydra.core.TypeRecord(hydra.core.RowType("testTypeListName", [
  hydra.core.FieldType(hydra.core.Name("head"), hydra.core.TypeVariable(hydra.core.Name("a"))),
  hydra.core.FieldType(hydra.core.Name("tail"), hydra.core.TypeOptional(hydra.core.TypeApplication(hydra.core.ApplicationType(hydra.core.TypeVariable("testTypeListName"), hydra.core.TypeVariable(hydra.core.Name("a"))))))]))))

testTypeListName = hydra.core.Name("List")

testTypeNumber = hydra.core.TypeUnion(hydra.core.RowType("testTypeNumberName", [
  hydra.core.FieldType(hydra.core.Name("int"), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))),
  hydra.core.FieldType(hydra.core.Name("float"), hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32)))]))

testTypeNumberName = hydra.core.Name("Number")

testTypePerson = hydra.core.TypeRecord(hydra.core.RowType("testTypePersonName", [
  hydra.core.FieldType(hydra.core.Name("firstName"), hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))),
  hydra.core.FieldType(hydra.core.Name("lastName"), hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))),
  hydra.core.FieldType(hydra.core.Name("age"), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)))]))

testTypePersonName = hydra.core.Name("Person")

testTypePersonOrSomething = hydra.core.TypeLambda(hydra.core.LambdaType(hydra.core.Name("a"), hydra.core.TypeUnion(hydra.core.RowType("testTypePersonOrSomethingName", [
  hydra.core.FieldType(hydra.core.Name("person"), hydra.core.TypeVariable("testTypePersonName")),
  hydra.core.FieldType(hydra.core.Name("other"), hydra.core.TypeVariable(hydra.core.Name("a")))]))))

testTypePersonOrSomethingName = hydra.core.Name("PersonOrSomething")

testTypeSimpleNumber = hydra.core.TypeUnion(hydra.core.RowType("testTypeSimpleNumberName", [
  hydra.core.FieldType(hydra.core.Name("int"), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32))),
  hydra.core.FieldType(hydra.core.Name("float"), hydra.core.TypeLiteral(hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32)))]))

testTypeSimpleNumberName = hydra.core.Name("SimpleNumber")

testTypeTimestamp = hydra.core.TypeUnion(hydra.core.RowType("testTypeTimestampName", [
  hydra.core.FieldType(hydra.core.Name("unixTimeMillis"), hydra.core.TypeLiteral(hydra.core.LiteralTypeInteger(hydra.core.IntegerType.UINT64))),
  hydra.core.FieldType(hydra.core.Name("date"), hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit())))]))

testTypeTimestampName = hydra.core.Name("Timestamp")

testTypeUnionMonomorphic = hydra.core.TypeUnion(hydra.core.RowType("testTypeUnionMonomorphicName", [
  hydra.core.FieldType(hydra.core.Name("bool"), hydra.core.TypeLiteral(hydra.core.LiteralTypeBoolean(hydra.core.Unit()))),
  hydra.core.FieldType(hydra.core.Name("string"), hydra.core.TypeLiteral(hydra.core.LiteralTypeString(hydra.core.Unit()))),
  hydra.core.FieldType(hydra.core.Name("unit"), hydra.core.TypeRecord(hydra.core.RowType(hydra.core.Name("hydra.core.Unit"), [])))]))

testTypeUnionMonomorphicName = hydra.core.Name("UnionMonomorphic")

testTypeUnionPolymorphicRecursive = hydra.core.TypeLambda(hydra.core.LambdaType(hydra.core.Name("a"), hydra.core.TypeUnion(hydra.core.RowType("testTypeUnionPolymorphicRecursiveName", [
  hydra.core.FieldType(hydra.core.Name("bool"), hydra.core.TypeLiteral(hydra.core.LiteralTypeBoolean(hydra.core.Unit()))),
  hydra.core.FieldType(hydra.core.Name("value"), hydra.core.TypeVariable(hydra.core.Name("a"))),
  hydra.core.FieldType(hydra.core.Name("other"), hydra.core.TypeApplication(hydra.core.ApplicationType(hydra.core.TypeVariable("testTypeUnionPolymorphicRecursiveName"), hydra.core.TypeVariable(hydra.core.Name("a")))))]))))

testTypeUnionPolymorphicRecursiveName = hydra.core.Name("UnionPolymorphicRecursive")
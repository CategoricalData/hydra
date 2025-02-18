// Note: this is an automatically generated file. Do not edit.

package hydra.test.testGraph;

/**
 * A module defining the graph used in the test suite.
 */
public interface TestGraph {
  hydra.core.Name testTypeLatLonName = new hydra.core.Name("LatLon");
  
  hydra.core.Name testTypeLatLonPolyName = new hydra.core.Name("LatLonPoly");
  
  static java.util.function.Function<Float, hydra.core.Term> latlonRecord(Float lat) {
    return (java.util.function.Function<Float, hydra.core.Term>) (lon -> new hydra.core.Term.Record(new hydra.core.Record((hydra.test.testGraph.TestGraph.testTypeLatLonName), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("lat"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((lat))))),
      new hydra.core.Field(new hydra.core.Name("lon"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((lon)))))))));
  }
  
  static hydra.core.Type testTypeLatLon() {
    return new hydra.core.Type.Record(new hydra.core.RowType((hydra.test.testGraph.TestGraph.testTypeLatLonName), java.util.Arrays.asList(
      new hydra.core.FieldType(new hydra.core.Name("lat"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32()))),
      new hydra.core.FieldType(new hydra.core.Name("lon"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32()))))));
  }
  
  static hydra.core.Type testTypeLatLonPoly() {
    return new hydra.core.Type.Lambda(new hydra.core.LambdaType(new hydra.core.Name("a"), new hydra.core.Type.Record(new hydra.core.RowType((hydra.test.testGraph.TestGraph.testTypeLatLonPolyName), java.util.Arrays.asList(
      new hydra.core.FieldType(new hydra.core.Name("lat"), new hydra.core.Type.Variable(new hydra.core.Name("a"))),
      new hydra.core.FieldType(new hydra.core.Name("lon"), new hydra.core.Type.Variable(new hydra.core.Name("a"))))))));
  }
  
  static hydra.core.Type testTypeStringAlias() {
    return new hydra.core.Type.Wrap(new hydra.core.WrappedType((hydra.test.testGraph.TestGraph.testTypeStringAliasName), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())));
  }
  
  hydra.core.Name testTypeStringAliasName = new hydra.core.Name("StringTypeAlias");
  
  static hydra.graph.Element testElementArthur() {
    return new hydra.graph.Element(new hydra.core.Name("firstName"), (hydra.test.testGraph.TestGraph.testDataArthur()));
  }
  
  static hydra.graph.Element testElementFirstName() {
    return new hydra.graph.Element(new hydra.core.Name("firstName"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection((hydra.test.testGraph.TestGraph.testTypePersonName), new hydra.core.Name("firstName"))))));
  }
  
  hydra.module.Namespace testNamespace = new hydra.module.Namespace("testGraph");
  
  hydra.module.Namespace testSchemaNamespace = new hydra.module.Namespace("testSchemaGraph");
  
  static hydra.core.Term testDataArthur() {
    return new hydra.core.Term.Record(new hydra.core.Record((hydra.test.testGraph.TestGraph.testTypePersonName), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("firstName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Arthur"))),
      new hydra.core.Field(new hydra.core.Name("lastName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Dent"))),
      new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))));
  }
  
  static hydra.core.Type testTypeBuddyListA() {
    return new hydra.core.Type.Lambda(new hydra.core.LambdaType(new hydra.core.Name("a"), new hydra.core.Type.Record(new hydra.core.RowType((hydra.test.testGraph.TestGraph.testTypeBuddyListAName), java.util.Arrays.asList(
      new hydra.core.FieldType(new hydra.core.Name("head"), new hydra.core.Type.Variable(new hydra.core.Name("a"))),
      new hydra.core.FieldType(new hydra.core.Name("tail"), new hydra.core.Type.Optional(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable((hydra.test.testGraph.TestGraph.testTypeBuddyListBName)), new hydra.core.Type.Variable(new hydra.core.Name("a")))))))))));
  }
  
  hydra.core.Name testTypeBuddyListAName = new hydra.core.Name("BuddyListA");
  
  static hydra.core.Type testTypeBuddyListB() {
    return new hydra.core.Type.Lambda(new hydra.core.LambdaType(new hydra.core.Name("a"), new hydra.core.Type.Record(new hydra.core.RowType((hydra.test.testGraph.TestGraph.testTypeBuddyListBName), java.util.Arrays.asList(
      new hydra.core.FieldType(new hydra.core.Name("head"), new hydra.core.Type.Variable(new hydra.core.Name("a"))),
      new hydra.core.FieldType(new hydra.core.Name("tail"), new hydra.core.Type.Optional(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable((hydra.test.testGraph.TestGraph.testTypeBuddyListAName)), new hydra.core.Type.Variable(new hydra.core.Name("a")))))))))));
  }
  
  hydra.core.Name testTypeBuddyListBName = new hydra.core.Name("BuddyListB");
  
  static hydra.core.Type testTypeComparison() {
    return new hydra.core.Type.Union(new hydra.core.RowType((hydra.test.testGraph.TestGraph.testTypeComparisonName), java.util.Arrays.asList(
      new hydra.core.FieldType(new hydra.core.Name("lessThan"), new hydra.core.Type.Record(new hydra.core.RowType(new hydra.core.Name("hydra/core.Unit"), java.util.Arrays.asList()))),
      new hydra.core.FieldType(new hydra.core.Name("equalTo"), new hydra.core.Type.Record(new hydra.core.RowType(new hydra.core.Name("hydra/core.Unit"), java.util.Arrays.asList()))),
      new hydra.core.FieldType(new hydra.core.Name("greaterThan"), new hydra.core.Type.Record(new hydra.core.RowType(new hydra.core.Name("hydra/core.Unit"), java.util.Arrays.asList()))))));
  }
  
  hydra.core.Name testTypeComparisonName = new hydra.core.Name("Comparison");
  
  static hydra.core.Type testTypeFoobarValue() {
    return new hydra.core.Type.Union(new hydra.core.RowType((hydra.test.testGraph.TestGraph.testTypeFoobarValueName), java.util.Arrays.asList(
      new hydra.core.FieldType(new hydra.core.Name("bool"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_())),
      new hydra.core.FieldType(new hydra.core.Name("string"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("unit"), new hydra.core.Type.Record(new hydra.core.RowType(new hydra.core.Name("hydra/core.Unit"), java.util.Arrays.asList()))))));
  }
  
  hydra.core.Name testTypeFoobarValueName = new hydra.core.Name("FoobarValue");
  
  static hydra.core.Type testTypeIntList() {
    return new hydra.core.Type.Record(new hydra.core.RowType((hydra.test.testGraph.TestGraph.testTypeIntListName), java.util.Arrays.asList(
      new hydra.core.FieldType(new hydra.core.Name("head"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
      new hydra.core.FieldType(new hydra.core.Name("tail"), new hydra.core.Type.Optional(new hydra.core.Type.Variable((hydra.test.testGraph.TestGraph.testTypeIntListName)))))));
  }
  
  hydra.core.Name testTypeIntListName = new hydra.core.Name("IntList");
  
  static hydra.core.Type testTypeHydraLiteralType() {
    return new hydra.core.Type.Union(new hydra.core.RowType((hydra.test.testGraph.TestGraph.testTypeHydraLiteralTypeName), java.util.Arrays.asList(
      new hydra.core.FieldType(new hydra.core.Name("boolean"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_())),
      new hydra.core.FieldType(new hydra.core.Name("string"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))));
  }
  
  hydra.core.Name testTypeHydraLiteralTypeName = new hydra.core.Name("HydraLiteralType");
  
  static hydra.core.Type testTypeHydraType() {
    return new hydra.core.Type.Union(new hydra.core.RowType((hydra.test.testGraph.TestGraph.testTypeHydraTypeName), java.util.Arrays.asList(
      new hydra.core.FieldType(new hydra.core.Name("literal"), new hydra.core.Type.Variable((hydra.test.testGraph.TestGraph.testTypeHydraLiteralTypeName))),
      new hydra.core.FieldType(new hydra.core.Name("list"), new hydra.core.Type.Variable((hydra.test.testGraph.TestGraph.testTypeHydraTypeName))))));
  }
  
  hydra.core.Name testTypeHydraTypeName = new hydra.core.Name("HydraType");
  
  static hydra.core.Type testTypeList() {
    return new hydra.core.Type.Lambda(new hydra.core.LambdaType(new hydra.core.Name("a"), new hydra.core.Type.Record(new hydra.core.RowType((hydra.test.testGraph.TestGraph.testTypeListName), java.util.Arrays.asList(
      new hydra.core.FieldType(new hydra.core.Name("head"), new hydra.core.Type.Variable(new hydra.core.Name("a"))),
      new hydra.core.FieldType(new hydra.core.Name("tail"), new hydra.core.Type.Optional(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable((hydra.test.testGraph.TestGraph.testTypeListName)), new hydra.core.Type.Variable(new hydra.core.Name("a")))))))))));
  }
  
  hydra.core.Name testTypeListName = new hydra.core.Name("List");
  
  static hydra.core.Type testTypeNumber() {
    return new hydra.core.Type.Union(new hydra.core.RowType((hydra.test.testGraph.TestGraph.testTypeNumberName), java.util.Arrays.asList(
      new hydra.core.FieldType(new hydra.core.Name("int"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
      new hydra.core.FieldType(new hydra.core.Name("float"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32()))))));
  }
  
  hydra.core.Name testTypeNumberName = new hydra.core.Name("Number");
  
  static hydra.core.Type testTypePerson() {
    return new hydra.core.Type.Record(new hydra.core.RowType((hydra.test.testGraph.TestGraph.testTypePersonName), java.util.Arrays.asList(
      new hydra.core.FieldType(new hydra.core.Name("firstName"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("lastName"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("age"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))));
  }
  
  hydra.core.Name testTypePersonName = new hydra.core.Name("Person");
  
  static hydra.core.Type testTypePersonOrSomething() {
    return new hydra.core.Type.Lambda(new hydra.core.LambdaType(new hydra.core.Name("a"), new hydra.core.Type.Union(new hydra.core.RowType((hydra.test.testGraph.TestGraph.testTypePersonOrSomethingName), java.util.Arrays.asList(
      new hydra.core.FieldType(new hydra.core.Name("person"), new hydra.core.Type.Variable((hydra.test.testGraph.TestGraph.testTypePersonName))),
      new hydra.core.FieldType(new hydra.core.Name("other"), new hydra.core.Type.Variable(new hydra.core.Name("a"))))))));
  }
  
  hydra.core.Name testTypePersonOrSomethingName = new hydra.core.Name("PersonOrSomething");
  
  static hydra.core.Type testTypeSimpleNumber() {
    return new hydra.core.Type.Union(new hydra.core.RowType((hydra.test.testGraph.TestGraph.testTypeSimpleNumberName), java.util.Arrays.asList(
      new hydra.core.FieldType(new hydra.core.Name("int"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
      new hydra.core.FieldType(new hydra.core.Name("float"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32()))))));
  }
  
  hydra.core.Name testTypeSimpleNumberName = new hydra.core.Name("SimpleNumber");
  
  static hydra.core.Type testTypeTimestamp() {
    return new hydra.core.Type.Union(new hydra.core.RowType((hydra.test.testGraph.TestGraph.testTypeTimestampName), java.util.Arrays.asList(
      new hydra.core.FieldType(new hydra.core.Name("unixTimeMillis"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Uint64()))),
      new hydra.core.FieldType(new hydra.core.Name("date"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))));
  }
  
  hydra.core.Name testTypeTimestampName = new hydra.core.Name("Timestamp");
  
  static hydra.core.Type testTypeUnionMonomorphic() {
    return new hydra.core.Type.Union(new hydra.core.RowType((hydra.test.testGraph.TestGraph.testTypeUnionMonomorphicName), java.util.Arrays.asList(
      new hydra.core.FieldType(new hydra.core.Name("bool"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_())),
      new hydra.core.FieldType(new hydra.core.Name("string"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("unit"), new hydra.core.Type.Record(new hydra.core.RowType(new hydra.core.Name("hydra/core.Unit"), java.util.Arrays.asList()))))));
  }
  
  hydra.core.Name testTypeUnionMonomorphicName = new hydra.core.Name("UnionMonomorphic");
  
  static hydra.core.Type testTypeUnionPolymorphicRecursive() {
    return new hydra.core.Type.Lambda(new hydra.core.LambdaType(new hydra.core.Name("a"), new hydra.core.Type.Union(new hydra.core.RowType((hydra.test.testGraph.TestGraph.testTypeUnionPolymorphicRecursiveName), java.util.Arrays.asList(
      new hydra.core.FieldType(new hydra.core.Name("bool"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_())),
      new hydra.core.FieldType(new hydra.core.Name("value"), new hydra.core.Type.Variable(new hydra.core.Name("a"))),
      new hydra.core.FieldType(new hydra.core.Name("other"), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable((hydra.test.testGraph.TestGraph.testTypeUnionPolymorphicRecursiveName)), new hydra.core.Type.Variable(new hydra.core.Name("a"))))))))));
  }
  
  hydra.core.Name testTypeUnionPolymorphicRecursiveName = new hydra.core.Name("UnionPolymorphicRecursive");
}
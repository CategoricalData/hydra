// Note: this is an automatically generated file. Do not edit.

package hydra.test.testTypes;

/**
 * Type definitions for the test suite
 */
public interface TestTypes {
  static hydra.core.Type testTypeBuddyListA() {
    return new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("a"), new hydra.core.Type.Record(new hydra.core.RowType(hydra.test.testTypes.TestTypes.testTypeBuddyListAName(), java.util.List.of(
      new hydra.core.FieldType(new hydra.core.Name("head"), new hydra.core.Type.Variable(new hydra.core.Name("a"))),
      new hydra.core.FieldType(new hydra.core.Name("tail"), new hydra.core.Type.Maybe(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.testTypes.TestTypes.testTypeBuddyListBName()), new hydra.core.Type.Variable(new hydra.core.Name("a")))))))))));
  }
  
  static hydra.core.Name testTypeBuddyListAName() {
    return new hydra.core.Name("BuddyListA");
  }
  
  static hydra.core.Type testTypeBuddyListB() {
    return new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("a"), new hydra.core.Type.Record(new hydra.core.RowType(hydra.test.testTypes.TestTypes.testTypeBuddyListBName(), java.util.List.of(
      new hydra.core.FieldType(new hydra.core.Name("head"), new hydra.core.Type.Variable(new hydra.core.Name("a"))),
      new hydra.core.FieldType(new hydra.core.Name("tail"), new hydra.core.Type.Maybe(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.testTypes.TestTypes.testTypeBuddyListAName()), new hydra.core.Type.Variable(new hydra.core.Name("a")))))))))));
  }
  
  static hydra.core.Name testTypeBuddyListBName() {
    return new hydra.core.Name("BuddyListB");
  }
  
  static hydra.core.Type testTypeComparison() {
    return new hydra.core.Type.Union(new hydra.core.RowType(hydra.test.testTypes.TestTypes.testTypeComparisonName(), java.util.List.of(
      new hydra.core.FieldType(new hydra.core.Name("lessThan"), new hydra.core.Type.Unit()),
      new hydra.core.FieldType(new hydra.core.Name("equalTo"), new hydra.core.Type.Unit()),
      new hydra.core.FieldType(new hydra.core.Name("greaterThan"), new hydra.core.Type.Unit()))));
  }
  
  static hydra.core.Name testTypeComparisonName() {
    return new hydra.core.Name("Comparison");
  }
  
  static hydra.core.Type testTypeEither() {
    return new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("a"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("b"), new hydra.core.Type.Union(new hydra.core.RowType(hydra.test.testTypes.TestTypes.testTypeEitherName(), java.util.List.of(
      new hydra.core.FieldType(new hydra.core.Name("left"), new hydra.core.Type.Variable(new hydra.core.Name("a"))),
      new hydra.core.FieldType(new hydra.core.Name("right"), new hydra.core.Type.Variable(new hydra.core.Name("b"))))))))));
  }
  
  static hydra.core.Name testTypeEitherName() {
    return new hydra.core.Name("Either");
  }
  
  static hydra.core.Type testTypeFlow() {
    return new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("s"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("a"), new hydra.core.Type.Record(new hydra.core.RowType(hydra.test.testTypes.TestTypes.testTypeFlowName(), java.util.List.of(new hydra.core.FieldType(new hydra.core.Name("value"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("s")), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.testTypes.TestTypes.testTypeFlowStateName()), new hydra.core.Type.Variable(new hydra.core.Name("s")))), new hydra.core.Type.Variable(new hydra.core.Name("a"))))))))))))));
  }
  
  static hydra.core.Name testTypeFlowName() {
    return new hydra.core.Name("hydra.compute.Flow");
  }
  
  static hydra.core.Type testTypeFlowState() {
    return new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("s"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("a"), new hydra.core.Type.Record(new hydra.core.RowType(hydra.test.testTypes.TestTypes.testTypeFlowStateName(), java.util.List.of(
      new hydra.core.FieldType(new hydra.core.Name("value"), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("a")))),
      new hydra.core.FieldType(new hydra.core.Name("state"), new hydra.core.Type.Variable(new hydra.core.Name("s"))),
      new hydra.core.FieldType(new hydra.core.Name("trace"), new hydra.core.Type.Variable(hydra.test.testTypes.TestTypes.testTypeTraceName())))))))));
  }
  
  static hydra.core.Name testTypeFlowStateName() {
    return new hydra.core.Name("hydra.compute.FlowState");
  }
  
  static hydra.core.Type testTypeHydraLiteralType() {
    return new hydra.core.Type.Union(new hydra.core.RowType(hydra.test.testTypes.TestTypes.testTypeHydraLiteralTypeName(), java.util.List.of(
      new hydra.core.FieldType(new hydra.core.Name("boolean"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_())),
      new hydra.core.FieldType(new hydra.core.Name("string"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))));
  }
  
  static hydra.core.Name testTypeHydraLiteralTypeName() {
    return new hydra.core.Name("HydraLiteralType");
  }
  
  static hydra.core.Type testTypeHydraType() {
    return new hydra.core.Type.Union(new hydra.core.RowType(hydra.test.testTypes.TestTypes.testTypeHydraTypeName(), java.util.List.of(
      new hydra.core.FieldType(new hydra.core.Name("literal"), new hydra.core.Type.Variable(hydra.test.testTypes.TestTypes.testTypeHydraLiteralTypeName())),
      new hydra.core.FieldType(new hydra.core.Name("list"), new hydra.core.Type.Variable(hydra.test.testTypes.TestTypes.testTypeHydraTypeName())))));
  }
  
  static hydra.core.Name testTypeHydraTypeName() {
    return new hydra.core.Name("HydraType");
  }
  
  static hydra.core.Type testTypeIntList() {
    return new hydra.core.Type.Record(new hydra.core.RowType(hydra.test.testTypes.TestTypes.testTypeIntListName(), java.util.List.of(
      new hydra.core.FieldType(new hydra.core.Name("head"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
      new hydra.core.FieldType(new hydra.core.Name("tail"), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(hydra.test.testTypes.TestTypes.testTypeIntListName()))))));
  }
  
  static hydra.core.Name testTypeIntListName() {
    return new hydra.core.Name("IntList");
  }
  
  static hydra.core.Type testTypeLatLon() {
    return new hydra.core.Type.Record(new hydra.core.RowType(hydra.test.testTypes.TestTypes.testTypeLatLonName(), java.util.List.of(
      new hydra.core.FieldType(new hydra.core.Name("lat"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32()))),
      new hydra.core.FieldType(new hydra.core.Name("lon"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32()))))));
  }
  
  static hydra.core.Name testTypeLatLonName() {
    return new hydra.core.Name("LatLon");
  }
  
  static hydra.core.Type testTypeLatLonPoly() {
    return new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("a"), new hydra.core.Type.Record(new hydra.core.RowType(hydra.test.testTypes.TestTypes.testTypeLatLonPolyName(), java.util.List.of(
      new hydra.core.FieldType(new hydra.core.Name("lat"), new hydra.core.Type.Variable(new hydra.core.Name("a"))),
      new hydra.core.FieldType(new hydra.core.Name("lon"), new hydra.core.Type.Variable(new hydra.core.Name("a"))))))));
  }
  
  static hydra.core.Name testTypeLatLonPolyName() {
    return new hydra.core.Name("LatLonPoly");
  }
  
  static hydra.core.Type testTypeList() {
    return new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("a"), new hydra.core.Type.Record(new hydra.core.RowType(hydra.test.testTypes.TestTypes.testTypeListName(), java.util.List.of(
      new hydra.core.FieldType(new hydra.core.Name("head"), new hydra.core.Type.Variable(new hydra.core.Name("a"))),
      new hydra.core.FieldType(new hydra.core.Name("tail"), new hydra.core.Type.Maybe(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.testTypes.TestTypes.testTypeListName()), new hydra.core.Type.Variable(new hydra.core.Name("a")))))))))));
  }
  
  static hydra.core.Name testTypeListName() {
    return new hydra.core.Name("List");
  }
  
  static hydra.core.Type testTypeNumber() {
    return new hydra.core.Type.Union(new hydra.core.RowType(hydra.test.testTypes.TestTypes.testTypeNumberName(), java.util.List.of(
      new hydra.core.FieldType(new hydra.core.Name("int"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
      new hydra.core.FieldType(new hydra.core.Name("float"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32()))))));
  }
  
  static hydra.core.Name testTypeNumberName() {
    return new hydra.core.Name("Number");
  }
  
  static hydra.core.Type testTypePerson() {
    return new hydra.core.Type.Record(new hydra.core.RowType(hydra.test.testTypes.TestTypes.testTypePersonName(), java.util.List.of(
      new hydra.core.FieldType(new hydra.core.Name("firstName"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("lastName"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("age"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))));
  }
  
  static hydra.core.Name testTypePersonName() {
    return new hydra.core.Name("Person");
  }
  
  static hydra.core.Type testTypePersonOrSomething() {
    return new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("a"), new hydra.core.Type.Union(new hydra.core.RowType(hydra.test.testTypes.TestTypes.testTypePersonOrSomethingName(), java.util.List.of(
      new hydra.core.FieldType(new hydra.core.Name("person"), new hydra.core.Type.Variable(hydra.test.testTypes.TestTypes.testTypePersonName())),
      new hydra.core.FieldType(new hydra.core.Name("other"), new hydra.core.Type.Variable(new hydra.core.Name("a"))))))));
  }
  
  static hydra.core.Name testTypePersonOrSomethingName() {
    return new hydra.core.Name("PersonOrSomething");
  }
  
  static hydra.core.Type testTypePolymorphicWrapper() {
    return new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("a"), new hydra.core.Type.Wrap(new hydra.core.WrappedType(hydra.test.testTypes.TestTypes.testTypePolymorphicWrapperName(), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("a")))))));
  }
  
  static hydra.core.Name testTypePolymorphicWrapperName() {
    return new hydra.core.Name("PolymorphicWrapper");
  }
  
  static hydra.core.Type testTypeSimpleNumber() {
    return new hydra.core.Type.Union(new hydra.core.RowType(hydra.test.testTypes.TestTypes.testTypeSimpleNumberName(), java.util.List.of(
      new hydra.core.FieldType(new hydra.core.Name("int"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
      new hydra.core.FieldType(new hydra.core.Name("float"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32()))))));
  }
  
  static hydra.core.Name testTypeSimpleNumberName() {
    return new hydra.core.Name("SimpleNumber");
  }
  
  static hydra.core.Type testTypeStringAlias() {
    return new hydra.core.Type.Wrap(new hydra.core.WrappedType(hydra.test.testTypes.TestTypes.testTypeStringAliasName(), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())));
  }
  
  static hydra.core.Name testTypeStringAliasName() {
    return new hydra.core.Name("StringAlias");
  }
  
  static hydra.core.Type testTypeSymmetricTriple() {
    return new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("v"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("e"), new hydra.core.Type.Wrap(new hydra.core.WrappedType(hydra.test.testTypes.TestTypes.testTypeSymmetricTripleName(), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.testTypes.TestTypes.testTypeTripleName()), new hydra.core.Type.Variable(new hydra.core.Name("v")))), new hydra.core.Type.Variable(new hydra.core.Name("e")))), new hydra.core.Type.Variable(new hydra.core.Name("v"))))))))));
  }
  
  static hydra.core.Name testTypeSymmetricTripleName() {
    return new hydra.core.Name("SymmetricTriple");
  }
  
  static hydra.core.Type testTypeTimestamp() {
    return new hydra.core.Type.Union(new hydra.core.RowType(hydra.test.testTypes.TestTypes.testTypeTimestampName(), java.util.List.of(
      new hydra.core.FieldType(new hydra.core.Name("unixTimeMillis"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Uint64()))),
      new hydra.core.FieldType(new hydra.core.Name("date"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))));
  }
  
  static hydra.core.Name testTypeTimestampName() {
    return new hydra.core.Name("Timestamp");
  }
  
  static hydra.core.Type testTypeTrace() {
    return new hydra.core.Type.Record(new hydra.core.RowType(hydra.test.testTypes.TestTypes.testTypeTraceName(), java.util.List.of(
      new hydra.core.FieldType(new hydra.core.Name("stack"), new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))),
      new hydra.core.FieldType(new hydra.core.Name("messages"), new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))),
      new hydra.core.FieldType(new hydra.core.Name("other"), new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))));
  }
  
  static hydra.core.Name testTypeTraceName() {
    return new hydra.core.Name("hydra.compute.Trace");
  }
  
  static hydra.core.Type testTypeTriple() {
    return new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("a"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("b"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("c"), new hydra.core.Type.Record(new hydra.core.RowType(hydra.test.testTypes.TestTypes.testTypeTripleName(), java.util.List.of(
      new hydra.core.FieldType(new hydra.core.Name("first"), new hydra.core.Type.Variable(new hydra.core.Name("a"))),
      new hydra.core.FieldType(new hydra.core.Name("second"), new hydra.core.Type.Variable(new hydra.core.Name("b"))),
      new hydra.core.FieldType(new hydra.core.Name("third"), new hydra.core.Type.Variable(new hydra.core.Name("c"))))))))))));
  }
  
  static hydra.core.Name testTypeTripleName() {
    return new hydra.core.Name("Triple");
  }
  
  static hydra.core.Type testTypeUnionMonomorphic() {
    return new hydra.core.Type.Union(new hydra.core.RowType(hydra.test.testTypes.TestTypes.testTypeUnionMonomorphicName(), java.util.List.of(
      new hydra.core.FieldType(new hydra.core.Name("bool"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_())),
      new hydra.core.FieldType(new hydra.core.Name("string"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("unit"), new hydra.core.Type.Unit()))));
  }
  
  static hydra.core.Name testTypeUnionMonomorphicName() {
    return new hydra.core.Name("UnionMonomorphic");
  }
  
  static hydra.core.Type testTypeUnionPolymorphicRecursive() {
    return new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("a"), new hydra.core.Type.Union(new hydra.core.RowType(hydra.test.testTypes.TestTypes.testTypeUnionPolymorphicRecursiveName(), java.util.List.of(
      new hydra.core.FieldType(new hydra.core.Name("bool"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_())),
      new hydra.core.FieldType(new hydra.core.Name("value"), new hydra.core.Type.Variable(new hydra.core.Name("a"))),
      new hydra.core.FieldType(new hydra.core.Name("other"), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.testTypes.TestTypes.testTypeUnionPolymorphicRecursiveName()), new hydra.core.Type.Variable(new hydra.core.Name("a"))))))))));
  }
  
  static hydra.core.Name testTypeUnionPolymorphicRecursiveName() {
    return new hydra.core.Name("UnionPolymorphicRecursive");
  }
  
  static hydra.core.Type testTypeUnit() {
    return new hydra.core.Type.Record(new hydra.core.RowType(hydra.test.testTypes.TestTypes.testTypeUnitName(), (java.util.List<hydra.core.FieldType>) (java.util.List.<hydra.core.FieldType>of())));
  }
  
  static hydra.core.Name testTypeUnitName() {
    return new hydra.core.Name("Unit");
  }
  
  static hydra.core.Type concatType() {
    return new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))));
  }
  
  static hydra.core.Type compareStringsType() {
    return new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())));
  }
  
  static hydra.core.Name eitherStringOrInt8TypeName() {
    return new hydra.core.Name("EitherStringOrInt8");
  }
  
  static hydra.core.Type eitherStringOrInt8Type() {
    return new hydra.core.Type.Union(new hydra.core.RowType(hydra.test.testTypes.TestTypes.eitherStringOrInt8TypeName(), java.util.List.of(
      new hydra.core.FieldType(new hydra.core.Name("left"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("right"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int8()))))));
  }
  
  static hydra.core.Type exampleProjectionType() {
    return new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(hydra.test.testTypes.TestTypes.testTypePersonName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())));
  }
  
  static hydra.core.Type listOfInt8sType() {
    return new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int8())));
  }
  
  static hydra.core.Type listOfInt16sType() {
    return new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int16())));
  }
  
  static hydra.core.Type listOfListsOfStringsType() {
    return new hydra.core.Type.List(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())));
  }
  
  static hydra.core.Type listOfSetOfStringsType() {
    return new hydra.core.Type.List(new hydra.core.Type.Set(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())));
  }
  
  static hydra.core.Type listOfStringsType() {
    return new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()));
  }
  
  static hydra.core.Type mapOfStringsToIntsType() {
    return new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))));
  }
  
  static hydra.core.Type optionalInt8Type() {
    return new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int8())));
  }
  
  static hydra.core.Type optionalInt16Type() {
    return new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int16())));
  }
  
  static hydra.core.Type optionalStringType() {
    return new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()));
  }
  
  static hydra.core.Type setOfStringsType() {
    return new hydra.core.Type.Set(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()));
  }
  
  static hydra.core.Name stringOrIntName() {
    return new hydra.core.Name("StringOrInt");
  }
  
  static hydra.core.Type stringOrIntType() {
    return new hydra.core.Type.Union(new hydra.core.RowType(hydra.test.testTypes.TestTypes.stringOrIntName(), java.util.List.of(
      new hydra.core.FieldType(new hydra.core.Name("left"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("right"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))));
  }
  
  static hydra.core.Name testTypeName() {
    return new hydra.core.Name("Test");
  }
}

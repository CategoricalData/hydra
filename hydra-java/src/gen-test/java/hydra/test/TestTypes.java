// Note: this is an automatically generated file. Do not edit.

package hydra.test;

/**
 * Type definitions for the test suite
 */
public interface TestTypes {
  static hydra.core.Type testTypeBuddyListA() {
    return new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("a"), new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("head"), new hydra.core.Type.Variable(new hydra.core.Name("a"))),
      new hydra.core.FieldType(new hydra.core.Name("tail"), new hydra.core.Type.Maybe(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeBuddyListBName()), new hydra.core.Type.Variable(new hydra.core.Name("a"))))))))));
  }

  static hydra.core.Name testTypeBuddyListAName() {
    return new hydra.core.Name("BuddyListA");
  }

  static hydra.core.Type testTypeBuddyListB() {
    return new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("a"), new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("head"), new hydra.core.Type.Variable(new hydra.core.Name("a"))),
      new hydra.core.FieldType(new hydra.core.Name("tail"), new hydra.core.Type.Maybe(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeBuddyListAName()), new hydra.core.Type.Variable(new hydra.core.Name("a"))))))))));
  }

  static hydra.core.Name testTypeBuddyListBName() {
    return new hydra.core.Name("BuddyListB");
  }

  static hydra.core.Type testTypeComparison() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("lessThan"), new hydra.core.Type.Unit()),
      new hydra.core.FieldType(new hydra.core.Name("equalTo"), new hydra.core.Type.Unit()),
      new hydra.core.FieldType(new hydra.core.Name("greaterThan"), new hydra.core.Type.Unit())));
  }

  static hydra.core.Name testTypeComparisonName() {
    return new hydra.core.Name("Comparison");
  }

  static hydra.core.Type testTypeEither() {
    return new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("a"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("b"), new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("left"), new hydra.core.Type.Variable(new hydra.core.Name("a"))),
      new hydra.core.FieldType(new hydra.core.Name("right"), new hydra.core.Type.Variable(new hydra.core.Name("b")))))))));
  }

  static hydra.core.Name testTypeEitherName() {
    return new hydra.core.Name("Either");
  }

  static hydra.core.Type testTypeHydraLiteralType() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("boolean"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_())),
      new hydra.core.FieldType(new hydra.core.Name("string"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))));
  }

  static hydra.core.Name testTypeHydraLiteralTypeName() {
    return new hydra.core.Name("HydraLiteralType");
  }

  static hydra.core.Type testTypeHydraType() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("literal"), new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeHydraLiteralTypeName())),
      new hydra.core.FieldType(new hydra.core.Name("list"), new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeHydraTypeName()))));
  }

  static hydra.core.Name testTypeHydraTypeName() {
    return new hydra.core.Name("HydraType");
  }

  static hydra.core.Type testTypeIntList() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("head"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
      new hydra.core.FieldType(new hydra.core.Name("tail"), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeIntListName())))));
  }

  static hydra.core.Name testTypeIntListName() {
    return new hydra.core.Name("IntList");
  }

  static hydra.core.Type testTypeLatLon() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("lat"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32()))),
      new hydra.core.FieldType(new hydra.core.Name("lon"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32())))));
  }

  static hydra.core.Name testTypeLatLonName() {
    return new hydra.core.Name("LatLon");
  }

  static hydra.core.Type testTypeLatLonPoly() {
    return new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("a"), new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("lat"), new hydra.core.Type.Variable(new hydra.core.Name("a"))),
      new hydra.core.FieldType(new hydra.core.Name("lon"), new hydra.core.Type.Variable(new hydra.core.Name("a")))))));
  }

  static hydra.core.Name testTypeLatLonPolyName() {
    return new hydra.core.Name("LatLonPoly");
  }

  static hydra.core.Type testTypeList() {
    return new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("a"), new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("head"), new hydra.core.Type.Variable(new hydra.core.Name("a"))),
      new hydra.core.FieldType(new hydra.core.Name("tail"), new hydra.core.Type.Maybe(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeListName()), new hydra.core.Type.Variable(new hydra.core.Name("a"))))))))));
  }

  static hydra.core.Name testTypeListName() {
    return new hydra.core.Name("List");
  }

  static hydra.core.Type testTypeNumber() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("int"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
      new hydra.core.FieldType(new hydra.core.Name("float"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32())))));
  }

  static hydra.core.Name testTypeNumberName() {
    return new hydra.core.Name("Number");
  }

  static hydra.core.Type testTypePerson() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("firstName"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("lastName"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("age"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))));
  }

  static hydra.core.Name testTypePersonName() {
    return new hydra.core.Name("Person");
  }

  static hydra.core.Type testTypePersonOrSomething() {
    return new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("a"), new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("person"), new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePersonName())),
      new hydra.core.FieldType(new hydra.core.Name("other"), new hydra.core.Type.Variable(new hydra.core.Name("a")))))));
  }

  static hydra.core.Name testTypePersonOrSomethingName() {
    return new hydra.core.Name("PersonOrSomething");
  }

  static hydra.core.Type testTypePolymorphicWrapper() {
    return new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("a"), new hydra.core.Type.Wrap(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("a"))))));
  }

  static hydra.core.Name testTypePolymorphicWrapperName() {
    return new hydra.core.Name("PolymorphicWrapper");
  }

  static hydra.core.Type testTypeSimpleNumber() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("int"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
      new hydra.core.FieldType(new hydra.core.Name("float"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32())))));
  }

  static hydra.core.Name testTypeSimpleNumberName() {
    return new hydra.core.Name("SimpleNumber");
  }

  static hydra.core.Type testTypeStringAlias() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()));
  }

  static hydra.core.Name testTypeStringAliasName() {
    return new hydra.core.Name("StringAlias");
  }

  static hydra.core.Type testTypeSymmetricTriple() {
    return new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("v"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("e"), new hydra.core.Type.Wrap(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeTripleName()), new hydra.core.Type.Variable(new hydra.core.Name("v")))), new hydra.core.Type.Variable(new hydra.core.Name("e")))), new hydra.core.Type.Variable(new hydra.core.Name("v")))))))));
  }

  static hydra.core.Name testTypeSymmetricTripleName() {
    return new hydra.core.Name("SymmetricTriple");
  }

  static hydra.core.Type testTypeTimestamp() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("unixTimeMillis"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Uint64()))),
      new hydra.core.FieldType(new hydra.core.Name("date"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))));
  }

  static hydra.core.Name testTypeTimestampName() {
    return new hydra.core.Name("Timestamp");
  }

  static hydra.core.Type testTypeTriple() {
    return new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("a"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("b"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("c"), new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("first"), new hydra.core.Type.Variable(new hydra.core.Name("a"))),
      new hydra.core.FieldType(new hydra.core.Name("second"), new hydra.core.Type.Variable(new hydra.core.Name("b"))),
      new hydra.core.FieldType(new hydra.core.Name("third"), new hydra.core.Type.Variable(new hydra.core.Name("c")))))))))));
  }

  static hydra.core.Name testTypeTripleName() {
    return new hydra.core.Name("Triple");
  }

  static hydra.core.Type testTypeUnionMonomorphic() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("bool"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_())),
      new hydra.core.FieldType(new hydra.core.Name("string"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("unit"), new hydra.core.Type.Unit())));
  }

  static hydra.core.Name testTypeUnionMonomorphicName() {
    return new hydra.core.Name("UnionMonomorphic");
  }

  static hydra.core.Type testTypeUnionPolymorphicRecursive() {
    return new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("a"), new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("bool"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_())),
      new hydra.core.FieldType(new hydra.core.Name("value"), new hydra.core.Type.Variable(new hydra.core.Name("a"))),
      new hydra.core.FieldType(new hydra.core.Name("other"), new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypeUnionPolymorphicRecursiveName()), new hydra.core.Type.Variable(new hydra.core.Name("a")))))))));
  }

  static hydra.core.Name testTypeUnionPolymorphicRecursiveName() {
    return new hydra.core.Name("UnionPolymorphicRecursive");
  }

  static hydra.core.Type testTypeUnit() {
    return new hydra.core.Type.Record((hydra.util.ConsList<hydra.core.FieldType>) (hydra.util.ConsList.<hydra.core.FieldType>empty()));
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
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("left"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("right"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int8())))));
  }

  static hydra.core.Type exampleProjectionType() {
    return new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePersonName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())));
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
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("left"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("right"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))));
  }

  static hydra.core.Name testTypeName() {
    return new hydra.core.Name("Test");
  }
}

// Note: this is an automatically generated file. Do not edit.

package hydra.test.testTerms;

/**
 * Term definitions for the test suite
 */
public interface TestTerms {
  static hydra.core.Term latlonRecord(Float lat, Float lon) {
    return new hydra.core.Term.Record(new hydra.core.Record(hydra.test.testTypes.TestTypes.testTypeLatLonName(), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("lat"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((lat))))),
      new hydra.core.Field(new hydra.core.Name("lon"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((lon))))))));
  }
  
  static hydra.core.Term testDataArthur() {
    return new hydra.core.Term.Record(new hydra.core.Record(hydra.test.testTypes.TestTypes.testTypePersonName(), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("firstName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Arthur"))),
      new hydra.core.Field(new hydra.core.Name("lastName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Dent"))),
      new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))));
  }
  
  static hydra.core.Binding testElementArthur() {
    return new hydra.core.Binding(new hydra.core.Name("firstName"), hydra.test.testTerms.TestTerms.testDataArthur(), hydra.util.Maybe.just(new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()), new hydra.core.Type.Variable(hydra.test.testTypes.TestTypes.testTypePersonName()), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))));
  }
  
  static hydra.core.Binding testElementFirstName() {
    return new hydra.core.Binding(new hydra.core.Name("firstName"), new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(hydra.test.testTypes.TestTypes.testTypePersonName(), new hydra.core.Name("firstName"))))), hydra.util.Maybe.just(new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(hydra.test.testTypes.TestTypes.testTypePersonName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))));
  }
}

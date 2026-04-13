// Note: this is an automatically generated file. Do not edit.

package hydra.test;

/**
 * Term definitions for the test suite
 */
public interface TestTerms {
  static hydra.core.Term latlonRecord(Float lat, Float lon) {
    return new hydra.core.Term.Record(new hydra.core.Record(hydra.test.TestTypes.testTypeLatLonName(), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("lat"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32(lat)))),
      new hydra.core.Field(new hydra.core.Name("lon"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32(lon)))))));
  }

  static hydra.core.Term testDataArthur() {
    return new hydra.core.Term.Record(new hydra.core.Record(hydra.test.TestTypes.testTypePersonName(), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("firstName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Arthur"))),
      new hydra.core.Field(new hydra.core.Name("lastName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Dent"))),
      new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))));
  }

  static hydra.core.Binding testElementArthur() {
    return new hydra.core.Binding(new hydra.core.Name("firstName"), hydra.test.TestTerms.testDataArthur(), hydra.util.Maybe.just(new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePersonName()), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))));
  }

  static hydra.core.Binding testElementFirstName() {
    return new hydra.core.Binding(new hydra.core.Name("firstName"), new hydra.core.Term.Project(new hydra.core.Projection(hydra.test.TestTypes.testTypePersonName(), new hydra.core.Name("firstName"))), hydra.util.Maybe.just(new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(hydra.test.TestTypes.testTypePersonName()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing()))));
  }
}

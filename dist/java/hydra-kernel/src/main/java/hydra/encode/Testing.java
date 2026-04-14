// Note: this is an automatically generated file. Do not edit.

package hydra.encode;

/**
 * Term encoders for hydra.testing
 */
public interface Testing {
  static hydra.core.Term tag(hydra.testing.Tag x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.testing.Tag"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).value))));
  }

  static hydra.core.Term testCase(hydra.testing.TestCase v1) {
    return (v1).accept(new hydra.testing.TestCase.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.testing.TestCase.Universal y) {
        return new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("universal"), hydra.encode.Testing.universalTestCase((y).value))));
      }
    });
  }

  static hydra.core.Term testCaseWithMetadata(hydra.testing.TestCaseWithMetadata x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).name))),
      new hydra.core.Field(new hydra.core.Name("case"), hydra.encode.Testing.testCase((x).case_)),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
        (java.util.function.Function<String, hydra.core.Term>) (x2 -> new hydra.core.Term.Literal(new hydra.core.Literal.String_(x2))),
        (x).description))),
      new hydra.core.Field(new hydra.core.Name("tags"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.Testing::tag,
        (x).tags))))));
  }

  static hydra.core.Term testGroup(hydra.testing.TestGroup x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestGroup"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).name))),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
        (java.util.function.Function<String, hydra.core.Term>) (x2 -> new hydra.core.Term.Literal(new hydra.core.Literal.String_(x2))),
        (x).description))),
      new hydra.core.Field(new hydra.core.Name("subgroups"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.Testing::testGroup,
        (x).subgroups))),
      new hydra.core.Field(new hydra.core.Name("cases"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.Testing::testCaseWithMetadata,
        (x).cases))))));
  }

  static hydra.core.Term universalTestCase(hydra.testing.UniversalTestCase x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.UniversalTestCase"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("actual"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).actual))),
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).expected))))));
  }
}

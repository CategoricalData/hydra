// Note: this is an automatically generated file. Do not edit.

package hydra.dsl;

/**
 * DSL functions for hydra.testing
 */
public interface Testing {
  static hydra.phantoms.TTerm<hydra.testing.Tag> tag(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.testing.Tag"), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseUniversal(hydra.phantoms.TTerm<hydra.testing.UniversalTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.testing.TestCase"), new hydra.core.Field(new hydra.core.Name("universal"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> testCaseWithMetadata(hydra.phantoms.TTerm<String> name, hydra.phantoms.TTerm<hydra.testing.TestCase> case_, hydra.phantoms.TTerm<hydra.util.Maybe<String>> description, hydra.phantoms.TTerm<java.util.List<hydra.testing.Tag>> tags) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("case"), (case_).value),
      new hydra.core.Field(new hydra.core.Name("description"), (description).value),
      new hydra.core.Field(new hydra.core.Name("tags"), (tags).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCase> testCaseWithMetadataCase(hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("case"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<String>> testCaseWithMetadataDescription(hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("description"))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> testCaseWithMetadataName(hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.testing.Tag>> testCaseWithMetadataTags(hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("tags"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> testCaseWithMetadataWithCase(hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> original, hydra.phantoms.TTerm<hydra.testing.TestCase> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("case"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("description"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("tags"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("tags"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> testCaseWithMetadataWithDescription(hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> original, hydra.phantoms.TTerm<hydra.util.Maybe<String>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("case"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("case"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("description"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("tags"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("tags"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> testCaseWithMetadataWithName(hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("case"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("case"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("description"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("tags"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("tags"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> testCaseWithMetadataWithTags(hydra.phantoms.TTerm<hydra.testing.TestCaseWithMetadata> original, hydra.phantoms.TTerm<java.util.List<hydra.testing.Tag>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("case"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("case"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestCaseWithMetadata"), new hydra.core.Name("description"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("tags"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestGroup> testGroup(hydra.phantoms.TTerm<String> name, hydra.phantoms.TTerm<hydra.util.Maybe<String>> description, hydra.phantoms.TTerm<java.util.List<hydra.testing.TestGroup>> subgroups, hydra.phantoms.TTerm<java.util.List<hydra.testing.TestCaseWithMetadata>> cases) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestGroup"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("description"), (description).value),
      new hydra.core.Field(new hydra.core.Name("subgroups"), (subgroups).value),
      new hydra.core.Field(new hydra.core.Name("cases"), (cases).value)))));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.testing.TestCaseWithMetadata>> testGroupCases(hydra.phantoms.TTerm<hydra.testing.TestGroup> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("cases"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.Maybe<String>> testGroupDescription(hydra.phantoms.TTerm<hydra.testing.TestGroup> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("description"))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> testGroupName(hydra.phantoms.TTerm<hydra.testing.TestGroup> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.testing.TestGroup>> testGroupSubgroups(hydra.phantoms.TTerm<hydra.testing.TestGroup> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("subgroups"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestGroup> testGroupWithCases(hydra.phantoms.TTerm<hydra.testing.TestGroup> original, hydra.phantoms.TTerm<java.util.List<hydra.testing.TestCaseWithMetadata>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestGroup"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("description"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("subgroups"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("subgroups"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("cases"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestGroup> testGroupWithDescription(hydra.phantoms.TTerm<hydra.testing.TestGroup> original, hydra.phantoms.TTerm<hydra.util.Maybe<String>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestGroup"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("description"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("subgroups"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("subgroups"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("cases"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("cases"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestGroup> testGroupWithName(hydra.phantoms.TTerm<hydra.testing.TestGroup> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestGroup"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("description"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("subgroups"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("subgroups"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("cases"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("cases"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.TestGroup> testGroupWithSubgroups(hydra.phantoms.TTerm<hydra.testing.TestGroup> original, hydra.phantoms.TTerm<java.util.List<hydra.testing.TestGroup>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.TestGroup"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("description"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("description"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("subgroups"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("cases"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.TestGroup"), new hydra.core.Name("cases"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<String> unTag(hydra.phantoms.TTerm<hydra.testing.Tag> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.testing.Tag")), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.UniversalTestCase> universalTestCase(hydra.phantoms.TTerm<String> actual, hydra.phantoms.TTerm<String> expected) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.UniversalTestCase"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("actual"), (actual).value),
      new hydra.core.Field(new hydra.core.Name("expected"), (expected).value)))));
  }

  static hydra.phantoms.TTerm<String> universalTestCaseActual(hydra.phantoms.TTerm<hydra.testing.UniversalTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UniversalTestCase"), new hydra.core.Name("actual"))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> universalTestCaseExpected(hydra.phantoms.TTerm<hydra.testing.UniversalTestCase> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UniversalTestCase"), new hydra.core.Name("expected"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.testing.UniversalTestCase> universalTestCaseWithActual(hydra.phantoms.TTerm<hydra.testing.UniversalTestCase> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.UniversalTestCase"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("actual"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("expected"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UniversalTestCase"), new hydra.core.Name("expected"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.testing.UniversalTestCase> universalTestCaseWithExpected(hydra.phantoms.TTerm<hydra.testing.UniversalTestCase> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.testing.UniversalTestCase"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("actual"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.testing.UniversalTestCase"), new hydra.core.Name("actual"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("expected"), (newVal).value)))));
  }
}

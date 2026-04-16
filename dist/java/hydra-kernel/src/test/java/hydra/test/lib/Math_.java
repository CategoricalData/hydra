// Note: this is an automatically generated file. Do not edit.

package hydra.test.lib;

/**
 * Test cases for hydra.lib.math primitives
 */
public interface Math_ {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("hydra.lib.math primitives", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("abs", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("positive", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.abs")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.abs")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.abs")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("add", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("positive numbers", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(8)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative numbers", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-3))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-8)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("mixed sign", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-3))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(7)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("with zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("even", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("even positive", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.even")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(4))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("odd positive", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.even")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("even negative", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.even")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-4))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("odd negative", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.even")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.even")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("max", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("first is larger", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.max")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("second is larger", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.max")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("equal values", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.max")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(7))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(7))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(7)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative numbers", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.max")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-3))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-3)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("mixed sign", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.max")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-5))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("with zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.max")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("maybeDiv", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("basic division", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.maybeDiv")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("exact division", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.maybeDiv")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("division by zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.maybeDiv")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))))), hydra.show.Core.term(new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("zero divided", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.maybeDiv")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative dividend", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.maybeDiv")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-10))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-4)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative divisor", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.maybeDiv")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-3))))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-4)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("min", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("first is smaller", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.min")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("second is smaller", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.min")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("equal values", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.min")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(7))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(7))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(7)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative numbers", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.min")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-3))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-5)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("mixed sign", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.min")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-5))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-5)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("with zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.min")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("maybeMod", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("basic modulo", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.maybeMod")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("exact division", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.maybeMod")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("division by zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.maybeMod")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))))), hydra.show.Core.term(new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative dividend", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.maybeMod")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-10))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative divisor", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.maybeMod")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-3))))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-2)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("mul", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("positive numbers", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mul")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(15)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative numbers", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mul")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-3))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(15)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("mixed sign", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mul")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-15)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("with zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mul")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("with one", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mul")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("negate", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("positive", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.negate")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-5)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.negate")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.negate")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("odd", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("odd positive", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.odd")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("even positive", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.odd")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(4))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("odd negative", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.odd")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("even negative", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.odd")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-4))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.odd")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("maybePred", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("positive", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.maybePred")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(4)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.maybePred")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-1)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.maybePred")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-5))))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-6)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("minBound", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.maybePred")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-2147483648))))))), hydra.show.Core.term(new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("range", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("ascending range", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.range")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(4))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("single element", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.range")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("two elements", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.range")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(4))))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(4)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative start", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.range")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-2))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-2))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-1))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("maybeRem", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("basic remainder", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.maybeRem")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("exact division", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.maybeRem")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("division by zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.maybeRem")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))))), hydra.show.Core.term(new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative dividend", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.maybeRem")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-10))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-1)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative divisor", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.maybeRem")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-3))))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("signum", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("positive", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.signum")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.signum")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-1)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.signum")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("sub", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("positive numbers", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sub")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(7)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative numbers", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sub")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-10))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-3))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-7)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("mixed sign", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sub")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-3))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(13)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("with zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sub")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("maybeSucc", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("positive", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.maybeSucc")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(6)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.maybeSucc")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.maybeSucc")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-5))))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-4)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("maxBound", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.maybeSucc")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2147483647))))))), hydra.show.Core.term(new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("addFloat64", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("positive numbers", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.addFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(5.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(8.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative numbers", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.addFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-3.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-5.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-8.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("mixed sign", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.addFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(10.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-3.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(7.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("with zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.addFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(42.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(42.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("fractional", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.addFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.5))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(4.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("mulFloat64", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("positive numbers", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(5.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(15.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative numbers", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-3.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-5.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(15.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("mixed sign", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(10.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-3.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-30.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("with zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(42.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("with one", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(42.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(42.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("fractional", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.5))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("negateFloat64", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("positive", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.negateFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(5.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-5.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.negateFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-5.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(5.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("fractional", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.negateFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.5)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("subFloat64", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("positive numbers", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.subFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(5.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative result", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.subFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(5.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-2.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative numbers", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.subFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-3.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-5.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("with zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.subFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(42.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(42.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("same value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.subFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(42.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(42.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("fractional", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.subFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.5))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("e", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("Euler's number", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
        (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
        hydra.Reduction.reduceTerm(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          true,
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.e")))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.71828182846)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("pi", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("pi constant", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
        (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
        hydra.Reduction.reduceTerm(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          true,
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pi")))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.14159265359)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("sin", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("sin 0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sin")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("sin pi/2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sin")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.5707963267948966))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("sin pi", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sin")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.141592653589793))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.22464679915e-16)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("sin 1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sin")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.841470984808)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("sin 0.5", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sin")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.5))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.479425538604)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("sin NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sin")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("sin +Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sin")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("sin -Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sin")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("cos", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("cos 0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cos")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("cos pi/2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cos")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.5707963267948966))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(6.12323399574e-17)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("cos pi", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cos")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.141592653589793))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("cos 1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cos")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.540302305868)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("cos 0.5", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cos")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.5))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.87758256189)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("cos NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cos")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("cos +Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cos")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("cos -Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cos")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("tan", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("tan 0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.tan")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("tan pi/4", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.tan")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.7853981633974483))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("tan 1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.tan")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.55740772465)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("tan 0.5", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.tan")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.5))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.546302489844)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("tan NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.tan")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("tan +Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.tan")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("tan -Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.tan")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("asin", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("asin 0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.asin")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("asin 1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.asin")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.57079632679)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("asin -1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.asin")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.57079632679)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("asin 0.5", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.asin")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.5))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.523598775598)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("asin below domain", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.asin")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-2.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("asin above domain", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.asin")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("asin NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.asin")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("asin +Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.asin")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("asin -Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.asin")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("acos", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("acos 1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.acos")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("acos 0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.acos")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.57079632679)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("acos -1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.acos")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.14159265359)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("acos 0.5", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.acos")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.5))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0471975512)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("acos below domain", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.acos")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-2.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("acos above domain", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.acos")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("acos NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.acos")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("acos +Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.acos")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("acos -Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.acos")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("atan", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("atan 0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atan")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atan 1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atan")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.785398163397)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atan 0.5", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atan")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.5))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.463647609001)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atan NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atan")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atan +Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atan")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.57079632679)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atan -Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atan")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.57079632679)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("atan2", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("atan2 1 1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atan2")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.785398163397)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atan2 1 0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atan2")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.57079632679)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atan2 0 1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atan2")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atan2 3 4", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atan2")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(4.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.643501108793)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atan2 NaN 1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atan2")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atan2 +Inf 1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atan2")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.57079632679)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atan2 -Inf 1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atan2")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.57079632679)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atan2 1 NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atan2")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atan2 1 +Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atan2")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atan2 1 -Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atan2")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.14159265359)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atan2 +Inf +Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atan2")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atan2 +Inf -Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atan2")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atan2 -Inf +Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atan2")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atan2 -Inf -Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atan2")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("sinh", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("sinh 0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sinh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("sinh 1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sinh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.17520119364)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("sinh 2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sinh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.62686040785)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("sinh NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sinh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("sinh +Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sinh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("sinh -Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sinh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("cosh", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("cosh 0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cosh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("cosh 1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cosh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.54308063482)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("cosh 2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cosh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.76219569108)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("cosh NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cosh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("cosh +Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cosh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("cosh -Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cosh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("tanh", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("tanh 0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.tanh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("tanh 1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.tanh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.761594155956)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("tanh 0.5", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.tanh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.5))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.46211715726)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("tanh NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.tanh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("tanh +Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.tanh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("tanh -Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.tanh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("asinh", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("asinh 0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.asinh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("asinh 1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.asinh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.88137358702)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("asinh 0.5", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.asinh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.5))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.48121182506)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("asinh NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.asinh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("asinh +Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.asinh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("asinh -Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.asinh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("acosh", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("acosh 1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.acosh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("acosh 2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.acosh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.31695789692)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("acosh 3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.acosh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.76274717404)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("acosh below domain", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.acosh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("acosh negative", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.acosh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("acosh NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.acosh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("acosh +Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.acosh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("acosh -Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.acosh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("atanh", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("atanh 0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atanh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atanh 0.5", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atanh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.5))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.549306144334)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atanh 0.1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atanh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.1))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.100335347731)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atanh upper boundary", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atanh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atanh lower boundary", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atanh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atanh above domain", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atanh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atanh below domain", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atanh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-2.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atanh NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atanh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atanh +Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atanh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("atanh -Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.atanh")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("exp", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("exp 0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.exp")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("exp 1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.exp")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.71828182846)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("exp -1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.exp")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.367879441171)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("exp 2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.exp")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(7.38905609893)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("exp 0.5", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.exp")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.5))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.6487212707)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("exp NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.exp")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("exp +Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.exp")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("exp -Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.exp")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("log", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("log 1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.log")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("log e", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.log")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.718281828459045))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("log 2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.log")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.69314718056)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("log 10", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.log")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(10.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.30258509299)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("log 0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.log")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("log negative", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.log")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("log NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.log")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("log +Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.log")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("log -Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.log")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("logBase", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("log10 1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.logBase")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(10.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("log10 10", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.logBase")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(10.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(10.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("log10 100", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.logBase")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(10.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(100.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("log2 8", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.logBase")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(8.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("log2 10", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.logBase")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(10.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.32192809489)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("logBase 10 0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.logBase")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(10.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("logBase 10 negative", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.logBase")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(10.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("logBase negative 10", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.logBase")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(10.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("logBase 10 NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.logBase")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(10.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("logBase 10 +Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.logBase")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(10.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("logBase 10 -Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.logBase")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(10.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("logBase NaN 10", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.logBase")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(10.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("logBase +Inf 10", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.logBase")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(10.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("logBase -Inf 10", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.logBase")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(10.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("pow", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("2^3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(8.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("10^0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(10.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("2^-1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.5)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("2^0.5", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.5))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.41421356237)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("0^0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("0^-1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("(-1)^0.5", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("NaN^2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("+Inf^2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("-Inf^2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("+Inf^-1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("2^NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("2^+Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("2^-Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("sqrt", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("sqrt 4", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sqrt")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(4.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("sqrt 9", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sqrt")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(9.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("sqrt 2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sqrt")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.4142135623730951)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("sqrt 0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sqrt")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("sqrt 3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sqrt")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.73205080757)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("sqrt negative", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sqrt")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("sqrt NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sqrt")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("sqrt +Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sqrt")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("sqrt -Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sqrt")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("ceiling", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("ceiling 3.2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.ceiling")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.2))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(4.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("ceiling 3.0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.ceiling")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("ceiling -3.2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.ceiling")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-3.2))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-3.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("ceiling -3.0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.ceiling")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-3.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-3.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("ceiling NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.ceiling")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("ceiling +Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.ceiling")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("ceiling -Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.ceiling")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("floor", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("floor 3.8", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.floor")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.8))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("floor 3.0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.floor")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("floor -3.2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.floor")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-3.2))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-4.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("floor -3.0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.floor")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-3.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-3.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("floor NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.floor")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("floor +Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.floor")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("floor -Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.floor")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("round", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("round 3.4", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.round")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.4))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("round 3.5", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.round")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(4.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("round 3.6", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.round")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.6))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(4.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("round -3.4", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.round")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-3.4))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-3.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("round -3.5", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.round")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-3.5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-4.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("round NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.round")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("round +Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.round")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("round -Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.round")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("roundBigfloat", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundBigfloat")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Bigfloat(new java.math.BigDecimal("0.0")))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Bigfloat(new java.math.BigDecimal("0.0"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("round pi to 4 digits", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundBigfloat")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(4))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Bigfloat(new java.math.BigDecimal("3.141592653589793")))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Bigfloat(new java.math.BigDecimal("3.142"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("round 1234.5 to 3 digits", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundBigfloat")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Bigfloat(new java.math.BigDecimal("1234.5")))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Bigfloat(new java.math.BigDecimal("1230.0"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("round 0.001234 to 2 digits", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundBigfloat")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Bigfloat(new java.math.BigDecimal("1.234e-3")))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Bigfloat(new java.math.BigDecimal("1.2e-3"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundBigfloat")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Bigfloat(new java.math.BigDecimal("-1234.5")))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Bigfloat(new java.math.BigDecimal("-1230.0"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("roundFloat32", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat32")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (0.0)))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (0.0))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("round pi to 4 digits", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat32")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(4))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (3.1415927410125732)))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (3.1419999599456787))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("round 1234.5 to 3 digits", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat32")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (1234.5)))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (1230.0))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat32")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (-1234.5)))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (-1230.0))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat32")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32(Float.NaN))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32(Float.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("+Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat32")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32(Float.POSITIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32(Float.POSITIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("-Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat32")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32(Float.NEGATIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32(Float.NEGATIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("roundFloat64", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("round pi to 4 digits", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(4))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.141592653589793))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.142)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("round pi to 10 digits", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.141592653589793))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.141592654)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("round 1234.5 to 3 digits", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1234.5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1230.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("round 0.001234 to 2 digits", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.234e-3))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.2e-3)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1234.5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1230.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("round 1 digit", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(9.876))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(10.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("+Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("-Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("truncate", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("truncate 3.8", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.truncate")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.8))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("truncate 3.2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.truncate")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.2))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("truncate -3.8", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.truncate")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-3.8))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-3.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("truncate -3.2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.truncate")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-3.2))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-3.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("truncate NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.truncate")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("truncate +Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.truncate")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("truncate -Inf", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.truncate")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }
}

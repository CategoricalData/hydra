// Note: this is an automatically generated file. Do not edit.

package hydra.test;

/**
 * Test cases for term reduction/evaluation mechanics
 */
public interface Reduction {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("reduction", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("beta reduction", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("identity function applied to literal", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x"))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("constant function", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("nested application", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("monomorphic primitives", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("toUpper on lowercase", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.toUpper"))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello")))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.String_("HELLO"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("toUpper on mixed case", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.toUpper"))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Hello World")))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.String_("HELLO WORLD"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("toUpper on empty string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.toUpper"))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("")))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.String_(""))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("toLower on uppercase", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.toLower"))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("HELLO")))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string length", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.length"))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello")))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string length of empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.length"))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("")))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("add two positive integers", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.math.add"))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(8)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("add negative and positive", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.math.add"))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-10))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-7)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("add with zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.math.add"))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("subtract integers", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.math.sub"))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(7)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("multiply integers", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.math.mul"))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(6))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(7))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("multiply by zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.math.mul"))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(100))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("divide integers", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.math.div"))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(20))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(4))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("modulo", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.math.mod"))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(17))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("splitOn basic", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.splitOn"))), new hydra.core.Term.Literal(new hydra.core.Literal.String_(",")))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("a,b,c")))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("a")),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("b")),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("c"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("cat2 strings", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.cat2"))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello")))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("world")))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.String_("helloworld"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("polymorphic primitives", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("length of integer list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.length"))), new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("length of string list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.length"))), new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("a")),
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("b")))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("length of empty list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.length"))), new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList())))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("length of single element list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.length"))), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("head of integer list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.head"))), new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(20))),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(30))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("head of string list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.head"))), new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("first")),
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("second")))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.String_("first"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("last of integer list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.last"))), new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(10))),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(20))),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(30))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(30)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("concat two integer lists", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.concat2"))), new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))), new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(4))))))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(4)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("concat with empty list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.concat2"))), new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList())))), new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("reverse integer list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.reverse"))), new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("reverse empty list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.lists.reverse"))), new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList())))))), hydra.show.Core.term(new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("nullary primitives", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("empty set has size zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
        (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
        hydra.Reduction.reduceTerm(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          true,
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.sets.size"))), new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.sets.empty"))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("literals as values", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("integer literal is a value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative integer literal", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-17))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(-17)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("zero integer literal", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string literal is a value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello")))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty string literal", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("")))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.String_(""))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string with special characters", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello\nworld\ttab")))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello\nworld\ttab"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("boolean true is a value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("boolean false is a value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false)))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("float literal is a value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.14))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.14)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("negative float literal", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-2.718))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-2.718)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("zero float literal", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("list reduction", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("empty list is a value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList())))), hydra.show.Core.term(new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("list of literals is a value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("list with reducible element", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x"))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("optional reduction", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("nothing is a value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())))), hydra.show.Core.term(new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("just literal is a value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("just with reducible content", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x"))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("alpha conversion", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("variable at top level", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Reduction.alphaConvert(
          new hydra.core.Name("x"),
          new hydra.core.Name("y"),
          new hydra.core.Term.Variable(new hydra.core.Name("x")))), hydra.show.Core.term(new hydra.core.Term.Variable(new hydra.core.Name("y"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("variable in list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Reduction.alphaConvert(
          new hydra.core.Name("x"),
          new hydra.core.Name("y"),
          new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))),
            new hydra.core.Term.Variable(new hydra.core.Name("x")))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))),
          new hydra.core.Term.Variable(new hydra.core.Name("y"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("lambda with different variable is transparent", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Reduction.alphaConvert(
          new hydra.core.Name("x"),
          new hydra.core.Name("y"),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("z"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))),
            new hydra.core.Term.Variable(new hydra.core.Name("x")),
            new hydra.core.Term.Variable(new hydra.core.Name("z"))))))))), hydra.show.Core.term(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("z"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))),
          new hydra.core.Term.Variable(new hydra.core.Name("y")),
          new hydra.core.Term.Variable(new hydra.core.Name("z")))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("lambda with same variable is opaque", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Reduction.alphaConvert(
          new hydra.core.Name("x"),
          new hydra.core.Name("y"),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))),
            new hydra.core.Term.Variable(new hydra.core.Name("x")),
            new hydra.core.Term.Variable(new hydra.core.Name("z"))))))))), hydra.show.Core.term(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))),
          new hydra.core.Term.Variable(new hydra.core.Name("x")),
          new hydra.core.Term.Variable(new hydra.core.Name("z")))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("nested lambda outer variable", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Reduction.alphaConvert(
          new hydra.core.Name("x"),
          new hydra.core.Name("y"),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("a"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("b"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.term(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("a"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("b"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("y"))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("nested lambda shadows outer", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Reduction.alphaConvert(
          new hydra.core.Name("x"),
          new hydra.core.Name("z"),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.term(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("application with variable", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Reduction.alphaConvert(
          new hydra.core.Name("x"),
          new hydra.core.Name("y"),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), hydra.show.Core.term(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Variable(new hydra.core.Name("y"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("application with both variables same", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Reduction.alphaConvert(
          new hydra.core.Name("x"),
          new hydra.core.Name("y"),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("x")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), hydra.show.Core.term(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("y")), new hydra.core.Term.Variable(new hydra.core.Name("y"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("type reduction", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("unit type unchanged", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<type reduction error>>"),
          (java.util.function.Function<hydra.core.Type, String>) (t -> hydra.show.Core.type(t)),
          hydra.Reduction.betaReduceType(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Type.Unit())), hydra.show.Core.type(new hydra.core.Type.Unit()))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string type unchanged", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<type reduction error>>"),
          (java.util.function.Function<hydra.core.Type, String>) (t -> hydra.show.Core.type(t)),
          hydra.Reduction.betaReduceType(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))), hydra.show.Core.type(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("int32 type unchanged", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<type reduction error>>"),
          (java.util.function.Function<hydra.core.Type, String>) (t -> hydra.show.Core.type(t)),
          hydra.Reduction.betaReduceType(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))), hydra.show.Core.type(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("identity type applied to string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<type reduction error>>"),
          (java.util.function.Function<hydra.core.Type, String>) (t -> hydra.show.Core.type(t)),
          hydra.Reduction.betaReduceType(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("t"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("t")), new hydra.core.Type.Variable(new hydra.core.Name("t")))))), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("constant type ignores argument", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<type reduction error>>"),
          (java.util.function.Function<hydra.core.Type, String>) (t -> hydra.show.Core.type(t)),
          hydra.Reduction.betaReduceType(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("x"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))), new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()))))), hydra.show.Core.type(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("nested forall first application", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<type reduction error>>"),
          (java.util.function.Function<hydra.core.Type, String>) (t -> hydra.show.Core.type(t)),
          hydra.Reduction.betaReduceType(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("x"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("y"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("x")), new hydra.core.Type.Variable(new hydra.core.Name("y")))))))), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))), hydra.show.Core.type(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("y"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Variable(new hydra.core.Name("y"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("nested forall both applications", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<type reduction error>>"),
          (java.util.function.Function<hydra.core.Type, String>) (t -> hydra.show.Core.type(t)),
          hydra.Reduction.betaReduceType(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("x"), new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("y"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("x")), new hydra.core.Type.Variable(new hydra.core.Name("y")))))))), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))), hydra.show.Core.type(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("list type applied", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<type reduction error>>"),
          (java.util.function.Function<hydra.core.Type, String>) (t -> hydra.show.Core.type(t)),
          hydra.Reduction.betaReduceType(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("a"), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("a"))))), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))), hydra.show.Core.type(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("optional type applied", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (e -> "<<type reduction error>>"),
          (java.util.function.Function<hydra.core.Type, String>) (t -> hydra.show.Core.type(t)),
          hydra.Reduction.betaReduceType(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Type.Application(new hydra.core.ApplicationType(new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("a"), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("a"))))), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))), hydra.show.Core.type(new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("etaExpandTerm", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("integer literal unchanged", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (ignored -> "eta expansion failed"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.etaExpandTypedTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string list unchanged", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (ignored -> "eta expansion failed"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.etaExpandTypedTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")),
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("fully applied binary function unchanged", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (ignored -> "eta expansion failed"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.etaExpandTypedTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.splitOn"))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")))))), hydra.show.Core.term(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.splitOn"))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("lambda with fully applied primitive unchanged", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (ignored -> "eta expansion failed"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.etaExpandTypedTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.splitOn"))), new hydra.core.Term.Literal(new hydra.core.Literal.String_(",")))), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))), hydra.show.Core.term(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.splitOn"))), new hydra.core.Term.Literal(new hydra.core.Literal.String_(",")))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("lambda returning constant unchanged", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (ignored -> "eta expansion failed"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.etaExpandTypedTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))))), hydra.show.Core.term(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("bare unary primitive unchanged", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (ignored -> "eta expansion failed"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.etaExpandTypedTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.toLower"))))), hydra.show.Core.term(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.toLower")))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("bare binary primitive unchanged", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (ignored -> "eta expansion failed"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.etaExpandTypedTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.splitOn"))))), hydra.show.Core.term(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.splitOn")))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("partially applied binary primitive expands to one lambda", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (ignored -> "eta expansion failed"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.etaExpandTypedTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.splitOn"))), new hydra.core.Term.Variable(new hydra.core.Name("foo")))))), hydra.show.Core.term(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v1"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.splitOn"))), new hydra.core.Term.Variable(new hydra.core.Name("foo")))), new hydra.core.Term.Variable(new hydra.core.Name("v1")))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("projection expands to lambda", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (ignored -> "eta expansion failed"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.etaExpandTypedTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("Person"), new hydra.core.Name("firstName"))))))), hydra.show.Core.term(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v1"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("Person"), new hydra.core.Name("firstName"))))), new hydra.core.Term.Variable(new hydra.core.Name("v1")))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("partial application inside lambda expands", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (ignored -> "eta expansion failed"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.etaExpandTypedTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.splitOn"))), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))), hydra.show.Core.term(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v1"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.splitOn"))), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.Variable(new hydra.core.Name("v1"))))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("let with constant body unchanged", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (ignored -> "eta expansion failed"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.etaExpandTypedTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("foo"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(137))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), hydra.show.Core.term(new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("foo"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(137))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("let with bare primitive value unchanged", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (ignored -> "eta expansion failed"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.etaExpandTypedTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("foo"), new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.splitOn"))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("foo")))))), hydra.show.Core.term(new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("foo"), new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.splitOn"))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("foo"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("fully applied unary unchanged", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (ignored -> "eta expansion failed"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.etaExpandTypedTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.toLower"))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("FOO")))))), hydra.show.Core.term(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.toLower"))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("FOO"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("partial application in list expands", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (ignored -> "eta expansion failed"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.etaExpandTypedTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))))))),
              new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.splitOn"))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")))))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))))))),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("v1"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.strings.splitOn"))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")))), new hydra.core.Term.Variable(new hydra.core.Name("v1")))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }
}

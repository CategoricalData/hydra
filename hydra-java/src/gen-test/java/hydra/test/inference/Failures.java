// Note: this is an automatically generated file. Do not edit.

package hydra.test.inference;

/**
 * Inference tests for expected failures
 */
public interface Failures {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("Expected failures", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      hydra.test.inference.Failures.undefinedVariableTests(),
      hydra.test.inference.Failures.unificationFailureTests(),
      hydra.test.inference.Failures.invalidApplicationTests(),
      hydra.test.inference.Failures.selfApplicationTests(),
      hydra.test.inference.Failures.arityMismatchTests(),
      hydra.test.inference.Failures.recursiveTypeTests(),
      hydra.test.inference.Failures.occurCheckTests(),
      hydra.test.inference.Failures.typeConstructorMisuseTests(),
      hydra.test.inference.Failures.polymorphismViolationTests(),
      hydra.test.inference.Failures.letBindingMismatchTests(),
      hydra.test.inference.Failures.constraintSolverEdgeCaseTests(),
      hydra.test.inference.Failures.primitiveTypeErrorTests(),
      hydra.test.inference.Failures.complexConstraintFailureTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup arityMismatchTests() {
    return new hydra.testing.TestGroup("Arity mismatch", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("Too many arguments", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(137))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(999))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.cons")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(137))))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("extra")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Wrong argument types with extra args", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.strings.length")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("extra")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.logic.not")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("arg")))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(137))))), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup complexConstraintFailureTests() {
    return new hydra.testing.TestGroup("Complex constraint failures", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("Multi-level constraint conflicts", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("f"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Variable(new hydra.core.Name("x")), new hydra.core.Term.Variable(new hydra.core.Name("y")))))))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("g"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("a"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Variable(new hydra.core.Name("a")))), new hydra.core.Term.Variable(new hydra.core.Name("a"))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("h"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("g")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("z"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("z"))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("h")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("weird"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("bad"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("weird")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Variable(new hydra.core.Name("y")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("bad")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("nested"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("g"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("g")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("g")), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))))))))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("int_f"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("n"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")), new hydra.core.Term.Variable(new hydra.core.Name("n")))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("str_g"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("s"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.strings.cat")), new hydra.core.Term.List(java.util.Arrays.asList(
                new hydra.core.Term.Variable(new hydra.core.Name("s")),
                new hydra.core.Term.Literal(new hydra.core.Literal.String_("!"))))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("bad"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("nested")), new hydra.core.Term.Variable(new hydra.core.Name("int_f")))), new hydra.core.Term.Variable(new hydra.core.Name("str_g")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("bad")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Function composition failures", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("triple"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("increment"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("n"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")), new hydra.core.Term.Variable(new hydra.core.Name("n")))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1)))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("stringify"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("s"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.strings.cat")), new hydra.core.Term.List(java.util.Arrays.asList(
                new hydra.core.Term.Variable(new hydra.core.Name("s")),
                new hydra.core.Term.Literal(new hydra.core.Literal.String_("!"))))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("bad"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("triple")), new hydra.core.Term.Variable(new hydra.core.Name("increment")))), new hydra.core.Term.Variable(new hydra.core.Name("stringify")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("bad")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("compose"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("g"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("g")), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("reverse_compose"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("g"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("g")), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("bad"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("compose")), new hydra.core.Term.Variable(new hydra.core.Name("reverse_compose")))), new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")))), new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.strings.length")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("bad")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup constraintSolverEdgeCaseTests() {
    return new hydra.testing.TestGroup("Constraint solver edge cases", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("Complex constraint propagation", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
          "unexpected: ",
          hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
            new hydra.core.Binding(new hydra.core.Name("complex"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("g"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("g")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("g")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))))))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
            new hydra.core.Binding(new hydra.core.Name("bad"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("complex")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("a"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("b"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("bad")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Fixed point combinators", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("fix"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Variable(new hydra.core.Name("f"))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("bad"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("fix")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("x")), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("bad")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("y"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("x")), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("x")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("bad"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("y")), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("rec"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("n"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("rec")), new hydra.core.Term.Variable(new hydra.core.Name("rec")))))))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("bad")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("omega"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("x")), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("bad"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("omega")), new hydra.core.Term.Variable(new hydra.core.Name("omega")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("bad")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Constraint cycles", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("a"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("b")), new hydra.core.Term.Variable(new hydra.core.Name("c")))), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("b"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("c")), new hydra.core.Term.Variable(new hydra.core.Name("a")))), new hydra.core.Term.Variable(new hydra.core.Name("y"))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("c"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("z"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("a")), new hydra.core.Term.Variable(new hydra.core.Name("b")))), new hydra.core.Term.Variable(new hydra.core.Name("z"))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("a")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("circular"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Variable(new hydra.core.Name("circular")))), new hydra.core.Term.Variable(new hydra.core.Name("f"))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("circular")), new hydra.core.Term.Variable(new hydra.core.Name("circular")))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup invalidApplicationTests() {
    return new hydra.testing.TestGroup("Invalid application", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("Non-function application", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(137))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false)))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#4", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.14))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Collection application", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList())), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#4", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))))))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("index")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Primitive misapplication", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.empty")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.empty")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())), new hydra.core.Term.Literal(new hydra.core.Literal.String_("value")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#4", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList())), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup letBindingMismatchTests() {
    return new hydra.testing.TestGroup("Let binding type mismatches", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("Application type mismatches", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("y"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("x")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("y")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("f"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.String_("result"))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("g"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("extra")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("g")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("num"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("bad"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("num")), new hydra.core.Term.Variable(new hydra.core.Name("num")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("bad")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Collection type mismatches", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("list1"), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("list2"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.cons")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))), new hydra.core.Term.Variable(new hydra.core.Name("list1")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("list2")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("nums"), new hydra.core.Term.List(java.util.Arrays.asList(
                new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
                new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("mixed"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.cons")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bad")))), new hydra.core.Term.Variable(new hydra.core.Name("nums")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("mixed")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("pair1"), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("pair2"), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")), new hydra.core.Term.Variable(new hydra.core.Name("pair1")))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.pairs.first")), new hydra.core.Term.Variable(new hydra.core.Name("pair2")))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Function binding mismatches", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("add"), new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("badCall"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("add")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("not a number")))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("badCall")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("f"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("g"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("bad"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("g")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("bad")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup occurCheckTests() {
    return new hydra.testing.TestGroup("Occur check failures", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("Function occur checks", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
        (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
          "unexpected: ",
          hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
        hydra.Inference.inferTypeOf(
          hydra.test.TestGraph.testContext(),
          hydra.test.TestGraph.testGraph(),
          new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("g"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("h"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("g")), new hydra.core.Term.Variable(new hydra.core.Name("g")))), new hydra.core.Term.Variable(new hydra.core.Name("h"))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("g")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Mutual occur checks", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("f"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("g")), new hydra.core.Term.Variable(new hydra.core.Name("f"))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("g"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Variable(new hydra.core.Name("g"))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("f")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("a"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("b")), new hydra.core.Term.Variable(new hydra.core.Name("a")))), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("b"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("a")), new hydra.core.Term.Variable(new hydra.core.Name("b"))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("a")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("cycle1"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("cycle2")), new hydra.core.Term.Variable(new hydra.core.Name("cycle1")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("cycle2"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("cycle1")), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("cycle1")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Complex occur checks", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("omega"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("x")), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.Variable(new hydra.core.Name("omega"))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("omega")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("loop"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("loop")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("x")), new hydra.core.Term.Variable(new hydra.core.Name("loop")))))), new hydra.core.Term.Variable(new hydra.core.Name("y")))))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("loop")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup polymorphismViolationTests() {
    return new hydra.testing.TestGroup("Polymorphism violations", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("Identity function violations", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("id"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x"))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("id")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("id")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("id"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x"))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("id")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))),
              new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("id")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("id"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x"))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("id")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("id")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))))), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Constrained polymorphism violations", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("f"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Variable(new hydra.core.Name("x")),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("g"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Variable(new hydra.core.Name("x")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("constant"))))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.pairs.first")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("g")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.pairs.first")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("g")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bad")))))))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("h"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.cons")), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(0)))))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("h")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("incompatible")))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Higher-order polymorphism violations", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo"))))))))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("g"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("g")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))))),
              new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("g")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bad"))))))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("h"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("h")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("h")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("error"))))))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup primitiveTypeErrorTests() {
    return new hydra.testing.TestGroup("Primitive function type errors", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("Logic primitive errors", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.logic.ifElse")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false)))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.logic.ifElse")), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false)))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.logic.and")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#4", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.logic.or")), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("not boolean")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Collection primitive errors", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maps.lookup")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("not a map")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.sets.member")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.head")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("not a list")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#4", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.maybes.fromMaybe")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("not optional")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Math primitive errors", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("not a number")))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mul")), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false)))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.div")), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#4", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mod")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("not a number")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup recursiveTypeTests() {
    return new hydra.testing.TestGroup("Recursive type construction", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("Direct recursive types", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Variable(new hydra.core.Name("x")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Variable(new hydra.core.Name("x")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Variable(new hydra.core.Name("x")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Recursive function types", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("f"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("f"))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("f")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("f"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Variable(new hydra.core.Name("f")))))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("f")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("f"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Variable(new hydra.core.Name("f"))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("f")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Mutually recursive types", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Variable(new hydra.core.Name("y")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("y"), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Variable(new hydra.core.Name("x")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("a"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("b"))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("b"), new hydra.core.Term.Variable(new hydra.core.Name("a")), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("a")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("f"), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Variable(new hydra.core.Name("g")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("g"), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Variable(new hydra.core.Name("f")))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("f")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup selfApplicationTests() {
    return new hydra.testing.TestGroup("Self-application", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("Direct self-application", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("x")), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("f"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Variable(new hydra.core.Name("f")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("f")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Indirect self-application", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("f"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("g")), new hydra.core.Term.Variable(new hydra.core.Name("f"))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("g"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("y")), new hydra.core.Term.Variable(new hydra.core.Name("y"))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("f")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("a"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("b")), new hydra.core.Term.Variable(new hydra.core.Name("a")))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("b"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("x")), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("a")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("cycle"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("f"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Variable(new hydra.core.Name("cycle"))))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("cycle")), new hydra.core.Term.Variable(new hydra.core.Name("cycle")))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup typeConstructorMisuseTests() {
    return new hydra.testing.TestGroup("Type constructor misuse", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("List constructor errors", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.cons")), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(137))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.length")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.head")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("not a list")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#4", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.tail")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("String constructor errors", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.strings.length")), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.strings.cat")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.strings.fromList")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("not a list")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#4", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.strings.toList")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Math constructor errors", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(137))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sub")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("not a number")))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mul")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("not a number")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#4", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.div")), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false)))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup undefinedVariableTests() {
    return new hydra.testing.TestGroup("Undefined variable", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("Basic unbound variables", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Variable(new hydra.core.Name("x")))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("y"))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("y")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Unbound in let expressions", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Variable(new hydra.core.Name("y")), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Variable(new hydra.core.Name("y")), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("z"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(
              new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing())),
              new hydra.core.Binding(new hydra.core.Name("y"), new hydra.core.Term.Variable(new hydra.core.Name("z")), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Variable(new hydra.core.Name("x")), new hydra.core.Term.Variable(new hydra.core.Name("y")))))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Shadowing scope errors", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("y"), new hydra.core.Term.Variable(new hydra.core.Name("x")), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("z"))))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("y"), new hydra.core.Term.Variable(new hydra.core.Name("x")), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("z")))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("x"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("z"))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup unificationFailureTests() {
    return new hydra.testing.TestGroup("Unification failure", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("Basic type mismatches", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))),
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))),
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#4", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("bar")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Collection type mismatches", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.cons")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("not a list")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))),
              new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(137))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#4", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.concat")), new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))),
              new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Conditional type mismatches", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.logic.ifElse")), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.logic.ifElse")), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))), new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.logic.ifElse")), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("Polymorphic instantiation conflicts", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("#1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("f"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x"))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))),
              new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("f")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("id"), new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x"))))), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("id")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("id")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))))), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("#3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "FAIL"),
          (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.Term, hydra.core.TypeScheme>, hydra.context.Context>, String>) (result -> hydra.lib.strings.Cat2.apply(
            "unexpected: ",
            hydra.show.Core.typeScheme(hydra.lib.pairs.Second.apply(hydra.lib.pairs.First.apply(result))))),
          hydra.Inference.inferTypeOf(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            new hydra.core.Term.Let(new hydra.core.Let(java.util.Arrays.asList(new hydra.core.Binding(new hydra.core.Name("cons"), new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.lists.cons")), (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))), new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("cons")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))),
              new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("cons")), new hydra.core.Term.Literal(new hydra.core.Literal.String_("foo")))))))))), "FAIL")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }
}

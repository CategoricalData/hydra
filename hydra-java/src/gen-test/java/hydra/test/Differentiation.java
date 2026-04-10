// Note: this is an automatically generated file. Do not edit.

package hydra.test;

/**
 * Test cases for automatic differentiation
 */
public interface Differentiation {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("differentiation", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("basic rules", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("variable wrt itself", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Differentiation.differentiateTerm(
          new hydra.core.Name("x"),
          new hydra.core.Term.Variable(new hydra.core.Name("x")))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("variable wrt different variable", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Differentiation.differentiateTerm(
          new hydra.core.Name("x"),
          new hydra.core.Term.Variable(new hydra.core.Name("y")))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("float64 literal", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Differentiation.differentiateTerm(
          new hydra.core.Name("x"),
          new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(42.0))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("zero literal", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Differentiation.differentiateTerm(
          new hydra.core.Name("x"),
          new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("identity function", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Differentiation.differentiateFunction(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))), hydra.show.Core.term(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("constant function", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Differentiation.differentiateFunction(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(5.0)))))))), hydra.show.Core.term(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("lambda binding different variable", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Differentiation.differentiateTerm(
          new hydra.core.Name("x"),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))), hydra.show.Core.term(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("y"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("lambda shadowing differentiation variable", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Differentiation.differentiateTerm(
          new hydra.core.Name("x"),
          new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Variable(new hydra.core.Name("x"))))))), hydra.show.Core.term(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("unary primitives", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("sin of variable", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Differentiation.differentiateTerm(
          new hydra.core.Name("x"),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sin")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), hydra.show.Core.term(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cos")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("exp of variable", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Differentiation.differentiateTerm(
          new hydra.core.Name("x"),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.exp")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), hydra.show.Core.term(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.exp")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("sin of different variable", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Differentiation.differentiateTerm(
          new hydra.core.Name("x"),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sin")), new hydra.core.Term.Variable(new hydra.core.Name("y")))))), hydra.show.Core.term(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cos")), new hydra.core.Term.Variable(new hydra.core.Name("y")))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("binary primitives", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("add variable to itself", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Differentiation.differentiateTerm(
          new hydra.core.Name("x"),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), hydra.show.Core.term(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.addFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("multiply variable by itself", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Differentiation.differentiateTerm(
          new hydra.core.Name("x"),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mul")), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), hydra.show.Core.term(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.addFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("subtract constant from variable", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Differentiation.differentiateTerm(
          new hydra.core.Name("x"),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sub")), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(5.0))))))), hydra.show.Core.term(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.subFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("chain rule", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("nested sin(cos(x))", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Differentiation.differentiateTerm(
        new hydra.core.Name("x"),
        new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sin")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cos")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))), hydra.show.Core.term(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cos")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cos")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Lambda(new hydra.core.Lambda(new hydra.core.Name("_x"), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.negateFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sin")), new hydra.core.Term.Variable(new hydra.core.Name("_x"))))))))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("structural", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("list of terms", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Differentiation.differentiateTerm(
          new hydra.core.Name("x"),
          new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Variable(new hydra.core.Name("x")),
            new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(5.0))))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("pair of terms", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Differentiation.differentiateTerm(
          new hydra.core.Name("x"),
          new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Variable(new hydra.core.Name("x")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(5.0))))))))), hydra.show.Core.term(new hydra.core.Term.Pair((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("unit term", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Differentiation.differentiateTerm(
          new hydra.core.Name("x"),
          new hydra.core.Term.Unit())), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("evaluate derivatives", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("d/dx(x) at 3.0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.0))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Variable(new hydra.core.Name("x")))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("d/dx(42.0) at 3.0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.0))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(42.0))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("d/dx(x+x) at 3.0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.0))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.add")), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("d/dx(x*x) at 3.0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.0))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mul")), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(6.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("d/dx(x*x) at 5.0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(5.0))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mul")), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(10.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("d/dx(x-5) at 7.0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(7.0))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sub")), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(5.0))))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("d/dx(sin(x)) at 0.0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sin")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("d/dx(sin(x)) at pi/2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.5707963267948966))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sin")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(6.12323399574e-17)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("d/dx(cos(x)) at 1.0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cos")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-0.841470984808)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("d/dx(cos(x)) at pi", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.141592653589793))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cos")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-1.22464679915e-16)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("d/dx(exp(x)) at 0.0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.exp")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("d/dx(exp(x)) at 1.0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.exp")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.71828182846)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("d/dx(log(x)) at 1.0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.log")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("d/dx(log(x)) at e", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.718281828459045))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.log")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.367879441171)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("d/dx(sqrt(x)) at 4.0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(4.0))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sqrt")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.25)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("d/dx(x^3) at 2.0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.0))))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(12.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("d/dx(sin(cos(x))) at 0.5", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.5))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sin")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cos")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-0.30635890919)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("d/dx(sin(cos(x))) at 1.0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sin")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cos")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-0.721606149063)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("d/dx(exp(sin(x))) at 0.0", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.exp")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sin")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("d/dx(x*sin(x)) at pi", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(12))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.141592653589793))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mul")), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sin")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-3.14159265359)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("gradient check", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("x^2", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mul")), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(4.0)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("sin(x)", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sin")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.5403)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("exp(x)", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.5))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.exp")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.6487)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("x^3", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.5))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.pow")), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.0))))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(6.75)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("log(x)", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.log")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.5)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("cos(x)", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cos")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-0.84147)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("sin(cos(x))", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sin")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.cos")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(-0.72161)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("x*sin(x)", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.Error_, String>) (e -> "<<eval error>>"),
          (java.util.function.Function<hydra.core.Term, String>) (t -> hydra.show.Core.term(t)),
          hydra.Reduction.reduceTerm(
            hydra.test.TestGraph.testContext(),
            hydra.test.TestGraph.testGraph(),
            true,
            new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.roundFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(5))))), hydra.Variables.replaceFreeTermVariable(
              new hydra.core.Name("x"),
              new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(2.0))),
              hydra.Differentiation.differentiateTerm(
                new hydra.core.Name("x"),
                new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mul")), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.sin")), new hydra.core.Term.Variable(new hydra.core.Name("x")))))))))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(7.7004e-2)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("gradient", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("add two variables", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Differentiation.gradient(
          new hydra.core.Name("Gradient"),
          java.util.Arrays.asList(
            new hydra.core.Name("x"),
            new hydra.core.Name("y")),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.addFloat64")), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.Variable(new hydra.core.Name("y")))))), hydra.show.Core.term(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Gradient"), java.util.Arrays.asList(
          new hydra.core.Field(new hydra.core.Name("x"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.addFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))),
          new hydra.core.Field(new hydra.core.Name("y"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.addFloat64")), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("product of two variables", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Differentiation.gradient(
          new hydra.core.Name("Gradient"),
          java.util.Arrays.asList(
            new hydra.core.Name("x"),
            new hydra.core.Name("y")),
          new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.Variable(new hydra.core.Name("y")))))), hydra.show.Core.term(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Gradient"), java.util.Arrays.asList(
          new hydra.core.Field(new hydra.core.Name("x"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.addFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Variable(new hydra.core.Name("y")))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))))))),
          new hydra.core.Field(new hydra.core.Name("y"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.addFloat64")), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Variable(new hydra.core.Name("x")))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0))))))), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Variable(new hydra.core.Name("hydra.lib.math.mulFloat64")), new hydra.core.Term.Variable(new hydra.core.Name("y")))), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("single variable partial", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Differentiation.gradient(
          new hydra.core.Name("Gradient"),
          java.util.Arrays.asList(
            new hydra.core.Name("x"),
            new hydra.core.Name("y")),
          new hydra.core.Term.Variable(new hydra.core.Name("x")))), hydra.show.Core.term(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Gradient"), java.util.Arrays.asList(
          new hydra.core.Field(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(1.0)))),
          new hydra.core.Field(new hydra.core.Name("y"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("constant", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.term(hydra.Differentiation.gradient(
          new hydra.core.Name("Gradient"),
          java.util.Arrays.asList(
            new hydra.core.Name("x"),
            new hydra.core.Name("y")),
          new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(7.0))))), hydra.show.Core.term(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("Gradient"), java.util.Arrays.asList(
          new hydra.core.Field(new hydra.core.Name("x"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))),
          new hydra.core.Field(new hydra.core.Name("y"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(0.0)))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }
}

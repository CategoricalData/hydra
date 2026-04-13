// Note: this is an automatically generated file. Do not edit.

package hydra.test;

/**
 * Test cases for type unification operations
 */
public interface Unification {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("unification", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("variableOccursInType", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("variable occurs in itself", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.Unification.variableOccursInType(
          new hydra.core.Name("a"),
          new hydra.core.Type.Variable(new hydra.core.Name("a")))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("variable does not occur in different variable", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.Unification.variableOccursInType(
          new hydra.core.Name("a"),
          new hydra.core.Type.Variable(new hydra.core.Name("b")))), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("variable does not occur in int32", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.Unification.variableOccursInType(
          new hydra.core.Name("a"),
          new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("variable does not occur in string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.Unification.variableOccursInType(
          new hydra.core.Name("a"),
          new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("variable occurs in list element type", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.Unification.variableOccursInType(
          new hydra.core.Name("a"),
          new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("a"))))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("variable does not occur in list of different type", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.Unification.variableOccursInType(
          new hydra.core.Name("a"),
          new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("b"))))), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("variable occurs in function domain", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.Unification.variableOccursInType(
          new hydra.core.Name("a"),
          new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("a")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("variable occurs in function codomain", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.Unification.variableOccursInType(
          new hydra.core.Name("a"),
          new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Variable(new hydra.core.Name("a")))))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("variable does not occur in function with different vars", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.Unification.variableOccursInType(
          new hydra.core.Name("a"),
          new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("b")), new hydra.core.Type.Variable(new hydra.core.Name("c")))))), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("variable occurs in optional type", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.Unification.variableOccursInType(
          new hydra.core.Name("a"),
          new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("a"))))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("variable occurs in pair first", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.Unification.variableOccursInType(
          new hydra.core.Name("a"),
          new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Variable(new hydra.core.Name("a")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("variable occurs in pair second", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.Unification.variableOccursInType(
          new hydra.core.Name("a"),
          new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Variable(new hydra.core.Name("a")))))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("variable occurs in either left", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.Unification.variableOccursInType(
          new hydra.core.Name("a"),
          new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Variable(new hydra.core.Name("a")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("variable occurs in either right", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.Unification.variableOccursInType(
          new hydra.core.Name("a"),
          new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Variable(new hydra.core.Name("a")))))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("variable occurs in map key type", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.Unification.variableOccursInType(
          new hydra.core.Name("a"),
          new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("a")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("variable occurs in map value type", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.Unification.variableOccursInType(
          new hydra.core.Name("a"),
          new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Variable(new hydra.core.Name("a")))))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("variable occurs in set type", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.Unification.variableOccursInType(
          new hydra.core.Name("a"),
          new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("a"))))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("variable occurs in nested list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.Unification.variableOccursInType(
          new hydra.core.Name("a"),
          new hydra.core.Type.List(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("a")))))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("variable occurs in list of functions", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.Unification.variableOccursInType(
          new hydra.core.Name("a"),
          new hydra.core.Type.List(new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Variable(new hydra.core.Name("a"))))))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("variable does not occur in complex type without it", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.Unification.variableOccursInType(
          new hydra.core.Name("a"),
          new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))), new hydra.core.Type.Maybe(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Variable(new hydra.core.Name("b"))))))))), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("variable occurs deep in complex type", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.Unification.variableOccursInType(
          new hydra.core.Name("a"),
          new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))), new hydra.core.Type.Maybe(new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Variable(new hydra.core.Name("a"))))))))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("variable occurs in forAll body", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.Unification.variableOccursInType(
          new hydra.core.Name("a"),
          new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("b"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("b")), new hydra.core.Type.Variable(new hydra.core.Name("a")))))))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("variable occurs in forAll bound position", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.Unification.variableOccursInType(
          new hydra.core.Name("a"),
          new hydra.core.Type.Forall(new hydra.core.ForallType(new hydra.core.Name("a"), new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("a")), new hydra.core.Type.Variable(new hydra.core.Name("a")))))))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("unifyTypes", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("unify identical int32 types", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<hydra.typing.TypeSubst, String>) (ts -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "{",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  hydra.lib.pairs.First.apply(p).value,
                  ": ",
                  hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
                hydra.lib.maps.ToList.apply((ts).value))),
            "}"))),
          hydra.Unification.unifyTypes(
            hydra.Lexical.emptyContext(),
            hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (n -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>(n, new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(n), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))),
              (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()))),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "{",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                hydra.lib.pairs.First.apply(p).value,
                ": ",
                hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
              hydra.lib.maps.ToList.apply(new hydra.typing.TypeSubst((java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (java.util.Collections.<hydra.core.Name, hydra.core.Type>emptyMap()))).value))),
          "}")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("unify identical string types", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<hydra.typing.TypeSubst, String>) (ts -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "{",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  hydra.lib.pairs.First.apply(p).value,
                  ": ",
                  hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
                hydra.lib.maps.ToList.apply((ts).value))),
            "}"))),
          hydra.Unification.unifyTypes(
            hydra.Lexical.emptyContext(),
            hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (n -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>(n, new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(n), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))),
              (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()))),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "{",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                hydra.lib.pairs.First.apply(p).value,
                ": ",
                hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
              hydra.lib.maps.ToList.apply(new hydra.typing.TypeSubst((java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (java.util.Collections.<hydra.core.Name, hydra.core.Type>emptyMap()))).value))),
          "}")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("unify identical variable types", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<hydra.typing.TypeSubst, String>) (ts -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "{",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  hydra.lib.pairs.First.apply(p).value,
                  ": ",
                  hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
                hydra.lib.maps.ToList.apply((ts).value))),
            "}"))),
          hydra.Unification.unifyTypes(
            hydra.Lexical.emptyContext(),
            hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (n -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>(n, new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(n), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))),
              (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()))),
            new hydra.core.Type.Variable(new hydra.core.Name("a")),
            new hydra.core.Type.Variable(new hydra.core.Name("a")),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "{",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                hydra.lib.pairs.First.apply(p).value,
                ": ",
                hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
              hydra.lib.maps.ToList.apply(new hydra.typing.TypeSubst((java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (java.util.Collections.<hydra.core.Name, hydra.core.Type>emptyMap()))).value))),
          "}")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("unify variable with int32", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<hydra.typing.TypeSubst, String>) (ts -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "{",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  hydra.lib.pairs.First.apply(p).value,
                  ": ",
                  hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
                hydra.lib.maps.ToList.apply((ts).value))),
            "}"))),
          hydra.Unification.unifyTypes(
            hydra.Lexical.emptyContext(),
            hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (n -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>(n, new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(n), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))),
              (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()))),
            new hydra.core.Type.Variable(new hydra.core.Name("a")),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "{",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                hydra.lib.pairs.First.apply(p).value,
                ": ",
                hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
              hydra.lib.maps.ToList.apply(new hydra.typing.TypeSubst(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
                new hydra.core.Name("a"),
                new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))).value))),
          "}")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("unify int32 with variable", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<hydra.typing.TypeSubst, String>) (ts -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "{",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  hydra.lib.pairs.First.apply(p).value,
                  ": ",
                  hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
                hydra.lib.maps.ToList.apply((ts).value))),
            "}"))),
          hydra.Unification.unifyTypes(
            hydra.Lexical.emptyContext(),
            hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (n -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>(n, new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(n), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))),
              (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()))),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())),
            new hydra.core.Type.Variable(new hydra.core.Name("a")),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "{",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                hydra.lib.pairs.First.apply(p).value,
                ": ",
                hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
              hydra.lib.maps.ToList.apply(new hydra.typing.TypeSubst(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
                new hydra.core.Name("a"),
                new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))).value))),
          "}")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("unify two different variables", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<hydra.typing.TypeSubst, String>) (ts -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "{",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  hydra.lib.pairs.First.apply(p).value,
                  ": ",
                  hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
                hydra.lib.maps.ToList.apply((ts).value))),
            "}"))),
          hydra.Unification.unifyTypes(
            hydra.Lexical.emptyContext(),
            hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (n -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>(n, new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(n), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))),
              (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()))),
            new hydra.core.Type.Variable(new hydra.core.Name("a")),
            new hydra.core.Type.Variable(new hydra.core.Name("b")),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "{",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                hydra.lib.pairs.First.apply(p).value,
                ": ",
                hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
              hydra.lib.maps.ToList.apply(new hydra.typing.TypeSubst(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
                new hydra.core.Name("a"),
                new hydra.core.Type.Variable(new hydra.core.Name("b")))))).value))),
          "}")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("unify list of variables with list of int32", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<hydra.typing.TypeSubst, String>) (ts -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "{",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  hydra.lib.pairs.First.apply(p).value,
                  ": ",
                  hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
                hydra.lib.maps.ToList.apply((ts).value))),
            "}"))),
          hydra.Unification.unifyTypes(
            hydra.Lexical.emptyContext(),
            hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (n -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>(n, new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(n), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))),
              (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()))),
            new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("a"))),
            new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "{",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                hydra.lib.pairs.First.apply(p).value,
                ": ",
                hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
              hydra.lib.maps.ToList.apply(new hydra.typing.TypeSubst(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
                new hydra.core.Name("a"),
                new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))).value))),
          "}")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("unify identical list types", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<hydra.typing.TypeSubst, String>) (ts -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "{",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  hydra.lib.pairs.First.apply(p).value,
                  ": ",
                  hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
                hydra.lib.maps.ToList.apply((ts).value))),
            "}"))),
          hydra.Unification.unifyTypes(
            hydra.Lexical.emptyContext(),
            hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (n -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>(n, new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(n), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))),
              (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()))),
            new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
            new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "{",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                hydra.lib.pairs.First.apply(p).value,
                ": ",
                hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
              hydra.lib.maps.ToList.apply(new hydra.typing.TypeSubst((java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (java.util.Collections.<hydra.core.Name, hydra.core.Type>emptyMap()))).value))),
          "}")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("unify function types with variables", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<hydra.typing.TypeSubst, String>) (ts -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "{",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  hydra.lib.pairs.First.apply(p).value,
                  ": ",
                  hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
                hydra.lib.maps.ToList.apply((ts).value))),
            "}"))),
          hydra.Unification.unifyTypes(
            hydra.Lexical.emptyContext(),
            hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (n -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>(n, new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(n), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))),
              (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()))),
            new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("a")), new hydra.core.Type.Variable(new hydra.core.Name("b")))),
            new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "{",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                hydra.lib.pairs.First.apply(p).value,
                ": ",
                hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
              hydra.lib.maps.ToList.apply(new hydra.typing.TypeSubst(new java.util.TreeMap(java.util.Map.ofEntries(
                java.util.Map.entry(
                  new hydra.core.Name("a"),
                  new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
                java.util.Map.entry(
                  new hydra.core.Name("b"),
                  new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))).value))),
          "}")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("unify identical function types", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<hydra.typing.TypeSubst, String>) (ts -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "{",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  hydra.lib.pairs.First.apply(p).value,
                  ": ",
                  hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
                hydra.lib.maps.ToList.apply((ts).value))),
            "}"))),
          hydra.Unification.unifyTypes(
            hydra.Lexical.emptyContext(),
            hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (n -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>(n, new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(n), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))),
              (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()))),
            new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))),
            new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "{",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                hydra.lib.pairs.First.apply(p).value,
                ": ",
                hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
              hydra.lib.maps.ToList.apply(new hydra.typing.TypeSubst((java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (java.util.Collections.<hydra.core.Name, hydra.core.Type>emptyMap()))).value))),
          "}")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("unify optional types", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<hydra.typing.TypeSubst, String>) (ts -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "{",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  hydra.lib.pairs.First.apply(p).value,
                  ": ",
                  hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
                hydra.lib.maps.ToList.apply((ts).value))),
            "}"))),
          hydra.Unification.unifyTypes(
            hydra.Lexical.emptyContext(),
            hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (n -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>(n, new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(n), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))),
              (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()))),
            new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("a"))),
            new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "{",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                hydra.lib.pairs.First.apply(p).value,
                ": ",
                hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
              hydra.lib.maps.ToList.apply(new hydra.typing.TypeSubst(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
                new hydra.core.Name("a"),
                new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))).value))),
          "}")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("unify pair types", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<hydra.typing.TypeSubst, String>) (ts -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "{",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  hydra.lib.pairs.First.apply(p).value,
                  ": ",
                  hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
                hydra.lib.maps.ToList.apply((ts).value))),
            "}"))),
          hydra.Unification.unifyTypes(
            hydra.Lexical.emptyContext(),
            hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (n -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>(n, new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(n), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))),
              (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()))),
            new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Variable(new hydra.core.Name("a")), new hydra.core.Type.Variable(new hydra.core.Name("b")))),
            new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "{",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                hydra.lib.pairs.First.apply(p).value,
                ": ",
                hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
              hydra.lib.maps.ToList.apply(new hydra.typing.TypeSubst(new java.util.TreeMap(java.util.Map.ofEntries(
                java.util.Map.entry(
                  new hydra.core.Name("a"),
                  new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
                java.util.Map.entry(
                  new hydra.core.Name("b"),
                  new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))).value))),
          "}")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("unify either types", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<hydra.typing.TypeSubst, String>) (ts -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "{",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  hydra.lib.pairs.First.apply(p).value,
                  ": ",
                  hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
                hydra.lib.maps.ToList.apply((ts).value))),
            "}"))),
          hydra.Unification.unifyTypes(
            hydra.Lexical.emptyContext(),
            hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (n -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>(n, new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(n), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))),
              (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()))),
            new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Variable(new hydra.core.Name("a")), new hydra.core.Type.Variable(new hydra.core.Name("b")))),
            new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "{",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                hydra.lib.pairs.First.apply(p).value,
                ": ",
                hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
              hydra.lib.maps.ToList.apply(new hydra.typing.TypeSubst(new java.util.TreeMap(java.util.Map.ofEntries(
                java.util.Map.entry(
                  new hydra.core.Name("a"),
                  new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
                java.util.Map.entry(
                  new hydra.core.Name("b"),
                  new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))).value))),
          "}")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("unify map types", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<hydra.typing.TypeSubst, String>) (ts -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "{",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  hydra.lib.pairs.First.apply(p).value,
                  ": ",
                  hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
                hydra.lib.maps.ToList.apply((ts).value))),
            "}"))),
          hydra.Unification.unifyTypes(
            hydra.Lexical.emptyContext(),
            hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (n -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>(n, new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(n), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))),
              (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()))),
            new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("k")), new hydra.core.Type.Variable(new hydra.core.Name("v")))),
            new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "{",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                hydra.lib.pairs.First.apply(p).value,
                ": ",
                hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
              hydra.lib.maps.ToList.apply(new hydra.typing.TypeSubst(new java.util.TreeMap(java.util.Map.ofEntries(
                java.util.Map.entry(
                  new hydra.core.Name("k"),
                  new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
                java.util.Map.entry(
                  new hydra.core.Name("v"),
                  new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))).value))),
          "}")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("unify set types", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<hydra.typing.TypeSubst, String>) (ts -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "{",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  hydra.lib.pairs.First.apply(p).value,
                  ": ",
                  hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
                hydra.lib.maps.ToList.apply((ts).value))),
            "}"))),
          hydra.Unification.unifyTypes(
            hydra.Lexical.emptyContext(),
            hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (n -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>(n, new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(n), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))),
              (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()))),
            new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("a"))),
            new hydra.core.Type.Set(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "{",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                hydra.lib.pairs.First.apply(p).value,
                ": ",
                hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
              hydra.lib.maps.ToList.apply(new hydra.typing.TypeSubst(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
                new hydra.core.Name("a"),
                new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))).value))),
          "}")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("unify unit types", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<hydra.typing.TypeSubst, String>) (ts -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "{",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  hydra.lib.pairs.First.apply(p).value,
                  ": ",
                  hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
                hydra.lib.maps.ToList.apply((ts).value))),
            "}"))),
          hydra.Unification.unifyTypes(
            hydra.Lexical.emptyContext(),
            hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (n -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>(n, new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(n), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))),
              (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()))),
            new hydra.core.Type.Unit(),
            new hydra.core.Type.Unit(),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "{",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                hydra.lib.pairs.First.apply(p).value,
                ": ",
                hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
              hydra.lib.maps.ToList.apply(new hydra.typing.TypeSubst((java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (java.util.Collections.<hydra.core.Name, hydra.core.Type>emptyMap()))).value))),
          "}")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("fail to unify int32 with string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<hydra.typing.TypeSubst, String>) (ts -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "{",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  hydra.lib.pairs.First.apply(p).value,
                  ": ",
                  hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
                hydra.lib.maps.ToList.apply((ts).value))),
            "}"))),
          hydra.Unification.unifyTypes(
            hydra.Lexical.emptyContext(),
            hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (n -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>(n, new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(n), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))),
              (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()))),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()),
            "test")), "failure")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("fail to unify list with function", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<hydra.typing.TypeSubst, String>) (ts -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "{",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  hydra.lib.pairs.First.apply(p).value,
                  ": ",
                  hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
                hydra.lib.maps.ToList.apply((ts).value))),
            "}"))),
          hydra.Unification.unifyTypes(
            hydra.Lexical.emptyContext(),
            hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (n -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>(n, new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(n), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))),
              (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()))),
            new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
            new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))),
            "test")), "failure")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("occur check: variable with list containing it", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<hydra.typing.TypeSubst, String>) (ts -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "{",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Type>, String>) (p -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  hydra.lib.pairs.First.apply(p).value,
                  ": ",
                  hydra.show.Core.type(hydra.lib.pairs.Second.apply(p))))),
                hydra.lib.maps.ToList.apply((ts).value))),
            "}"))),
          hydra.Unification.unifyTypes(
            hydra.Lexical.emptyContext(),
            hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>>) (n -> (hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) ((hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>) (new hydra.util.Pair<hydra.core.Name, hydra.core.TypeScheme>(n, new hydra.core.TypeScheme((java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()), new hydra.core.Type.Variable(n), (hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>) (hydra.util.Maybe.<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>nothing())))))),
              (java.util.List<hydra.core.Name>) (java.util.Collections.<hydra.core.Name>emptyList()))),
            new hydra.core.Type.Variable(new hydra.core.Name("a")),
            new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("a"))),
            "test")), "failure")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("joinTypes", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("join identical int32", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<java.util.List<hydra.typing.TypeConstraint>, String>) (cs -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "[",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.typing.TypeConstraint, String>) (c -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  "(",
                  hydra.show.Core.type((c).left),
                  " ~ ",
                  hydra.show.Core.type((c).right),
                  ")"))),
                cs)),
            "]"))),
          hydra.Unification.joinTypes(
            hydra.Lexical.emptyContext(),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "[",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.typing.TypeConstraint, String>) (c -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                "(",
                hydra.show.Core.type((c).left),
                " ~ ",
                hydra.show.Core.type((c).right),
                ")"))),
              (java.util.List<hydra.typing.TypeConstraint>) (java.util.Collections.<hydra.typing.TypeConstraint>emptyList()))),
          "]")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("join identical string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<java.util.List<hydra.typing.TypeConstraint>, String>) (cs -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "[",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.typing.TypeConstraint, String>) (c -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  "(",
                  hydra.show.Core.type((c).left),
                  " ~ ",
                  hydra.show.Core.type((c).right),
                  ")"))),
                cs)),
            "]"))),
          hydra.Unification.joinTypes(
            hydra.Lexical.emptyContext(),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "[",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.typing.TypeConstraint, String>) (c -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                "(",
                hydra.show.Core.type((c).left),
                " ~ ",
                hydra.show.Core.type((c).right),
                ")"))),
              (java.util.List<hydra.typing.TypeConstraint>) (java.util.Collections.<hydra.typing.TypeConstraint>emptyList()))),
          "]")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("join list types", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<java.util.List<hydra.typing.TypeConstraint>, String>) (cs -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "[",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.typing.TypeConstraint, String>) (c -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  "(",
                  hydra.show.Core.type((c).left),
                  " ~ ",
                  hydra.show.Core.type((c).right),
                  ")"))),
                cs)),
            "]"))),
          hydra.Unification.joinTypes(
            hydra.Lexical.emptyContext(),
            new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("a"))),
            new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "[",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.typing.TypeConstraint, String>) (c -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                "(",
                hydra.show.Core.type((c).left),
                " ~ ",
                hydra.show.Core.type((c).right),
                ")"))),
              java.util.Arrays.asList(new hydra.typing.TypeConstraint(new hydra.core.Type.Variable(new hydra.core.Name("a")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), "join types; test")))),
          "]")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("join function types", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<java.util.List<hydra.typing.TypeConstraint>, String>) (cs -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "[",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.typing.TypeConstraint, String>) (c -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  "(",
                  hydra.show.Core.type((c).left),
                  " ~ ",
                  hydra.show.Core.type((c).right),
                  ")"))),
                cs)),
            "]"))),
          hydra.Unification.joinTypes(
            hydra.Lexical.emptyContext(),
            new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Variable(new hydra.core.Name("a")), new hydra.core.Type.Variable(new hydra.core.Name("b")))),
            new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "[",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.typing.TypeConstraint, String>) (c -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                "(",
                hydra.show.Core.type((c).left),
                " ~ ",
                hydra.show.Core.type((c).right),
                ")"))),
              java.util.Arrays.asList(
                new hydra.typing.TypeConstraint(new hydra.core.Type.Variable(new hydra.core.Name("a")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), "join types; test"),
                new hydra.typing.TypeConstraint(new hydra.core.Type.Variable(new hydra.core.Name("b")), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), "join types; test")))),
          "]")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("join optional types", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<java.util.List<hydra.typing.TypeConstraint>, String>) (cs -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "[",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.typing.TypeConstraint, String>) (c -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  "(",
                  hydra.show.Core.type((c).left),
                  " ~ ",
                  hydra.show.Core.type((c).right),
                  ")"))),
                cs)),
            "]"))),
          hydra.Unification.joinTypes(
            hydra.Lexical.emptyContext(),
            new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("a"))),
            new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "[",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.typing.TypeConstraint, String>) (c -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                "(",
                hydra.show.Core.type((c).left),
                " ~ ",
                hydra.show.Core.type((c).right),
                ")"))),
              java.util.Arrays.asList(new hydra.typing.TypeConstraint(new hydra.core.Type.Variable(new hydra.core.Name("a")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), "join types; test")))),
          "]")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("join pair types", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<java.util.List<hydra.typing.TypeConstraint>, String>) (cs -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "[",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.typing.TypeConstraint, String>) (c -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  "(",
                  hydra.show.Core.type((c).left),
                  " ~ ",
                  hydra.show.Core.type((c).right),
                  ")"))),
                cs)),
            "]"))),
          hydra.Unification.joinTypes(
            hydra.Lexical.emptyContext(),
            new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Variable(new hydra.core.Name("a")), new hydra.core.Type.Variable(new hydra.core.Name("b")))),
            new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "[",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.typing.TypeConstraint, String>) (c -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                "(",
                hydra.show.Core.type((c).left),
                " ~ ",
                hydra.show.Core.type((c).right),
                ")"))),
              java.util.Arrays.asList(
                new hydra.typing.TypeConstraint(new hydra.core.Type.Variable(new hydra.core.Name("a")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), "join types; test"),
                new hydra.typing.TypeConstraint(new hydra.core.Type.Variable(new hydra.core.Name("b")), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), "join types; test")))),
          "]")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("join either types", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<java.util.List<hydra.typing.TypeConstraint>, String>) (cs -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "[",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.typing.TypeConstraint, String>) (c -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  "(",
                  hydra.show.Core.type((c).left),
                  " ~ ",
                  hydra.show.Core.type((c).right),
                  ")"))),
                cs)),
            "]"))),
          hydra.Unification.joinTypes(
            hydra.Lexical.emptyContext(),
            new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Variable(new hydra.core.Name("a")), new hydra.core.Type.Variable(new hydra.core.Name("b")))),
            new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "[",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.typing.TypeConstraint, String>) (c -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                "(",
                hydra.show.Core.type((c).left),
                " ~ ",
                hydra.show.Core.type((c).right),
                ")"))),
              java.util.Arrays.asList(
                new hydra.typing.TypeConstraint(new hydra.core.Type.Variable(new hydra.core.Name("a")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), "join types; test"),
                new hydra.typing.TypeConstraint(new hydra.core.Type.Variable(new hydra.core.Name("b")), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), "join types; test")))),
          "]")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("join map types", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<java.util.List<hydra.typing.TypeConstraint>, String>) (cs -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "[",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.typing.TypeConstraint, String>) (c -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  "(",
                  hydra.show.Core.type((c).left),
                  " ~ ",
                  hydra.show.Core.type((c).right),
                  ")"))),
                cs)),
            "]"))),
          hydra.Unification.joinTypes(
            hydra.Lexical.emptyContext(),
            new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Variable(new hydra.core.Name("k")), new hydra.core.Type.Variable(new hydra.core.Name("v")))),
            new hydra.core.Type.Map(new hydra.core.MapType(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "[",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.typing.TypeConstraint, String>) (c -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                "(",
                hydra.show.Core.type((c).left),
                " ~ ",
                hydra.show.Core.type((c).right),
                ")"))),
              java.util.Arrays.asList(
                new hydra.typing.TypeConstraint(new hydra.core.Type.Variable(new hydra.core.Name("k")), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()), "join types; test"),
                new hydra.typing.TypeConstraint(new hydra.core.Type.Variable(new hydra.core.Name("v")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), "join types; test")))),
          "]")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("join set types", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<java.util.List<hydra.typing.TypeConstraint>, String>) (cs -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "[",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.typing.TypeConstraint, String>) (c -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  "(",
                  hydra.show.Core.type((c).left),
                  " ~ ",
                  hydra.show.Core.type((c).right),
                  ")"))),
                cs)),
            "]"))),
          hydra.Unification.joinTypes(
            hydra.Lexical.emptyContext(),
            new hydra.core.Type.Set(new hydra.core.Type.Variable(new hydra.core.Name("a"))),
            new hydra.core.Type.Set(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "[",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.typing.TypeConstraint, String>) (c -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                "(",
                hydra.show.Core.type((c).left),
                " ~ ",
                hydra.show.Core.type((c).right),
                ")"))),
              java.util.Arrays.asList(new hydra.typing.TypeConstraint(new hydra.core.Type.Variable(new hydra.core.Name("a")), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), "join types; test")))),
          "]")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("join unit types", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<java.util.List<hydra.typing.TypeConstraint>, String>) (cs -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "[",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.typing.TypeConstraint, String>) (c -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  "(",
                  hydra.show.Core.type((c).left),
                  " ~ ",
                  hydra.show.Core.type((c).right),
                  ")"))),
                cs)),
            "]"))),
          hydra.Unification.joinTypes(
            hydra.Lexical.emptyContext(),
            new hydra.core.Type.Unit(),
            new hydra.core.Type.Unit(),
            "test")), hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "[",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.typing.TypeConstraint, String>) (c -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                "(",
                hydra.show.Core.type((c).left),
                " ~ ",
                hydra.show.Core.type((c).right),
                ")"))),
              (java.util.List<hydra.typing.TypeConstraint>) (java.util.Collections.<hydra.typing.TypeConstraint>emptyList()))),
          "]")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("fail to join int32 with string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<java.util.List<hydra.typing.TypeConstraint>, String>) (cs -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "[",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.typing.TypeConstraint, String>) (c -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  "(",
                  hydra.show.Core.type((c).left),
                  " ~ ",
                  hydra.show.Core.type((c).right),
                  ")"))),
                cs)),
            "]"))),
          hydra.Unification.joinTypes(
            hydra.Lexical.emptyContext(),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()),
            "test")), "failure")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("fail to join list with function", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<java.util.List<hydra.typing.TypeConstraint>, String>) (cs -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "[",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.typing.TypeConstraint, String>) (c -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  "(",
                  hydra.show.Core.type((c).left),
                  " ~ ",
                  hydra.show.Core.type((c).right),
                  ")"))),
                cs)),
            "]"))),
          hydra.Unification.joinTypes(
            hydra.Lexical.emptyContext(),
            new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
            new hydra.core.Type.Function(new hydra.core.FunctionType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))),
            "test")), "failure")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("fail to join pair with either", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<hydra.errors.UnificationError, String>) (ignored -> "failure"),
          (java.util.function.Function<java.util.List<hydra.typing.TypeConstraint>, String>) (cs -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "[",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.typing.TypeConstraint, String>) (c -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                  "(",
                  hydra.show.Core.type((c).left),
                  " ~ ",
                  hydra.show.Core.type((c).right),
                  ")"))),
                cs)),
            "]"))),
          hydra.Unification.joinTypes(
            hydra.Lexical.emptyContext(),
            new hydra.core.Type.Pair(new hydra.core.PairType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))),
            new hydra.core.Type.Either(new hydra.core.EitherType(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))),
            "test")), "failure")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }
}

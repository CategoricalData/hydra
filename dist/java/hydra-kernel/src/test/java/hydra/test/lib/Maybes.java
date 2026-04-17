// Note: this is an automatically generated file. Do not edit.

package hydra.test.lib;

/**
 * Test cases for hydra.lib.maybes primitives
 */
public interface Maybes {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("hydra.lib.maybes primitives", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("apply", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("both just", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.maybe(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maybes.Apply.apply(
            hydra.util.Maybe.just((java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Add.apply(
              3,
              x))),
            hydra.util.Maybe.just(5))), hydra.show.Core.maybe(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.Maybe.just(8)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("nothing function", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.maybe(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maybes.Apply.apply(
            (hydra.util.Maybe<java.util.function.Function<Integer, Integer>>) (hydra.util.Maybe.<java.util.function.Function<Integer, Integer>>nothing()),
            hydra.util.Maybe.just(5))), hydra.show.Core.maybe(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("nothing value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.maybe(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maybes.Apply.apply(
            hydra.util.Maybe.just((java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Add.apply(
              3,
              x))),
            (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))), hydra.show.Core.maybe(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("bind", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("just to just", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.maybe(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maybes.Bind.apply(
            hydra.util.Maybe.just(5),
            (java.util.function.Function<Integer, hydra.util.Maybe<Integer>>) (x -> hydra.util.Maybe.just(hydra.lib.math.Mul.apply(
              x,
              2))))), hydra.show.Core.maybe(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.Maybe.just(10)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("nothing to nothing", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.maybe(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maybes.Bind.apply(
            (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()),
            (java.util.function.Function<Integer, hydra.util.Maybe<Integer>>) (x -> hydra.util.Maybe.just(hydra.lib.math.Mul.apply(
              x,
              2))))), hydra.show.Core.maybe(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("cases", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("just applies function", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.maybes.Cases.applyLazy(
          hydra.util.Maybe.just(5),
          () -> 0,
          (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
            x,
            2)))), hydra.lib.literals.ShowInt32.apply(10))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("nothing returns default", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.maybes.Cases.applyLazy(
          (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()),
          () -> 99,
          (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
            x,
            2)))), hydra.lib.literals.ShowInt32.apply(99))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("cat", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("filters nothings", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
            hydra.util.Maybe.just(1),
            (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()),
            hydra.util.Maybe.just(2)))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          java.util.Arrays.asList(
            1,
            2)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("all justs", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
            hydra.util.Maybe.just(1),
            hydra.util.Maybe.just(2)))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          java.util.Arrays.asList(
            1,
            2)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("all nothings", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
            (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()),
            (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing())))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maybes.Cat.apply((java.util.List<hydra.util.Maybe<Integer>>) (java.util.Collections.<hydra.util.Maybe<Integer>>emptyList()))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("compose", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("both succeed", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.maybe(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maybes.Compose.apply(
            (java.util.function.Function<Integer, hydra.util.Maybe<Integer>>) (x -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Lte.apply(
                x,
                5),
              () -> hydra.util.Maybe.just(hydra.lib.math.Add.apply(
                x,
                1)),
              () -> (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))),
            (java.util.function.Function<Integer, hydra.util.Maybe<Integer>>) (y -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Gte.apply(
                y,
                5),
              () -> hydra.util.Maybe.just(hydra.lib.math.Mul.apply(
                y,
                2)),
              () -> (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))),
            5)), hydra.show.Core.maybe(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.Maybe.just(12)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("first fails", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.maybe(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maybes.Compose.apply(
            (java.util.function.Function<Integer, hydra.util.Maybe<Integer>>) (x -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Lte.apply(
                x,
                5),
              () -> hydra.util.Maybe.just(hydra.lib.math.Add.apply(
                x,
                1)),
              () -> (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))),
            (java.util.function.Function<Integer, hydra.util.Maybe<Integer>>) (y -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Gte.apply(
                y,
                5),
              () -> hydra.util.Maybe.just(hydra.lib.math.Mul.apply(
                y,
                2)),
              () -> (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))),
            10)), hydra.show.Core.maybe(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("second fails", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.maybe(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maybes.Compose.apply(
            (java.util.function.Function<Integer, hydra.util.Maybe<Integer>>) (x -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Lte.apply(
                x,
                5),
              () -> hydra.util.Maybe.just(hydra.lib.math.Add.apply(
                x,
                1)),
              () -> (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))),
            (java.util.function.Function<Integer, hydra.util.Maybe<Integer>>) (y -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Gte.apply(
                y,
                5),
              () -> hydra.util.Maybe.just(hydra.lib.math.Mul.apply(
                y,
                2)),
              () -> (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))),
            3)), hydra.show.Core.maybe(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("fromMaybe", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("just value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.maybes.FromMaybe.applyLazy(
          () -> 0,
          hydra.util.Maybe.just(42))), hydra.lib.literals.ShowInt32.apply(42))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("nothing with default", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.maybes.FromMaybe.applyLazy(
          () -> 99,
          (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))), hydra.lib.literals.ShowInt32.apply(99))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("isJust", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("just value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.maybes.IsJust.apply(hydra.util.Maybe.just(42))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("nothing", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.maybes.IsJust.apply((hydra.util.Maybe<java.lang.Void>) (hydra.util.Maybe.<java.lang.Void>nothing()))), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("isNothing", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("just value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.maybes.IsNothing.apply(hydra.util.Maybe.just(42))), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("nothing", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.maybes.IsNothing.apply((hydra.util.Maybe<java.lang.Void>) (hydra.util.Maybe.<java.lang.Void>nothing()))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("map", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("maps just value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.maybe(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maybes.Map.apply(
            (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
              x,
              2)),
            hydra.util.Maybe.just(5))), hydra.show.Core.maybe(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.Maybe.just(10)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("nothing unchanged", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.maybe(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maybes.Map.apply(
            (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
              x,
              2)),
            (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))), hydra.show.Core.maybe(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("mapMaybe", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("filter and transform", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maybes.MapMaybe.apply(
            (java.util.function.Function<Integer, hydra.util.Maybe<Integer>>) (x -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Gt.apply(
                x,
                2),
              () -> hydra.util.Maybe.just(hydra.lib.math.Mul.apply(
                x,
                2)),
              () -> (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))),
            java.util.Arrays.asList(
              1,
              2,
              3,
              4,
              5))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          java.util.Arrays.asList(
            6,
            8,
            10)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty result", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maybes.MapMaybe.apply(
            (java.util.function.Function<Integer, hydra.util.Maybe<Integer>>) (x -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Gt.apply(
                x,
                2),
              () -> hydra.util.Maybe.just(hydra.lib.math.Mul.apply(
                x,
                2)),
              () -> (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))),
            java.util.Arrays.asList(
              1,
              2))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty input", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maybes.MapMaybe.apply(
            (java.util.function.Function<Integer, hydra.util.Maybe<Integer>>) (x -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Gt.apply(
                x,
                2),
              () -> hydra.util.Maybe.just(hydra.lib.math.Mul.apply(
                x,
                2)),
              () -> (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))),
            (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("maybe", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("just value applies function", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.maybes.Maybe.applyLazy(
          () -> 0,
          (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
            x,
            2)),
          hydra.util.Maybe.just(5))), hydra.lib.literals.ShowInt32.apply(10))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("nothing returns default", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.maybes.Maybe.applyLazy(
          () -> 99,
          (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
            x,
            2)),
          (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))), hydra.lib.literals.ShowInt32.apply(99))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("pure", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("wraps integer", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.maybe(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maybes.Pure.apply(42)), hydra.show.Core.maybe(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.Maybe.just(42)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("wraps string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.maybe(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maybes.Pure.apply("hello")), hydra.show.Core.maybe(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.util.Maybe.just("hello")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("toList", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("just value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maybes.ToList.apply(hydra.util.Maybe.just(42))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          java.util.Arrays.asList(42)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("nothing", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maybes.ToList.apply((hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }
}

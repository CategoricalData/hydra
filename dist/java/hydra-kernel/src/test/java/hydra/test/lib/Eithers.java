// Note: this is an automatically generated file. Do not edit.

package hydra.test.lib;

/**
 * Test cases for hydra.lib.eithers primitives
 */
public interface Eithers {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("hydra.lib.eithers primitives", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("bind", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("bind Right with success", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.either(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.eithers.Bind.apply(
            hydra.util.Either.<Integer, String>right("ab"),
            (java.util.function.Function<String, hydra.util.Either<Integer, Integer>>) (s -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.strings.Null.apply(s),
              () -> hydra.util.Either.<Integer, Integer>left(0),
              () -> hydra.util.Either.<Integer, Integer>right(hydra.lib.strings.Length.apply(s)))))), hydra.show.Core.either(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.Either.<Integer, Integer>right(2)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("bind Right with failure", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.either(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.eithers.Bind.apply(
            hydra.util.Either.<Integer, String>right(""),
            (java.util.function.Function<String, hydra.util.Either<Integer, Integer>>) (s -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.strings.Null.apply(s),
              () -> hydra.util.Either.<Integer, Integer>left(0),
              () -> hydra.util.Either.<Integer, Integer>right(hydra.lib.strings.Length.apply(s)))))), hydra.show.Core.either(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.Either.<Integer, Integer>left(0)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("bind Left returns Left unchanged", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.either(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.eithers.Bind.apply(
            hydra.util.Either.<Integer, String>left(42),
            (java.util.function.Function<String, hydra.util.Either<Integer, Integer>>) (s -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.strings.Null.apply(s),
              () -> hydra.util.Either.<Integer, Integer>left(0),
              () -> hydra.util.Either.<Integer, Integer>right(hydra.lib.strings.Length.apply(s)))))), hydra.show.Core.either(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.Either.<Integer, Integer>left(42)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("bimap", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("map left value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.either(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.eithers.Bimap.apply(
            (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
              x,
              2)),
            (java.util.function.Function<String, Integer>) (s -> hydra.lib.strings.Length.apply(s)),
            hydra.util.Either.<Integer, String>left(5))), hydra.show.Core.either(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.Either.<Integer, Integer>left(10)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("map right value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.either(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.eithers.Bimap.apply(
            (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
              x,
              2)),
            (java.util.function.Function<String, Integer>) (s -> hydra.lib.strings.Length.apply(s)),
            hydra.util.Either.<Integer, String>right("ab"))), hydra.show.Core.either(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.Either.<Integer, Integer>right(2)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("isLeft", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("left value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.eithers.IsLeft.apply(hydra.util.Either.<Integer, java.lang.Void>left(42))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("right value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.eithers.IsLeft.apply(hydra.util.Either.<java.lang.Void, String>right("test"))), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("isRight", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("right value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.eithers.IsRight.apply(hydra.util.Either.<java.lang.Void, String>right("test"))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("left value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.eithers.IsRight.apply(hydra.util.Either.<Integer, java.lang.Void>left(42))), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("fromLeft", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("extract left", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.eithers.FromLeft.applyLazy(
          () -> 99,
          hydra.util.Either.<Integer, java.lang.Void>left(42))), hydra.lib.literals.ShowInt32.apply(42))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("use default for right", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.eithers.FromLeft.applyLazy(
          () -> 99,
          hydra.util.Either.<Integer, String>right("test"))), hydra.lib.literals.ShowInt32.apply(99))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("fromRight", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("extract right", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.eithers.FromRight.applyLazy(
          () -> "default",
          hydra.util.Either.<java.lang.Void, String>right("test"))), hydra.lib.literals.ShowString.apply("test"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("use default for left", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.eithers.FromRight.applyLazy(
          () -> "default",
          hydra.util.Either.<Integer, String>left(42))), hydra.lib.literals.ShowString.apply("default"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("either", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("apply left function", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
            x,
            2)),
          (java.util.function.Function<String, Integer>) (s -> hydra.lib.strings.Length.apply(s)),
          hydra.util.Either.<Integer, String>left(5))), hydra.lib.literals.ShowInt32.apply(10))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("apply right function", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
            x,
            2)),
          (java.util.function.Function<String, Integer>) (s -> hydra.lib.strings.Length.apply(s)),
          hydra.util.Either.<Integer, String>right("ab"))), hydra.lib.literals.ShowInt32.apply(2))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("lefts", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("filter left values", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.eithers.Lefts.apply(java.util.Arrays.asList(
            hydra.util.Either.<Integer, String>left(1),
            hydra.util.Either.<Integer, String>right("a"),
            hydra.util.Either.<Integer, String>left(2),
            hydra.util.Either.<Integer, String>right("b")))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          java.util.Arrays.asList(
            1,
            2)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("all lefts", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.eithers.Lefts.apply(java.util.Arrays.asList(
            hydra.util.Either.<Integer, java.lang.Void>left(1),
            hydra.util.Either.<Integer, java.lang.Void>left(2)))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          java.util.Arrays.asList(
            1,
            2)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("all rights", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.eithers.Lefts.apply(java.util.Arrays.asList(
            hydra.util.Either.<Integer, String>right("a"),
            hydra.util.Either.<Integer, String>right("b")))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.eithers.Lefts.apply((java.util.List<hydra.util.Either<Integer, java.lang.Void>>) (java.util.Collections.<hydra.util.Either<Integer, java.lang.Void>>emptyList()))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("rights", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("filter right values", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.eithers.Rights.apply(java.util.Arrays.asList(
            hydra.util.Either.<Integer, String>left(1),
            hydra.util.Either.<Integer, String>right("a"),
            hydra.util.Either.<Integer, String>left(2),
            hydra.util.Either.<Integer, String>right("b")))), hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          java.util.Arrays.asList(
            "a",
            "b")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("all rights", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.eithers.Rights.apply(java.util.Arrays.asList(
            hydra.util.Either.<java.lang.Void, String>right("a"),
            hydra.util.Either.<java.lang.Void, String>right("b")))), hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          java.util.Arrays.asList(
            "a",
            "b")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("all lefts", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.eithers.Rights.apply(java.util.Arrays.asList(
            hydra.util.Either.<Integer, String>left(1),
            hydra.util.Either.<Integer, String>left(2)))), hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.List<String>) (java.util.Collections.<String>emptyList())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.eithers.Rights.apply((java.util.List<hydra.util.Either<java.lang.Void, String>>) (java.util.Collections.<hydra.util.Either<java.lang.Void, String>>emptyList()))), hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.List<String>) (java.util.Collections.<String>emptyList())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("partitionEithers", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("partition mixed", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.pair(
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            xs)),
          (java.util.function.Function<java.util.List<String>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            xs)),
          hydra.lib.eithers.PartitionEithers.apply(java.util.Arrays.asList(
            hydra.util.Either.<Integer, String>left(1),
            hydra.util.Either.<Integer, String>right("a"),
            hydra.util.Either.<Integer, String>left(2),
            hydra.util.Either.<Integer, String>right("b")))), hydra.show.Core.pair(
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            xs)),
          (java.util.function.Function<java.util.List<String>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            xs)),
          (hydra.util.Pair<java.util.List<Integer>, java.util.List<String>>) ((hydra.util.Pair<java.util.List<Integer>, java.util.List<String>>) (new hydra.util.Pair<java.util.List<Integer>, java.util.List<String>>(java.util.Arrays.asList(
            1,
            2), java.util.Arrays.asList(
            "a",
            "b"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("all lefts", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.pair(
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            xs)),
          (java.util.function.Function<java.util.List<String>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            xs)),
          hydra.lib.eithers.PartitionEithers.apply(java.util.Arrays.asList(
            hydra.util.Either.<Integer, String>left(1),
            hydra.util.Either.<Integer, String>left(2)))), hydra.show.Core.pair(
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            xs)),
          (java.util.function.Function<java.util.List<String>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            xs)),
          (hydra.util.Pair<java.util.List<Integer>, java.util.List<String>>) ((hydra.util.Pair<java.util.List<Integer>, java.util.List<String>>) (new hydra.util.Pair<java.util.List<Integer>, java.util.List<String>>(java.util.Arrays.asList(
            1,
            2), (java.util.List<String>) (java.util.Collections.<String>emptyList()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("all rights", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.pair(
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            xs)),
          (java.util.function.Function<java.util.List<String>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            xs)),
          hydra.lib.eithers.PartitionEithers.apply(java.util.Arrays.asList(
            hydra.util.Either.<Integer, String>right("a"),
            hydra.util.Either.<Integer, String>right("b")))), hydra.show.Core.pair(
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            xs)),
          (java.util.function.Function<java.util.List<String>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            xs)),
          (hydra.util.Pair<java.util.List<Integer>, java.util.List<String>>) ((hydra.util.Pair<java.util.List<Integer>, java.util.List<String>>) (new hydra.util.Pair<java.util.List<Integer>, java.util.List<String>>((java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()), java.util.Arrays.asList(
            "a",
            "b"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.pair(
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            xs)),
          (java.util.function.Function<java.util.List<String>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            xs)),
          hydra.lib.eithers.PartitionEithers.apply((java.util.List<hydra.util.Either<Integer, String>>) (java.util.Collections.<hydra.util.Either<Integer, String>>emptyList()))), hydra.show.Core.pair(
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            xs)),
          (java.util.function.Function<java.util.List<String>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            xs)),
          (hydra.util.Pair<java.util.List<Integer>, java.util.List<String>>) ((hydra.util.Pair<java.util.List<Integer>, java.util.List<String>>) (new hydra.util.Pair<java.util.List<Integer>, java.util.List<String>>((java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()), (java.util.List<String>) (java.util.Collections.<String>emptyList()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("map", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("map right value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.either(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.eithers.Map.apply(
            (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
              x,
              2)),
            hydra.util.Either.<Integer, Integer>right(5))), hydra.show.Core.either(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.Either.<Integer, Integer>right(10)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("preserve left", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.either(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.eithers.Map.apply(
            (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
              x,
              2)),
            hydra.util.Either.<Integer, Integer>left(99))), hydra.show.Core.either(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.Either.<Integer, Integer>left(99)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("mapList", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("all succeed", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.either(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            xs)),
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<Integer, hydra.util.Either<String, Integer>>) (x -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                x,
                0),
              () -> hydra.util.Either.<String, Integer>left("zero"),
              () -> hydra.util.Either.<String, Integer>right(hydra.lib.math.Mul.apply(
                x,
                2)))),
            java.util.Arrays.asList(
              1,
              2,
              3))), hydra.show.Core.either(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            xs)),
          hydra.util.Either.<String, java.util.List<Integer>>right(java.util.Arrays.asList(
            2,
            4,
            6))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("first fails", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.either(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            xs)),
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<Integer, hydra.util.Either<String, Integer>>) (x -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                x,
                0),
              () -> hydra.util.Either.<String, Integer>left("zero"),
              () -> hydra.util.Either.<String, Integer>right(hydra.lib.math.Mul.apply(
                x,
                2)))),
            java.util.Arrays.asList(
              1,
              0,
              3))), hydra.show.Core.either(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            xs)),
          hydra.util.Either.<String, java.util.List<Integer>>left("zero")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.either(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            xs)),
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<Integer, hydra.util.Either<String, Integer>>) (x -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                x,
                0),
              () -> hydra.util.Either.<String, Integer>left("zero"),
              () -> hydra.util.Either.<String, Integer>right(hydra.lib.math.Mul.apply(
                x,
                2)))),
            (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()))), hydra.show.Core.either(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.function.Function<java.util.List<Integer>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            xs)),
          hydra.util.Either.<String, java.util.List<Integer>>right((java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("mapMaybe", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("just succeeds", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.either(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.function.Function<hydra.util.Maybe<Integer>, String>) (mx -> hydra.show.Core.maybe(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            mx)),
          hydra.lib.eithers.MapMaybe.apply(
            (java.util.function.Function<Integer, hydra.util.Either<String, Integer>>) (x -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                x,
                0),
              () -> hydra.util.Either.<String, Integer>left("zero"),
              () -> hydra.util.Either.<String, Integer>right(hydra.lib.math.Mul.apply(
                x,
                2)))),
            hydra.util.Maybe.just(5))), hydra.show.Core.either(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.function.Function<hydra.util.Maybe<Integer>, String>) (mx -> hydra.show.Core.maybe(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            mx)),
          hydra.util.Either.<String, hydra.util.Maybe<Integer>>right(hydra.util.Maybe.just(10))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("just fails", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.either(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.function.Function<hydra.util.Maybe<Integer>, String>) (mx -> hydra.show.Core.maybe(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            mx)),
          hydra.lib.eithers.MapMaybe.apply(
            (java.util.function.Function<Integer, hydra.util.Either<String, Integer>>) (x -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                x,
                0),
              () -> hydra.util.Either.<String, Integer>left("zero"),
              () -> hydra.util.Either.<String, Integer>right(hydra.lib.math.Mul.apply(
                x,
                2)))),
            hydra.util.Maybe.just(0))), hydra.show.Core.either(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.function.Function<hydra.util.Maybe<Integer>, String>) (mx -> hydra.show.Core.maybe(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            mx)),
          hydra.util.Either.<String, hydra.util.Maybe<Integer>>left("zero")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("nothing", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.either(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.function.Function<hydra.util.Maybe<Integer>, String>) (mx -> hydra.show.Core.maybe(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            mx)),
          hydra.lib.eithers.MapMaybe.apply(
            (java.util.function.Function<Integer, hydra.util.Either<String, Integer>>) (x -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                x,
                0),
              () -> hydra.util.Either.<String, Integer>left("zero"),
              () -> hydra.util.Either.<String, Integer>right(hydra.lib.math.Mul.apply(
                x,
                2)))),
            (hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))), hydra.show.Core.either(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.function.Function<hydra.util.Maybe<Integer>, String>) (mx -> hydra.show.Core.maybe(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            mx)),
          hydra.util.Either.<String, hydra.util.Maybe<Integer>>right((hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }
}

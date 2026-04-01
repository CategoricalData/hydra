// Note: this is an automatically generated file. Do not edit.

package hydra.test.lib;

/**
 * Test cases for hydra.lib.eithers primitives
 */
public interface Eithers {
  static <T9, T8, T7, T6, T5, T4, T3, T2, T1, T0> hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("hydra.lib.eithers primitives", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), hydra.util.ConsList.of(
      new hydra.testing.TestGroup("bind", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
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
          hydra.util.Either.<Integer, Integer>right(2)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
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
          hydra.util.Either.<Integer, Integer>left(0)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
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
          hydra.util.Either.<Integer, Integer>left(42)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("bimap", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
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
          hydra.util.Either.<Integer, Integer>left(10)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
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
          hydra.util.Either.<Integer, Integer>right(2)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("isLeft", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("left value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.eithers.IsLeft.apply(hydra.util.Either.<Integer, T0>left(42))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("right value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.eithers.IsLeft.apply(hydra.util.Either.<T1, String>right("test"))), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("isRight", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("right value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.eithers.IsRight.apply(hydra.util.Either.<T2, String>right("test"))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("left value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.eithers.IsRight.apply(hydra.util.Either.<Integer, T3>left(42))), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("fromLeft", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("extract left", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.eithers.FromLeft.applyLazy(
          () -> 99,
          hydra.util.Either.<Integer, T4>left(42))), hydra.lib.literals.ShowInt32.apply(42))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("use default for right", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.eithers.FromLeft.applyLazy(
          () -> 99,
          hydra.util.Either.<Integer, String>right("test"))), hydra.lib.literals.ShowInt32.apply(99))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("fromRight", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("extract right", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.eithers.FromRight.applyLazy(
          () -> "default",
          hydra.util.Either.<T5, String>right("test"))), hydra.lib.literals.ShowString.apply("test"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("use default for left", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowString.apply(hydra.lib.eithers.FromRight.applyLazy(
          () -> "default",
          hydra.util.Either.<Integer, String>left(42))), hydra.lib.literals.ShowString.apply("default"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("either", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("apply left function", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
            x,
            2)),
          (java.util.function.Function<String, Integer>) (s -> hydra.lib.strings.Length.apply(s)),
          hydra.util.Either.<Integer, String>left(5))), hydra.lib.literals.ShowInt32.apply(10))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("apply right function", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
            x,
            2)),
          (java.util.function.Function<String, Integer>) (s -> hydra.lib.strings.Length.apply(s)),
          hydra.util.Either.<Integer, String>right("ab"))), hydra.lib.literals.ShowInt32.apply(2))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("lefts", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("filter left values", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.eithers.Lefts.apply(hydra.util.ConsList.of(
            hydra.util.Either.<Integer, String>left(1),
            hydra.util.Either.<Integer, String>right("a"),
            hydra.util.Either.<Integer, String>left(2),
            hydra.util.Either.<Integer, String>right("b")))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.ConsList.of(
            1,
            2)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("all lefts", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.eithers.Lefts.apply(hydra.util.ConsList.of(
            hydra.util.Either.<Integer, T6>left(1),
            hydra.util.Either.<Integer, T6>left(2)))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.ConsList.of(
            1,
            2)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("all rights", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.eithers.Lefts.apply(hydra.util.ConsList.of(
            hydra.util.Either.<Integer, String>right("a"),
            hydra.util.Either.<Integer, String>right("b")))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.eithers.Lefts.apply((hydra.util.ConsList<hydra.util.Either<Integer, T7>>) (hydra.util.ConsList.<hydra.util.Either<Integer, T7>>empty()))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("rights", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("filter right values", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.eithers.Rights.apply(hydra.util.ConsList.of(
            hydra.util.Either.<Integer, String>left(1),
            hydra.util.Either.<Integer, String>right("a"),
            hydra.util.Either.<Integer, String>left(2),
            hydra.util.Either.<Integer, String>right("b")))), hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.util.ConsList.of(
            "a",
            "b")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("all rights", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.eithers.Rights.apply(hydra.util.ConsList.of(
            hydra.util.Either.<T8, String>right("a"),
            hydra.util.Either.<T8, String>right("b")))), hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.util.ConsList.of(
            "a",
            "b")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("all lefts", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.eithers.Rights.apply(hydra.util.ConsList.of(
            hydra.util.Either.<Integer, String>left(1),
            hydra.util.Either.<Integer, String>left(2)))), hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (hydra.util.ConsList<String>) (hydra.util.ConsList.<String>empty())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.eithers.Rights.apply((hydra.util.ConsList<hydra.util.Either<T9, String>>) (hydra.util.ConsList.<hydra.util.Either<T9, String>>empty()))), hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (hydra.util.ConsList<String>) (hydra.util.ConsList.<String>empty())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("partitionEithers", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("partition mixed", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.pair(
          (java.util.function.Function<hydra.util.ConsList<Integer>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            xs)),
          (java.util.function.Function<hydra.util.ConsList<String>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            xs)),
          hydra.lib.eithers.PartitionEithers.apply(hydra.util.ConsList.of(
            hydra.util.Either.<Integer, String>left(1),
            hydra.util.Either.<Integer, String>right("a"),
            hydra.util.Either.<Integer, String>left(2),
            hydra.util.Either.<Integer, String>right("b")))), hydra.show.Core.pair(
          (java.util.function.Function<hydra.util.ConsList<Integer>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            xs)),
          (java.util.function.Function<hydra.util.ConsList<String>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            xs)),
          (hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<String>>) ((hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<String>>) (new hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<String>>(hydra.util.ConsList.of(
            1,
            2), hydra.util.ConsList.of(
            "a",
            "b"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("all lefts", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.pair(
          (java.util.function.Function<hydra.util.ConsList<Integer>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            xs)),
          (java.util.function.Function<hydra.util.ConsList<String>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            xs)),
          hydra.lib.eithers.PartitionEithers.apply(hydra.util.ConsList.of(
            hydra.util.Either.<Integer, String>left(1),
            hydra.util.Either.<Integer, String>left(2)))), hydra.show.Core.pair(
          (java.util.function.Function<hydra.util.ConsList<Integer>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            xs)),
          (java.util.function.Function<hydra.util.ConsList<String>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            xs)),
          (hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<String>>) ((hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<String>>) (new hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<String>>(hydra.util.ConsList.of(
            1,
            2), (hydra.util.ConsList<String>) (hydra.util.ConsList.<String>empty()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("all rights", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.pair(
          (java.util.function.Function<hydra.util.ConsList<Integer>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            xs)),
          (java.util.function.Function<hydra.util.ConsList<String>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            xs)),
          hydra.lib.eithers.PartitionEithers.apply(hydra.util.ConsList.of(
            hydra.util.Either.<Integer, String>right("a"),
            hydra.util.Either.<Integer, String>right("b")))), hydra.show.Core.pair(
          (java.util.function.Function<hydra.util.ConsList<Integer>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            xs)),
          (java.util.function.Function<hydra.util.ConsList<String>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            xs)),
          (hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<String>>) ((hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<String>>) (new hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<String>>((hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()), hydra.util.ConsList.of(
            "a",
            "b"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.pair(
          (java.util.function.Function<hydra.util.ConsList<Integer>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            xs)),
          (java.util.function.Function<hydra.util.ConsList<String>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            xs)),
          hydra.lib.eithers.PartitionEithers.apply((hydra.util.ConsList<hydra.util.Either<Integer, String>>) (hydra.util.ConsList.<hydra.util.Either<Integer, String>>empty()))), hydra.show.Core.pair(
          (java.util.function.Function<hydra.util.ConsList<Integer>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            xs)),
          (java.util.function.Function<hydra.util.ConsList<String>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            xs)),
          (hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<String>>) ((hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<String>>) (new hydra.util.Pair<hydra.util.ConsList<Integer>, hydra.util.ConsList<String>>((hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()), (hydra.util.ConsList<String>) (hydra.util.ConsList.<String>empty()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("map", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
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
          hydra.util.Either.<Integer, Integer>right(10)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
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
          hydra.util.Either.<Integer, Integer>left(99)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("mapList", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("all succeed", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.either(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.function.Function<hydra.util.ConsList<Integer>, String>) (xs -> hydra.show.Core.list(
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
            hydra.util.ConsList.of(
              1,
              2,
              3))), hydra.show.Core.either(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.function.Function<hydra.util.ConsList<Integer>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            xs)),
          hydra.util.Either.<String, hydra.util.ConsList<Integer>>right(hydra.util.ConsList.of(
            2,
            4,
            6))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("first fails", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.either(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.function.Function<hydra.util.ConsList<Integer>, String>) (xs -> hydra.show.Core.list(
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
            hydra.util.ConsList.of(
              1,
              0,
              3))), hydra.show.Core.either(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.function.Function<hydra.util.ConsList<Integer>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            xs)),
          hydra.util.Either.<String, hydra.util.ConsList<Integer>>left("zero")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.either(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.function.Function<hydra.util.ConsList<Integer>, String>) (xs -> hydra.show.Core.list(
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
            (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()))), hydra.show.Core.either(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.function.Function<hydra.util.ConsList<Integer>, String>) (xs -> hydra.show.Core.list(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            xs)),
          hydra.util.Either.<String, hydra.util.ConsList<Integer>>right((hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("mapMaybe", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
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
          hydra.util.Either.<String, hydra.util.Maybe<Integer>>right(hydra.util.Maybe.just(10))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
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
          hydra.util.Either.<String, hydra.util.Maybe<Integer>>left("zero")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
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
          hydra.util.Either.<String, hydra.util.Maybe<Integer>>right((hydra.util.Maybe<Integer>) (hydra.util.Maybe.<Integer>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty()))))), (hydra.util.ConsList<hydra.testing.TestCaseWithMetadata>) (hydra.util.ConsList.<hydra.testing.TestCaseWithMetadata>empty()));
  }
}

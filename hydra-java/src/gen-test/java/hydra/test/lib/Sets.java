// Note: this is an automatically generated file. Do not edit.

package hydra.test.lib;

/**
 * Test cases for hydra.lib.sets primitives
 */
public interface Sets {
  static <T1, T0> hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("hydra.lib.sets primitives", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), hydra.util.ConsList.of(
      new hydra.testing.TestGroup("empty", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(new hydra.testing.TestCaseWithMetadata("empty set", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
        (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
        (hydra.util.PersistentSet<Integer>) (hydra.lib.sets.Empty.<Integer>apply())), hydra.show.Core.set(
        (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
        (hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("singleton", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(new hydra.testing.TestCaseWithMetadata("single element", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
        (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
        hydra.lib.sets.Singleton.apply(42)), hydra.show.Core.set(
        (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
        hydra.util.PersistentSet.of(42)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("fromList", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("create from list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.FromList.apply(hydra.util.ConsList.of(
            1,
            2,
            3))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.PersistentSet.of(
            1,
            2,
            3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("duplicates removed", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.FromList.apply(hydra.util.ConsList.of(
            1,
            2,
            1,
            3))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.PersistentSet.of(
            1,
            2,
            3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.FromList.apply((hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty()))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("toList", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("convert to list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.ToList.apply(hydra.util.PersistentSet.of(
            1,
            2,
            3))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.ConsList.of(
            1,
            2,
            3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("unsorted input", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.ToList.apply(hydra.util.PersistentSet.of(
            1,
            2,
            3))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.ConsList.of(
            1,
            2,
            3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty set", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.ToList.apply((hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty()))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("insert", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("insert new element", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Insert.apply(
            4,
            hydra.util.PersistentSet.of(
              1,
              2,
              3))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.PersistentSet.of(
            1,
            2,
            3,
            4)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("insert existing element", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Insert.apply(
            2,
            hydra.util.PersistentSet.of(
              1,
              2,
              3))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.PersistentSet.of(
            1,
            2,
            3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("insert into empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Insert.apply(
            1,
            (hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty()))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.PersistentSet.of(1)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("delete", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("delete existing", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Delete.apply(
            2,
            hydra.util.PersistentSet.of(
              1,
              2,
              3))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.PersistentSet.of(
            1,
            3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("delete non-existing", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Delete.apply(
            4,
            hydra.util.PersistentSet.of(
              1,
              2,
              3))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.PersistentSet.of(
            1,
            2,
            3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("delete from empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Delete.apply(
            1,
            (hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty()))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("member", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("element exists", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.sets.Member.apply(
          2,
          hydra.util.PersistentSet.of(
            1,
            2,
            3))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("element missing", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.sets.Member.apply(
          4,
          hydra.util.PersistentSet.of(
            1,
            2,
            3))), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty set", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.sets.Member.apply(
          1,
          (hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty()))), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("size", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("three elements", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.sets.Size.apply(hydra.util.PersistentSet.of(
          1,
          2,
          3))), hydra.lib.literals.ShowInt32.apply(3))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("single element", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.sets.Size.apply(hydra.util.PersistentSet.of(42))), hydra.lib.literals.ShowInt32.apply(1))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty set", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.sets.Size.apply((hydra.util.PersistentSet<T0>) (hydra.util.PersistentSet.<T0>empty()))), hydra.lib.literals.ShowInt32.apply(0))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("null", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("empty set", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.sets.Null.apply((hydra.util.PersistentSet<T1>) (hydra.util.PersistentSet.<T1>empty()))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("non-empty set", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.sets.Null.apply(hydra.util.PersistentSet.of(
          1,
          2))), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("union", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("union two sets", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Union.apply(
            hydra.util.PersistentSet.of(
              1,
              2),
            hydra.util.PersistentSet.of(
              2,
              3))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.PersistentSet.of(
            1,
            2,
            3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("union with empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Union.apply(
            hydra.util.PersistentSet.of(
              1,
              2),
            (hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty()))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.PersistentSet.of(
            1,
            2)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty with non-empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Union.apply(
            (hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty()),
            hydra.util.PersistentSet.of(
              1,
              2))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.PersistentSet.of(
            1,
            2)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("unions", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("union of multiple sets", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Unions.apply(hydra.util.ConsList.of(
            hydra.util.PersistentSet.of(
              1,
              2),
            hydra.util.PersistentSet.of(
              2,
              3),
            hydra.util.PersistentSet.of(
              3,
              4)))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.PersistentSet.of(
            1,
            2,
            3,
            4)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("union with empty sets", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Unions.apply(hydra.util.ConsList.of(
            hydra.util.PersistentSet.of(
              1,
              2),
            (hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty()),
            hydra.util.PersistentSet.of(3)))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.PersistentSet.of(
            1,
            2,
            3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty list of sets", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Unions.apply((hydra.util.ConsList<hydra.util.PersistentSet<Integer>>) (hydra.util.ConsList.<hydra.util.PersistentSet<Integer>>empty()))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("single set", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Unions.apply(hydra.util.ConsList.of(hydra.util.PersistentSet.of(
            1,
            2,
            3)))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.PersistentSet.of(
            1,
            2,
            3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("intersection", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("common elements", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Intersection.apply(
            hydra.util.PersistentSet.of(
              1,
              2,
              3),
            hydra.util.PersistentSet.of(
              2,
              3,
              4))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.PersistentSet.of(
            2,
            3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("no common elements", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Intersection.apply(
            hydra.util.PersistentSet.of(
              1,
              2),
            hydra.util.PersistentSet.of(
              3,
              4))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("intersection with empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Intersection.apply(
            hydra.util.PersistentSet.of(
              1,
              2),
            (hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty()))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("difference", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("remove elements", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Difference.apply(
            hydra.util.PersistentSet.of(
              1,
              2,
              3),
            hydra.util.PersistentSet.of(
              2,
              4))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.PersistentSet.of(
            1,
            3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("no overlap", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Difference.apply(
            hydra.util.PersistentSet.of(
              1,
              2),
            hydra.util.PersistentSet.of(
              3,
              4))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.PersistentSet.of(
            1,
            2)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("difference with empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Difference.apply(
            hydra.util.PersistentSet.of(
              1,
              2),
            (hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty()))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.PersistentSet.of(
            1,
            2)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("map", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("map function", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Map.apply(
            (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
              x,
              2)),
            hydra.util.PersistentSet.of(
              1,
              2,
              3))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.PersistentSet.of(
            2,
            4,
            6)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("map on empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Map.apply(
            (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
              x,
              2)),
            (hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty()))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty()))))), (hydra.util.ConsList<hydra.testing.TestCaseWithMetadata>) (hydra.util.ConsList.<hydra.testing.TestCaseWithMetadata>empty()));
  }
}

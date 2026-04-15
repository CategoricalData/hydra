// Note: this is an automatically generated file. Do not edit.

package hydra.test.lib;

/**
 * Test cases for hydra.lib.sets primitives
 */
public interface Sets {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("hydra.lib.sets primitives", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("empty", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("empty set", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
        (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
        (java.util.Set<Integer>) (hydra.lib.sets.Empty.<Integer>apply())), hydra.show.Core.set(
        (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
        (java.util.Set<Integer>) (java.util.Collections.<Integer>emptySet())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("singleton", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("single element", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
        (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
        hydra.lib.sets.Singleton.apply(42)), hydra.show.Core.set(
        (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
        new java.util.TreeSet(java.util.Set.of(42))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("fromList", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("create from list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.FromList.apply(java.util.Arrays.asList(
            1,
            2,
            3))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          new java.util.TreeSet(java.util.Set.of(
            1,
            2,
            3))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("duplicates removed", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.FromList.apply(java.util.Arrays.asList(
            1,
            2,
            1,
            3))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          new java.util.TreeSet(java.util.Set.of(
            1,
            2,
            3))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.FromList.apply((java.util.List<Integer>) (java.util.Collections.<Integer>emptyList()))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.Set<Integer>) (java.util.Collections.<Integer>emptySet())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("toList", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("convert to list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.ToList.apply(new java.util.TreeSet(java.util.Set.of(
            1,
            2,
            3)))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          java.util.Arrays.asList(
            1,
            2,
            3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("unsorted input", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.ToList.apply(new java.util.TreeSet(java.util.Set.of(
            1,
            2,
            3)))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          java.util.Arrays.asList(
            1,
            2,
            3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty set", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.ToList.apply((java.util.Set<Integer>) (java.util.Collections.<Integer>emptySet()))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("insert", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("insert new element", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Insert.apply(
            4,
            new java.util.TreeSet(java.util.Set.of(
              1,
              2,
              3)))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          new java.util.TreeSet(java.util.Set.of(
            1,
            2,
            3,
            4))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("insert existing element", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Insert.apply(
            2,
            new java.util.TreeSet(java.util.Set.of(
              1,
              2,
              3)))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          new java.util.TreeSet(java.util.Set.of(
            1,
            2,
            3))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("insert into empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Insert.apply(
            1,
            (java.util.Set<Integer>) (java.util.Collections.<Integer>emptySet()))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          new java.util.TreeSet(java.util.Set.of(1))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("delete", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("delete existing", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Delete.apply(
            2,
            new java.util.TreeSet(java.util.Set.of(
              1,
              2,
              3)))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          new java.util.TreeSet(java.util.Set.of(
            1,
            3))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("delete non-existing", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Delete.apply(
            4,
            new java.util.TreeSet(java.util.Set.of(
              1,
              2,
              3)))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          new java.util.TreeSet(java.util.Set.of(
            1,
            2,
            3))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("delete from empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Delete.apply(
            1,
            (java.util.Set<Integer>) (java.util.Collections.<Integer>emptySet()))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.Set<Integer>) (java.util.Collections.<Integer>emptySet())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("member", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("element exists", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.sets.Member.apply(
          2,
          new java.util.TreeSet(java.util.Set.of(
            1,
            2,
            3)))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("element missing", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.sets.Member.apply(
          4,
          new java.util.TreeSet(java.util.Set.of(
            1,
            2,
            3)))), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty set", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.sets.Member.apply(
          1,
          (java.util.Set<Integer>) (java.util.Collections.<Integer>emptySet()))), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("size", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("three elements", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.sets.Size.apply(new java.util.TreeSet(java.util.Set.of(
          1,
          2,
          3)))), hydra.lib.literals.ShowInt32.apply(3))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("single element", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.sets.Size.apply(new java.util.TreeSet(java.util.Set.of(42)))), hydra.lib.literals.ShowInt32.apply(1))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty set", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.sets.Size.apply((java.util.Set<java.lang.Void>) (java.util.Collections.<java.lang.Void>emptySet()))), hydra.lib.literals.ShowInt32.apply(0))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("null", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("empty set", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.sets.Null.apply((java.util.Set<java.lang.Void>) (java.util.Collections.<java.lang.Void>emptySet()))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("non-empty set", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.sets.Null.apply(new java.util.TreeSet(java.util.Set.of(
          1,
          2)))), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("union", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("union two sets", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Union.apply(
            new java.util.TreeSet(java.util.Set.of(
              1,
              2)),
            new java.util.TreeSet(java.util.Set.of(
              2,
              3)))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          new java.util.TreeSet(java.util.Set.of(
            1,
            2,
            3))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("union with empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Union.apply(
            new java.util.TreeSet(java.util.Set.of(
              1,
              2)),
            (java.util.Set<Integer>) (java.util.Collections.<Integer>emptySet()))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          new java.util.TreeSet(java.util.Set.of(
            1,
            2))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty with non-empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Union.apply(
            (java.util.Set<Integer>) (java.util.Collections.<Integer>emptySet()),
            new java.util.TreeSet(java.util.Set.of(
              1,
              2)))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          new java.util.TreeSet(java.util.Set.of(
            1,
            2))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("unions", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("union of multiple sets", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Unions.apply(java.util.Arrays.asList(
            new java.util.TreeSet(java.util.Set.of(
              1,
              2)),
            new java.util.TreeSet(java.util.Set.of(
              2,
              3)),
            new java.util.TreeSet(java.util.Set.of(
              3,
              4))))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          new java.util.TreeSet(java.util.Set.of(
            1,
            2,
            3,
            4))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("union with empty sets", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Unions.apply(java.util.Arrays.asList(
            new java.util.TreeSet(java.util.Set.of(
              1,
              2)),
            (java.util.Set<Integer>) (java.util.Collections.<Integer>emptySet()),
            new java.util.TreeSet(java.util.Set.of(3))))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          new java.util.TreeSet(java.util.Set.of(
            1,
            2,
            3))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty list of sets", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Unions.apply((java.util.List<java.util.Set<Integer>>) (java.util.Collections.<java.util.Set<Integer>>emptyList()))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.Set<Integer>) (java.util.Collections.<Integer>emptySet())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("single set", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Unions.apply(java.util.Arrays.asList(new java.util.TreeSet(java.util.Set.of(
            1,
            2,
            3))))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          new java.util.TreeSet(java.util.Set.of(
            1,
            2,
            3))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("intersection", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("common elements", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Intersection.apply(
            new java.util.TreeSet(java.util.Set.of(
              1,
              2,
              3)),
            new java.util.TreeSet(java.util.Set.of(
              2,
              3,
              4)))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          new java.util.TreeSet(java.util.Set.of(
            2,
            3))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("no common elements", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Intersection.apply(
            new java.util.TreeSet(java.util.Set.of(
              1,
              2)),
            new java.util.TreeSet(java.util.Set.of(
              3,
              4)))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.Set<Integer>) (java.util.Collections.<Integer>emptySet())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("intersection with empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Intersection.apply(
            new java.util.TreeSet(java.util.Set.of(
              1,
              2)),
            (java.util.Set<Integer>) (java.util.Collections.<Integer>emptySet()))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.Set<Integer>) (java.util.Collections.<Integer>emptySet())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("difference", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("remove elements", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Difference.apply(
            new java.util.TreeSet(java.util.Set.of(
              1,
              2,
              3)),
            new java.util.TreeSet(java.util.Set.of(
              2,
              4)))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          new java.util.TreeSet(java.util.Set.of(
            1,
            3))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("no overlap", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Difference.apply(
            new java.util.TreeSet(java.util.Set.of(
              1,
              2)),
            new java.util.TreeSet(java.util.Set.of(
              3,
              4)))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          new java.util.TreeSet(java.util.Set.of(
            1,
            2))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("difference with empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Difference.apply(
            new java.util.TreeSet(java.util.Set.of(
              1,
              2)),
            (java.util.Set<Integer>) (java.util.Collections.<Integer>emptySet()))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          new java.util.TreeSet(java.util.Set.of(
            1,
            2))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("map", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("map function", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Map.apply(
            (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
              x,
              2)),
            new java.util.TreeSet(java.util.Set.of(
              1,
              2,
              3)))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          new java.util.TreeSet(java.util.Set.of(
            2,
            4,
            6))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("map on empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.sets.Map.apply(
            (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
              x,
              2)),
            (java.util.Set<Integer>) (java.util.Collections.<Integer>emptySet()))), hydra.show.Core.set(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.Set<Integer>) (java.util.Collections.<Integer>emptySet())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }
}

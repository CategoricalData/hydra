// Note: this is an automatically generated file. Do not edit.

package hydra.test.lib;

/**
 * Test cases for hydra.lib.maps primitives
 */
public interface Maps {
  static <T6, T5, T4, T3, T2, T1, T0> hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("hydra.lib.maps primitives", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), hydra.util.ConsList.of(
      new hydra.testing.TestGroup("alter", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("insert new key", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Alter.apply(
            (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Maybe<String>>) (ignored -> hydra.util.Maybe.just("new")),
            3,
            hydra.util.PersistentMap.ofEntries(
              hydra.util.PersistentMap.entry(
                1,
                "a"),
              hydra.util.PersistentMap.entry(
                2,
                "b")))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.util.PersistentMap.ofEntries(
            hydra.util.PersistentMap.entry(
              1,
              "a"),
            hydra.util.PersistentMap.entry(
              2,
              "b"),
            hydra.util.PersistentMap.entry(
              3,
              "new"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("update existing key", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Alter.apply(
            (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Maybe<String>>) (ignored -> hydra.util.Maybe.just("updated")),
            2,
            hydra.util.PersistentMap.ofEntries(
              hydra.util.PersistentMap.entry(
                1,
                "a"),
              hydra.util.PersistentMap.entry(
                2,
                "b")))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.util.PersistentMap.ofEntries(
            hydra.util.PersistentMap.entry(
              1,
              "a"),
            hydra.util.PersistentMap.entry(
              2,
              "updated"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("delete key", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Alter.apply(
            (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Maybe<String>>) (ignored -> (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())),
            2,
            hydra.util.PersistentMap.ofEntries(
              hydra.util.PersistentMap.entry(
                1,
                "a"),
              hydra.util.PersistentMap.entry(
                2,
                "b")))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
            1,
            "a"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("bimap", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("transform both", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Bimap.apply(
            (java.util.function.Function<Integer, Integer>) (k -> hydra.lib.math.Mul.apply(
              k,
              2)),
            (java.util.function.Function<String, String>) (v -> hydra.lib.strings.ToUpper.apply(v)),
            hydra.util.PersistentMap.ofEntries(
              hydra.util.PersistentMap.entry(
                1,
                "a"),
              hydra.util.PersistentMap.entry(
                2,
                "b")))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.util.PersistentMap.ofEntries(
            hydra.util.PersistentMap.entry(
              2,
              "A"),
            hydra.util.PersistentMap.entry(
              4,
              "B"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Bimap.apply(
            (java.util.function.Function<Integer, Integer>) (k -> hydra.lib.math.Mul.apply(
              k,
              2)),
            (java.util.function.Function<String, String>) (v -> hydra.lib.strings.ToUpper.apply(v)),
            (hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.util.PersistentMap.<Integer, String>empty())))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.util.PersistentMap.<Integer, String>empty()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("elems", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("get all elements", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Elems.apply(hydra.util.PersistentMap.ofEntries(
            hydra.util.PersistentMap.entry(
              1,
              "a"),
            hydra.util.PersistentMap.entry(
              2,
              "b")))), hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.util.ConsList.of(
            "a",
            "b")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("unsorted keys", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Elems.apply(hydra.util.PersistentMap.ofEntries(
            hydra.util.PersistentMap.entry(
              1,
              "a"),
            hydra.util.PersistentMap.entry(
              2,
              "b"),
            hydra.util.PersistentMap.entry(
              3,
              "c")))), hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.util.ConsList.of(
            "a",
            "b",
            "c")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Elems.apply((hydra.util.PersistentMap<T0, String>) ((hydra.util.PersistentMap<T0, String>) (hydra.util.PersistentMap.<T0, String>empty())))), hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (hydra.util.ConsList<String>) (hydra.util.ConsList.<String>empty())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("empty", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(new hydra.testing.TestCaseWithMetadata("empty map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
        (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
        (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
        (hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.lib.maps.Empty.<Integer, String>apply()))), hydra.show.Core.map(
        (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
        (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
        (hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.util.PersistentMap.<Integer, String>empty()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("filter", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("filter values starting with a", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Filter.apply(
            (java.util.function.Function<String, Boolean>) (v -> hydra.lib.equality.Equal.apply(
              hydra.lib.chars.ToLower.apply(hydra.lib.strings.CharAt.apply(
                0,
                v)),
              97)),
            hydra.util.PersistentMap.ofEntries(
              hydra.util.PersistentMap.entry(
                1,
                "a"),
              hydra.util.PersistentMap.entry(
                2,
                "b"),
              hydra.util.PersistentMap.entry(
                3,
                "ab")))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.util.PersistentMap.ofEntries(
            hydra.util.PersistentMap.entry(
              1,
              "a"),
            hydra.util.PersistentMap.entry(
              3,
              "ab"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("filter all", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Filter.apply(
            (java.util.function.Function<String, Boolean>) (v -> hydra.lib.equality.Equal.apply(
              hydra.lib.chars.ToLower.apply(hydra.lib.strings.CharAt.apply(
                0,
                v)),
              97)),
            hydra.util.PersistentMap.ofEntries(
              hydra.util.PersistentMap.entry(
                1,
                "b"),
              hydra.util.PersistentMap.entry(
                2,
                "c")))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.util.PersistentMap.<Integer, String>empty()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Filter.apply(
            (java.util.function.Function<String, Boolean>) (v -> hydra.lib.equality.Equal.apply(
              hydra.lib.chars.ToLower.apply(hydra.lib.strings.CharAt.apply(
                0,
                v)),
              97)),
            (hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.util.PersistentMap.<Integer, String>empty())))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.util.PersistentMap.<Integer, String>empty()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("filterWithKey", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("filter by key > 1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.FilterWithKey.apply(
            (java.util.function.Function<Integer, java.util.function.Function<String, Boolean>>) (k -> (java.util.function.Function<String, Boolean>) (v -> hydra.lib.equality.Gt.apply(
              k,
              1))),
            hydra.util.PersistentMap.ofEntries(
              hydra.util.PersistentMap.entry(
                1,
                "a"),
              hydra.util.PersistentMap.entry(
                2,
                "b"),
              hydra.util.PersistentMap.entry(
                3,
                "c")))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.util.PersistentMap.ofEntries(
            hydra.util.PersistentMap.entry(
              2,
              "b"),
            hydra.util.PersistentMap.entry(
              3,
              "c"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("filter all", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.FilterWithKey.apply(
            (java.util.function.Function<Integer, java.util.function.Function<String, Boolean>>) (k -> (java.util.function.Function<String, Boolean>) (v -> hydra.lib.equality.Gt.apply(
              k,
              1))),
            hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
              1,
              "a")))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.util.PersistentMap.<Integer, String>empty()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.FilterWithKey.apply(
            (java.util.function.Function<Integer, java.util.function.Function<String, Boolean>>) (k -> (java.util.function.Function<String, Boolean>) (v -> hydra.lib.equality.Gt.apply(
              k,
              1))),
            (hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.util.PersistentMap.<Integer, String>empty())))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.util.PersistentMap.<Integer, String>empty()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("findWithDefault", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("find existing", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.maps.FindWithDefault.applyLazy(
          () -> "default",
          2,
          hydra.util.PersistentMap.ofEntries(
            hydra.util.PersistentMap.entry(
              1,
              "a"),
            hydra.util.PersistentMap.entry(
              2,
              "b"))), "b")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("use default", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.maps.FindWithDefault.applyLazy(
          () -> "default",
          3,
          hydra.util.PersistentMap.ofEntries(
            hydra.util.PersistentMap.entry(
              1,
              "a"),
            hydra.util.PersistentMap.entry(
              2,
              "b"))), "default")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("fromList", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("create from pairs", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.FromList.apply(hydra.util.ConsList.of(
            (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(1, "a"))),
            (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(2, "b")))))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.util.PersistentMap.ofEntries(
            hydra.util.PersistentMap.entry(
              1,
              "a"),
            hydra.util.PersistentMap.entry(
              2,
              "b"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("duplicate keys", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.FromList.apply(hydra.util.ConsList.of(
            (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(1, "a"))),
            (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(1, "b")))))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
            1,
            "b"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.FromList.apply((hydra.util.ConsList<hydra.util.Pair<Integer, String>>) (hydra.util.ConsList.<hydra.util.Pair<Integer, String>>empty()))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.util.PersistentMap.<Integer, String>empty()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("insert", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("insert new key", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Insert.apply(
            3,
            "c",
            hydra.util.PersistentMap.ofEntries(
              hydra.util.PersistentMap.entry(
                1,
                "a"),
              hydra.util.PersistentMap.entry(
                2,
                "b")))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.util.PersistentMap.ofEntries(
            hydra.util.PersistentMap.entry(
              1,
              "a"),
            hydra.util.PersistentMap.entry(
              2,
              "b"),
            hydra.util.PersistentMap.entry(
              3,
              "c"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("update existing", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Insert.apply(
            2,
            "updated",
            hydra.util.PersistentMap.ofEntries(
              hydra.util.PersistentMap.entry(
                1,
                "a"),
              hydra.util.PersistentMap.entry(
                2,
                "b")))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.util.PersistentMap.ofEntries(
            hydra.util.PersistentMap.entry(
              1,
              "a"),
            hydra.util.PersistentMap.entry(
              2,
              "updated"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("insert into empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Insert.apply(
            1,
            "x",
            (hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.util.PersistentMap.<Integer, String>empty())))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
            1,
            "x"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("keys", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("get all keys", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maps.Keys.apply(hydra.util.PersistentMap.ofEntries(
            hydra.util.PersistentMap.entry(
              1,
              "a"),
            hydra.util.PersistentMap.entry(
              2,
              "b"),
            hydra.util.PersistentMap.entry(
              3,
              "c")))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.ConsList.of(
            1,
            2,
            3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("unsorted keys", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maps.Keys.apply(hydra.util.PersistentMap.ofEntries(
            hydra.util.PersistentMap.entry(
              1,
              "a"),
            hydra.util.PersistentMap.entry(
              2,
              "b"),
            hydra.util.PersistentMap.entry(
              3,
              "c")))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.util.ConsList.of(
            1,
            2,
            3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maps.Keys.apply((hydra.util.PersistentMap<Integer, T1>) ((hydra.util.PersistentMap<Integer, T1>) (hydra.util.PersistentMap.<Integer, T1>empty())))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (hydra.util.ConsList<Integer>) (hydra.util.ConsList.<Integer>empty())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("lookup", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("find existing key", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.maybe(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Lookup.apply(
            2,
            hydra.util.PersistentMap.ofEntries(
              hydra.util.PersistentMap.entry(
                1,
                "a"),
              hydra.util.PersistentMap.entry(
                2,
                "b")))), hydra.show.Core.maybe(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.util.Maybe.just("b")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("key not found", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.maybe(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Lookup.apply(
            3,
            hydra.util.PersistentMap.ofEntries(
              hydra.util.PersistentMap.entry(
                1,
                "a"),
              hydra.util.PersistentMap.entry(
                2,
                "b")))), hydra.show.Core.maybe(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("lookup in empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.maybe(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Lookup.apply(
            1,
            (hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.util.PersistentMap.<Integer, String>empty())))), hydra.show.Core.maybe(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("map", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("map over values", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Map.apply(
            (java.util.function.Function<String, String>) (s -> hydra.lib.strings.ToUpper.apply(s)),
            hydra.util.PersistentMap.ofEntries(
              hydra.util.PersistentMap.entry(
                1,
                "a"),
              hydra.util.PersistentMap.entry(
                2,
                "b")))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.util.PersistentMap.ofEntries(
            hydra.util.PersistentMap.entry(
              1,
              "A"),
            hydra.util.PersistentMap.entry(
              2,
              "B"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("map empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Map.apply(
            (java.util.function.Function<String, String>) (s -> hydra.lib.strings.ToUpper.apply(s)),
            (hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.util.PersistentMap.<Integer, String>empty())))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.util.PersistentMap.<Integer, String>empty()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("mapKeys", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("double keys", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.MapKeys.apply(
            (java.util.function.Function<Integer, Integer>) (k -> hydra.lib.math.Mul.apply(
              k,
              2)),
            hydra.util.PersistentMap.ofEntries(
              hydra.util.PersistentMap.entry(
                1,
                "a"),
              hydra.util.PersistentMap.entry(
                2,
                "b")))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.util.PersistentMap.ofEntries(
            hydra.util.PersistentMap.entry(
              2,
              "a"),
            hydra.util.PersistentMap.entry(
              4,
              "b"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.MapKeys.apply(
            (java.util.function.Function<Integer, Integer>) (k -> hydra.lib.math.Mul.apply(
              k,
              2)),
            (hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.util.PersistentMap.<Integer, String>empty())))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.util.PersistentMap.<Integer, String>empty()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("member", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("key exists", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.maps.Member.apply(
          2,
          hydra.util.PersistentMap.ofEntries(
            hydra.util.PersistentMap.entry(
              1,
              "a"),
            hydra.util.PersistentMap.entry(
              2,
              "b")))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("key missing", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.maps.Member.apply(
          3,
          hydra.util.PersistentMap.ofEntries(
            hydra.util.PersistentMap.entry(
              1,
              "a"),
            hydra.util.PersistentMap.entry(
              2,
              "b")))), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.maps.Member.apply(
          1,
          (hydra.util.PersistentMap<Integer, T2>) ((hydra.util.PersistentMap<Integer, T2>) (hydra.util.PersistentMap.<Integer, T2>empty())))), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("null", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("empty map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.maps.Null.apply((hydra.util.PersistentMap<T3, T4>) ((hydra.util.PersistentMap<T3, T4>) (hydra.util.PersistentMap.<T3, T4>empty())))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("non-empty map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.maps.Null.apply(hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
          1,
          "a")))), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("remove", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("remove existing", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Delete.apply(
            2,
            hydra.util.PersistentMap.ofEntries(
              hydra.util.PersistentMap.entry(
                1,
                "a"),
              hydra.util.PersistentMap.entry(
                2,
                "b"),
              hydra.util.PersistentMap.entry(
                3,
                "c")))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.util.PersistentMap.ofEntries(
            hydra.util.PersistentMap.entry(
              1,
              "a"),
            hydra.util.PersistentMap.entry(
              3,
              "c"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("remove non-existing", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Delete.apply(
            4,
            hydra.util.PersistentMap.ofEntries(
              hydra.util.PersistentMap.entry(
                1,
                "a"),
              hydra.util.PersistentMap.entry(
                2,
                "b")))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.util.PersistentMap.ofEntries(
            hydra.util.PersistentMap.entry(
              1,
              "a"),
            hydra.util.PersistentMap.entry(
              2,
              "b"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("remove from empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Delete.apply(
            1,
            (hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.util.PersistentMap.<Integer, String>empty())))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.util.PersistentMap.<Integer, String>empty()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("singleton", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(new hydra.testing.TestCaseWithMetadata("single entry", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
        (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
        (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
        hydra.lib.maps.Singleton.apply(
          42,
          "hello")), hydra.show.Core.map(
        (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
        (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
        hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
          42,
          "hello"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("size", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("three entries", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.maps.Size.apply(hydra.util.PersistentMap.ofEntries(
          hydra.util.PersistentMap.entry(
            1,
            "a"),
          hydra.util.PersistentMap.entry(
            2,
            "b"),
          hydra.util.PersistentMap.entry(
            3,
            "c")))), hydra.lib.literals.ShowInt32.apply(3))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("single entry", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.maps.Size.apply(hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
          42,
          "test")))), hydra.lib.literals.ShowInt32.apply(1))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.maps.Size.apply((hydra.util.PersistentMap<T5, T6>) ((hydra.util.PersistentMap<T5, T6>) (hydra.util.PersistentMap.<T5, T6>empty())))), hydra.lib.literals.ShowInt32.apply(0))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("toList", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("convert to pairs", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<hydra.util.Pair<Integer, String>, String>) (p -> hydra.show.Core.pair(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            p)),
          hydra.lib.maps.ToList.apply(hydra.util.PersistentMap.ofEntries(
            hydra.util.PersistentMap.entry(
              1,
              "a"),
            hydra.util.PersistentMap.entry(
              2,
              "b")))), hydra.show.Core.list(
          (java.util.function.Function<hydra.util.Pair<Integer, String>, String>) (p -> hydra.show.Core.pair(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            p)),
          hydra.util.ConsList.of(
            (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(1, "a"))),
            (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(2, "b"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("unsorted keys", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<hydra.util.Pair<Integer, String>, String>) (p -> hydra.show.Core.pair(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            p)),
          hydra.lib.maps.ToList.apply(hydra.util.PersistentMap.ofEntries(
            hydra.util.PersistentMap.entry(
              1,
              "a"),
            hydra.util.PersistentMap.entry(
              2,
              "b"),
            hydra.util.PersistentMap.entry(
              3,
              "c")))), hydra.show.Core.list(
          (java.util.function.Function<hydra.util.Pair<Integer, String>, String>) (p -> hydra.show.Core.pair(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            p)),
          hydra.util.ConsList.of(
            (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(1, "a"))),
            (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(2, "b"))),
            (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(3, "c"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<hydra.util.Pair<Integer, String>, String>) (p -> hydra.show.Core.pair(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            p)),
          hydra.lib.maps.ToList.apply((hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.util.PersistentMap.<Integer, String>empty())))), hydra.show.Core.list(
          (java.util.function.Function<hydra.util.Pair<Integer, String>, String>) (p -> hydra.show.Core.pair(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            p)),
          (hydra.util.ConsList<hydra.util.Pair<Integer, String>>) (hydra.util.ConsList.<hydra.util.Pair<Integer, String>>empty())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())))),
      new hydra.testing.TestGroup("union", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), hydra.util.ConsList.of(
        new hydra.testing.TestCaseWithMetadata("union two maps", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Union.apply(
            hydra.util.PersistentMap.ofEntries(
              hydra.util.PersistentMap.entry(
                1,
                "a"),
              hydra.util.PersistentMap.entry(
                2,
                "b")),
            hydra.util.PersistentMap.ofEntries(
              hydra.util.PersistentMap.entry(
                2,
                "x"),
              hydra.util.PersistentMap.entry(
                3,
                "c")))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.util.PersistentMap.ofEntries(
            hydra.util.PersistentMap.entry(
              1,
              "a"),
            hydra.util.PersistentMap.entry(
              2,
              "b"),
            hydra.util.PersistentMap.entry(
              3,
              "c"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("union with empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Union.apply(
            hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
              1,
              "a")),
            (hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.util.PersistentMap.<Integer, String>empty())))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
            1,
            "a"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty())),
        new hydra.testing.TestCaseWithMetadata("empty with map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Union.apply(
            (hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.util.PersistentMap.<Integer, String>empty())),
            hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
              1,
              "a")))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
            1,
            "a"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.Tag>) (hydra.util.ConsList.<hydra.testing.Tag>empty()))))), (hydra.util.ConsList<hydra.testing.TestCaseWithMetadata>) (hydra.util.ConsList.<hydra.testing.TestCaseWithMetadata>empty()));
  }
}

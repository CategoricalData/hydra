// Note: this is an automatically generated file. Do not edit.

package hydra.test.lib;

/**
 * Test cases for hydra.lib.maps primitives
 */
public interface Maps {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("hydra.lib.maps primitives", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("alter", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("insert new key", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Alter.apply(
            (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Maybe<String>>) (ignored -> hydra.util.Maybe.just("new")),
            3,
            new java.util.TreeMap(java.util.Map.ofEntries(
              java.util.Map.entry(
                1,
                "a"),
              java.util.Map.entry(
                2,
                "b"))))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              1,
              "a"),
            java.util.Map.entry(
              2,
              "b"),
            java.util.Map.entry(
              3,
              "new")))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("update existing key", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Alter.apply(
            (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Maybe<String>>) (ignored -> hydra.util.Maybe.just("updated")),
            2,
            new java.util.TreeMap(java.util.Map.ofEntries(
              java.util.Map.entry(
                1,
                "a"),
              java.util.Map.entry(
                2,
                "b"))))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              1,
              "a"),
            java.util.Map.entry(
              2,
              "updated")))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("delete key", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Alter.apply(
            (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Maybe<String>>) (ignored -> (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())),
            2,
            new java.util.TreeMap(java.util.Map.ofEntries(
              java.util.Map.entry(
                1,
                "a"),
              java.util.Map.entry(
                2,
                "b"))))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            1,
            "a")))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("bimap", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("transform both", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Bimap.apply(
            (java.util.function.Function<Integer, Integer>) (k -> hydra.lib.math.Mul.apply(
              k,
              2)),
            (java.util.function.Function<String, String>) (v -> hydra.lib.strings.ToUpper.apply(v)),
            new java.util.TreeMap(java.util.Map.ofEntries(
              java.util.Map.entry(
                1,
                "a"),
              java.util.Map.entry(
                2,
                "b"))))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              2,
              "A"),
            java.util.Map.entry(
              4,
              "B")))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Bimap.apply(
            (java.util.function.Function<Integer, Integer>) (k -> hydra.lib.math.Mul.apply(
              k,
              2)),
            (java.util.function.Function<String, String>) (v -> hydra.lib.strings.ToUpper.apply(v)),
            (java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (java.util.Collections.<Integer, String>emptyMap())))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (java.util.Collections.<Integer, String>emptyMap()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("elems", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("get all elements", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Elems.apply(new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              1,
              "a"),
            java.util.Map.entry(
              2,
              "b"))))), hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          java.util.Arrays.asList(
            "a",
            "b")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("unsorted keys", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Elems.apply(new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              1,
              "a"),
            java.util.Map.entry(
              2,
              "b"),
            java.util.Map.entry(
              3,
              "c"))))), hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          java.util.Arrays.asList(
            "a",
            "b",
            "c")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Elems.apply((java.util.Map<java.lang.Void, String>) ((java.util.Map<java.lang.Void, String>) (java.util.Collections.<java.lang.Void, String>emptyMap())))), hydra.show.Core.list(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.List<String>) (java.util.Collections.<String>emptyList())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("empty", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("empty map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
        (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
        (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
        (java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (hydra.lib.maps.Empty.<Integer, String>apply()))), hydra.show.Core.map(
        (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
        (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
        (java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (java.util.Collections.<Integer, String>emptyMap()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("filter", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("filter values starting with a", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Filter.apply(
            (java.util.function.Function<String, Boolean>) (v -> hydra.lib.equality.Equal.apply(
              hydra.lib.maybes.FromMaybe.applyLazy(
                () -> 0,
                hydra.lib.maybes.Map.apply(
                  (java.util.function.Function<Integer, Integer>) (c -> hydra.lib.chars.ToLower.apply(c)),
                  hydra.lib.strings.MaybeCharAt.apply(
                    0,
                    v))),
              97)),
            new java.util.TreeMap(java.util.Map.ofEntries(
              java.util.Map.entry(
                1,
                "a"),
              java.util.Map.entry(
                2,
                "b"),
              java.util.Map.entry(
                3,
                "ab"))))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              1,
              "a"),
            java.util.Map.entry(
              3,
              "ab")))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("filter all", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Filter.apply(
            (java.util.function.Function<String, Boolean>) (v -> hydra.lib.equality.Equal.apply(
              hydra.lib.maybes.FromMaybe.applyLazy(
                () -> 0,
                hydra.lib.maybes.Map.apply(
                  (java.util.function.Function<Integer, Integer>) (c -> hydra.lib.chars.ToLower.apply(c)),
                  hydra.lib.strings.MaybeCharAt.apply(
                    0,
                    v))),
              97)),
            new java.util.TreeMap(java.util.Map.ofEntries(
              java.util.Map.entry(
                1,
                "b"),
              java.util.Map.entry(
                2,
                "c"))))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (java.util.Collections.<Integer, String>emptyMap()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Filter.apply(
            (java.util.function.Function<String, Boolean>) (v -> hydra.lib.equality.Equal.apply(
              hydra.lib.maybes.FromMaybe.applyLazy(
                () -> 0,
                hydra.lib.maybes.Map.apply(
                  (java.util.function.Function<Integer, Integer>) (c -> hydra.lib.chars.ToLower.apply(c)),
                  hydra.lib.strings.MaybeCharAt.apply(
                    0,
                    v))),
              97)),
            (java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (java.util.Collections.<Integer, String>emptyMap())))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (java.util.Collections.<Integer, String>emptyMap()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("filterWithKey", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("filter by key > 1", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.FilterWithKey.apply(
            (java.util.function.Function<Integer, java.util.function.Function<String, Boolean>>) (k -> (java.util.function.Function<String, Boolean>) (v -> hydra.lib.equality.Gt.apply(
              k,
              1))),
            new java.util.TreeMap(java.util.Map.ofEntries(
              java.util.Map.entry(
                1,
                "a"),
              java.util.Map.entry(
                2,
                "b"),
              java.util.Map.entry(
                3,
                "c"))))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              2,
              "b"),
            java.util.Map.entry(
              3,
              "c")))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("filter all", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.FilterWithKey.apply(
            (java.util.function.Function<Integer, java.util.function.Function<String, Boolean>>) (k -> (java.util.function.Function<String, Boolean>) (v -> hydra.lib.equality.Gt.apply(
              k,
              1))),
            new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
              1,
              "a"))))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (java.util.Collections.<Integer, String>emptyMap()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.FilterWithKey.apply(
            (java.util.function.Function<Integer, java.util.function.Function<String, Boolean>>) (k -> (java.util.function.Function<String, Boolean>) (v -> hydra.lib.equality.Gt.apply(
              k,
              1))),
            (java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (java.util.Collections.<Integer, String>emptyMap())))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (java.util.Collections.<Integer, String>emptyMap()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("findWithDefault", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("find existing", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.maps.FindWithDefault.applyLazy(
          () -> "default",
          2,
          new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              1,
              "a"),
            java.util.Map.entry(
              2,
              "b")))), "b")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("use default", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.maps.FindWithDefault.applyLazy(
          () -> "default",
          3,
          new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              1,
              "a"),
            java.util.Map.entry(
              2,
              "b")))), "default")), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("fromList", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("create from pairs", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(1, "a"))),
            (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(2, "b")))))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              1,
              "a"),
            java.util.Map.entry(
              2,
              "b")))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("duplicate keys", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(1, "a"))),
            (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(1, "b")))))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            1,
            "b")))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.FromList.apply((java.util.List<hydra.util.Pair<Integer, String>>) (java.util.Collections.<hydra.util.Pair<Integer, String>>emptyList()))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (java.util.Collections.<Integer, String>emptyMap()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("insert", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("insert new key", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Insert.apply(
            3,
            "c",
            new java.util.TreeMap(java.util.Map.ofEntries(
              java.util.Map.entry(
                1,
                "a"),
              java.util.Map.entry(
                2,
                "b"))))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              1,
              "a"),
            java.util.Map.entry(
              2,
              "b"),
            java.util.Map.entry(
              3,
              "c")))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("update existing", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Insert.apply(
            2,
            "updated",
            new java.util.TreeMap(java.util.Map.ofEntries(
              java.util.Map.entry(
                1,
                "a"),
              java.util.Map.entry(
                2,
                "b"))))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              1,
              "a"),
            java.util.Map.entry(
              2,
              "updated")))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("insert into empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Insert.apply(
            1,
            "x",
            (java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (java.util.Collections.<Integer, String>emptyMap())))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            1,
            "x")))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("keys", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("get all keys", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maps.Keys.apply(new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              1,
              "a"),
            java.util.Map.entry(
              2,
              "b"),
            java.util.Map.entry(
              3,
              "c"))))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          java.util.Arrays.asList(
            1,
            2,
            3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("unsorted keys", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maps.Keys.apply(new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              1,
              "a"),
            java.util.Map.entry(
              2,
              "b"),
            java.util.Map.entry(
              3,
              "c"))))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          java.util.Arrays.asList(
            1,
            2,
            3)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          hydra.lib.maps.Keys.apply((java.util.Map<Integer, java.lang.Void>) ((java.util.Map<Integer, java.lang.Void>) (java.util.Collections.<Integer, java.lang.Void>emptyMap())))), hydra.show.Core.list(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.List<Integer>) (java.util.Collections.<Integer>emptyList())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("lookup", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("find existing key", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.maybe(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Lookup.apply(
            2,
            new java.util.TreeMap(java.util.Map.ofEntries(
              java.util.Map.entry(
                1,
                "a"),
              java.util.Map.entry(
                2,
                "b"))))), hydra.show.Core.maybe(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.util.Maybe.just("b")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("key not found", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.maybe(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Lookup.apply(
            3,
            new java.util.TreeMap(java.util.Map.ofEntries(
              java.util.Map.entry(
                1,
                "a"),
              java.util.Map.entry(
                2,
                "b"))))), hydra.show.Core.maybe(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("lookup in empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.maybe(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Lookup.apply(
            1,
            (java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (java.util.Collections.<Integer, String>emptyMap())))), hydra.show.Core.maybe(
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("map", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("map over values", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Map.apply(
            (java.util.function.Function<String, String>) (s -> hydra.lib.strings.ToUpper.apply(s)),
            new java.util.TreeMap(java.util.Map.ofEntries(
              java.util.Map.entry(
                1,
                "a"),
              java.util.Map.entry(
                2,
                "b"))))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              1,
              "A"),
            java.util.Map.entry(
              2,
              "B")))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("map empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Map.apply(
            (java.util.function.Function<String, String>) (s -> hydra.lib.strings.ToUpper.apply(s)),
            (java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (java.util.Collections.<Integer, String>emptyMap())))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (java.util.Collections.<Integer, String>emptyMap()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("mapKeys", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("double keys", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.MapKeys.apply(
            (java.util.function.Function<Integer, Integer>) (k -> hydra.lib.math.Mul.apply(
              k,
              2)),
            new java.util.TreeMap(java.util.Map.ofEntries(
              java.util.Map.entry(
                1,
                "a"),
              java.util.Map.entry(
                2,
                "b"))))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              2,
              "a"),
            java.util.Map.entry(
              4,
              "b")))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.MapKeys.apply(
            (java.util.function.Function<Integer, Integer>) (k -> hydra.lib.math.Mul.apply(
              k,
              2)),
            (java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (java.util.Collections.<Integer, String>emptyMap())))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (java.util.Collections.<Integer, String>emptyMap()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("member", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("key exists", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.maps.Member.apply(
          2,
          new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              1,
              "a"),
            java.util.Map.entry(
              2,
              "b"))))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("key missing", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.maps.Member.apply(
          3,
          new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              1,
              "a"),
            java.util.Map.entry(
              2,
              "b"))))), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.maps.Member.apply(
          1,
          (java.util.Map<Integer, java.lang.Void>) ((java.util.Map<Integer, java.lang.Void>) (java.util.Collections.<Integer, java.lang.Void>emptyMap())))), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("null", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("empty map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.maps.Null.apply((java.util.Map<java.lang.Void, java.lang.Void>) ((java.util.Map<java.lang.Void, java.lang.Void>) (java.util.Collections.<java.lang.Void, java.lang.Void>emptyMap())))), hydra.lib.literals.ShowBoolean.apply(true))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("non-empty map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowBoolean.apply(hydra.lib.maps.Null.apply(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          1,
          "a"))))), hydra.lib.literals.ShowBoolean.apply(false))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("remove", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("remove existing", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Delete.apply(
            2,
            new java.util.TreeMap(java.util.Map.ofEntries(
              java.util.Map.entry(
                1,
                "a"),
              java.util.Map.entry(
                2,
                "b"),
              java.util.Map.entry(
                3,
                "c"))))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              1,
              "a"),
            java.util.Map.entry(
              3,
              "c")))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("remove non-existing", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Delete.apply(
            4,
            new java.util.TreeMap(java.util.Map.ofEntries(
              java.util.Map.entry(
                1,
                "a"),
              java.util.Map.entry(
                2,
                "b"))))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              1,
              "a"),
            java.util.Map.entry(
              2,
              "b")))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("remove from empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Delete.apply(
            1,
            (java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (java.util.Collections.<Integer, String>emptyMap())))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          (java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (java.util.Collections.<Integer, String>emptyMap()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("singleton", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(new hydra.testing.TestCaseWithMetadata("single entry", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
        (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
        (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
        hydra.lib.maps.Singleton.apply(
          42,
          "hello")), hydra.show.Core.map(
        (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
        (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
        new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          42,
          "hello")))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("size", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("three entries", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.maps.Size.apply(new java.util.TreeMap(java.util.Map.ofEntries(
          java.util.Map.entry(
            1,
            "a"),
          java.util.Map.entry(
            2,
            "b"),
          java.util.Map.entry(
            3,
            "c"))))), hydra.lib.literals.ShowInt32.apply(3))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("single entry", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.maps.Size.apply(new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
          42,
          "test"))))), hydra.lib.literals.ShowInt32.apply(1))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.literals.ShowInt32.apply(hydra.lib.maps.Size.apply((java.util.Map<java.lang.Void, java.lang.Void>) ((java.util.Map<java.lang.Void, java.lang.Void>) (java.util.Collections.<java.lang.Void, java.lang.Void>emptyMap())))), hydra.lib.literals.ShowInt32.apply(0))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("toList", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("convert to pairs", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<hydra.util.Pair<Integer, String>, String>) (p -> hydra.show.Core.pair(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            p)),
          hydra.lib.maps.ToList.apply(new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              1,
              "a"),
            java.util.Map.entry(
              2,
              "b"))))), hydra.show.Core.list(
          (java.util.function.Function<hydra.util.Pair<Integer, String>, String>) (p -> hydra.show.Core.pair(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            p)),
          java.util.Arrays.asList(
            (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(1, "a"))),
            (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(2, "b"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("unsorted keys", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<hydra.util.Pair<Integer, String>, String>) (p -> hydra.show.Core.pair(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            p)),
          hydra.lib.maps.ToList.apply(new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              1,
              "a"),
            java.util.Map.entry(
              2,
              "b"),
            java.util.Map.entry(
              3,
              "c"))))), hydra.show.Core.list(
          (java.util.function.Function<hydra.util.Pair<Integer, String>, String>) (p -> hydra.show.Core.pair(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            p)),
          java.util.Arrays.asList(
            (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(1, "a"))),
            (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(2, "b"))),
            (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(3, "c"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.list(
          (java.util.function.Function<hydra.util.Pair<Integer, String>, String>) (p -> hydra.show.Core.pair(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            p)),
          hydra.lib.maps.ToList.apply((java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (java.util.Collections.<Integer, String>emptyMap())))), hydra.show.Core.list(
          (java.util.function.Function<hydra.util.Pair<Integer, String>, String>) (p -> hydra.show.Core.pair(
            (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
            (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
            p)),
          (java.util.List<hydra.util.Pair<Integer, String>>) (java.util.Collections.<hydra.util.Pair<Integer, String>>emptyList())))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("union", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("union two maps", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Union.apply(
            new java.util.TreeMap(java.util.Map.ofEntries(
              java.util.Map.entry(
                1,
                "a"),
              java.util.Map.entry(
                2,
                "b"))),
            new java.util.TreeMap(java.util.Map.ofEntries(
              java.util.Map.entry(
                2,
                "x"),
              java.util.Map.entry(
                3,
                "c"))))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          new java.util.TreeMap(java.util.Map.ofEntries(
            java.util.Map.entry(
              1,
              "a"),
            java.util.Map.entry(
              2,
              "b"),
            java.util.Map.entry(
              3,
              "c")))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("union with empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Union.apply(
            new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
              1,
              "a"))),
            (java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (java.util.Collections.<Integer, String>emptyMap())))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            1,
            "a")))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty with map", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          hydra.lib.maps.Union.apply(
            (java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (java.util.Collections.<Integer, String>emptyMap())),
            new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
              1,
              "a"))))), hydra.show.Core.map(
          (java.util.function.Function<Integer, String>) (n -> hydra.lib.literals.ShowInt32.apply(n)),
          (java.util.function.Function<String, String>) (s -> hydra.lib.literals.ShowString.apply(s)),
          new java.util.TreeMap(java.util.Map.ofEntries(java.util.Map.entry(
            1,
            "a")))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }
}

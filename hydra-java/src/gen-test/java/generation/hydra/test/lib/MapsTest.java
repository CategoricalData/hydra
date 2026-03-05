// Note: this is an automatically generated file. Do not edit.
// hydra.lib.maps primitives

package generation.hydra.test.lib;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import java.util.*;
import hydra.util.*;

public class MapsTest {

    // alter

    @Test

    public void testAlterInsertNewKey() {

        assertEquals(

            java.util.Map.ofEntries(
  java.util.Map.entry(
    1,
    "a"),
  java.util.Map.entry(
    2,
    "b"),
  java.util.Map.entry(
    3,
    "new")),

            hydra.lib.maps.Alter.apply(
  (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Maybe<String>>) (opt -> hydra.util.Maybe.just("new")),
  3,
  java.util.Map.ofEntries(
    java.util.Map.entry(
      1,
      "a"),
    java.util.Map.entry(
      2,
      "b"))));

    }

    @Test

    public void testAlterUpdateExistingKey() {

        assertEquals(

            java.util.Map.ofEntries(
  java.util.Map.entry(
    1,
    "a"),
  java.util.Map.entry(
    2,
    "updated")),

            hydra.lib.maps.Alter.apply(
  (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Maybe<String>>) (opt -> hydra.util.Maybe.just("updated")),
  2,
  java.util.Map.ofEntries(
    java.util.Map.entry(
      1,
      "a"),
    java.util.Map.entry(
      2,
      "b"))));

    }

    @Test

    public void testAlterDeleteKey() {

        assertEquals(

            java.util.Map.ofEntries(java.util.Map.entry(
  1,
  "a")),

            hydra.lib.maps.Alter.apply(
  (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Maybe<String>>) (opt -> (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())),
  2,
  java.util.Map.ofEntries(
    java.util.Map.entry(
      1,
      "a"),
    java.util.Map.entry(
      2,
      "b"))));

    }

    // bimap

    @Test

    public void testBimapTransformBoth() {

        assertEquals(

            java.util.Map.ofEntries(
  java.util.Map.entry(
    2,
    "A"),
  java.util.Map.entry(
    4,
    "B")),

            hydra.lib.maps.Bimap.apply(
  (java.util.function.Function<Integer, Integer>) (k -> hydra.lib.math.Mul.apply(
    k,
    2)),
  (java.util.function.Function<String, String>) (v -> hydra.lib.strings.ToUpper.apply(v)),
  java.util.Map.ofEntries(
    java.util.Map.entry(
      1,
      "a"),
    java.util.Map.entry(
      2,
      "b"))));

    }

    @Test

    public <T0, T1> void testBimapEmptyMap() {

        assertEquals(

            (java.util.Map<T0, T1>) ((java.util.Map<T0, T1>) (java.util.Map.<T0, T1>ofEntries())),

            hydra.lib.maps.Bimap.apply(
  (java.util.function.Function<Integer, Integer>) (k -> hydra.lib.math.Mul.apply(
    k,
    2)),
  (java.util.function.Function<String, String>) (v -> hydra.lib.strings.ToUpper.apply(v)),
  (java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (java.util.Map.<Integer, String>ofEntries()))));

    }

    // elems

    @Test

    public void testElemsGetAllElements() {

        assertEquals(

            java.util.List.of(
  "a",
  "b"),

            hydra.lib.maps.Elems.apply(java.util.Map.ofEntries(
  java.util.Map.entry(
    1,
    "a"),
  java.util.Map.entry(
    2,
    "b"))));

    }

    @Test

    public void testElemsUnsortedKeys() {

        assertEquals(

            java.util.List.of(
  "a",
  "b",
  "c"),

            hydra.lib.maps.Elems.apply(java.util.Map.ofEntries(
  java.util.Map.entry(
    1,
    "a"),
  java.util.Map.entry(
    2,
    "b"),
  java.util.Map.entry(
    3,
    "c"))));

    }

    @Test

    public <T0, T2, T3> void testElemsEmptyMap() {

        assertEquals(

            (java.util.List<T0>) (java.util.List.<T0>of()),

            hydra.lib.maps.Elems.apply((java.util.Map<T2, T3>) ((java.util.Map<T2, T3>) (java.util.Map.<T2, T3>ofEntries()))));

    }

    // empty

    @Test

    public <T0, T1> void testEmptyEmptyMap() {

        assertEquals(

            (java.util.Map<T0, T1>) ((java.util.Map<T0, T1>) (java.util.Map.<T0, T1>ofEntries())),

            (java.util.Map<T0, T1>) ((java.util.Map<T0, T1>) (hydra.lib.maps.Empty.<T0, T1>apply())));

    }

    // filter

    @Test

    public void testFilterFilterValuesStartingWithA() {

        assertEquals(

            java.util.Map.ofEntries(
  java.util.Map.entry(
    1,
    "a"),
  java.util.Map.entry(
    3,
    "ab")),

            hydra.lib.maps.Filter.apply(
  (java.util.function.Function<String, Boolean>) (v -> hydra.lib.equality.Equal.apply(
    hydra.lib.strings.CharAt.apply(
      0,
      v),
    97)),
  java.util.Map.ofEntries(
    java.util.Map.entry(
      1,
      "a"),
    java.util.Map.entry(
      2,
      "b"),
    java.util.Map.entry(
      3,
      "ab"))));

    }

    @Test

    public <T0, T1> void testFilterFilterAll() {

        assertEquals(

            (java.util.Map<T0, T1>) ((java.util.Map<T0, T1>) (java.util.Map.<T0, T1>ofEntries())),

            hydra.lib.maps.Filter.apply(
  (java.util.function.Function<String, Boolean>) (v -> hydra.lib.equality.Equal.apply(
    hydra.lib.strings.CharAt.apply(
      0,
      v),
    97)),
  java.util.Map.ofEntries(
    java.util.Map.entry(
      1,
      "b"),
    java.util.Map.entry(
      2,
      "c"))));

    }

    @Test

    public <T0, T1, T9> void testFilterEmptyMap() {

        assertEquals(

            (java.util.Map<T0, T1>) ((java.util.Map<T0, T1>) (java.util.Map.<T0, T1>ofEntries())),

            hydra.lib.maps.Filter.apply(
  (java.util.function.Function<String, Boolean>) (v -> hydra.lib.equality.Equal.apply(
    hydra.lib.strings.CharAt.apply(
      0,
      v),
    97)),
  (java.util.Map<T9, String>) ((java.util.Map<T9, String>) (java.util.Map.<T9, String>ofEntries()))));

    }

    // filterWithKey

    @Test

    public void testFilterwithkeyFilterByKey1() {

        assertEquals(

            java.util.Map.ofEntries(
  java.util.Map.entry(
    2,
    "b"),
  java.util.Map.entry(
    3,
    "c")),

            hydra.lib.maps.FilterWithKey.apply(
  (java.util.function.Function<Integer, java.util.function.Function<String, Boolean>>) (k -> (java.util.function.Function<String, Boolean>) (v -> hydra.lib.equality.Gt.apply(
    k,
    1))),
  java.util.Map.ofEntries(
    java.util.Map.entry(
      1,
      "a"),
    java.util.Map.entry(
      2,
      "b"),
    java.util.Map.entry(
      3,
      "c"))));

    }

    @Test

    public <T0, T1> void testFilterwithkeyFilterAll() {

        assertEquals(

            (java.util.Map<T0, T1>) ((java.util.Map<T0, T1>) (java.util.Map.<T0, T1>ofEntries())),

            hydra.lib.maps.FilterWithKey.apply(
  (java.util.function.Function<Integer, java.util.function.Function<String, Boolean>>) (k -> (java.util.function.Function<String, Boolean>) (v -> hydra.lib.equality.Gt.apply(
    k,
    1))),
  java.util.Map.ofEntries(java.util.Map.entry(
    1,
    "a"))));

    }

    @Test

    public <T0, T1, T9> void testFilterwithkeyEmptyMap() {

        assertEquals(

            (java.util.Map<T0, T1>) ((java.util.Map<T0, T1>) (java.util.Map.<T0, T1>ofEntries())),

            hydra.lib.maps.FilterWithKey.apply(
  (java.util.function.Function<Integer, java.util.function.Function<T9, Boolean>>) (k -> (java.util.function.Function<T9, Boolean>) (v -> hydra.lib.equality.Gt.apply(
    k,
    1))),
  (java.util.Map<Integer, T9>) ((java.util.Map<Integer, T9>) (java.util.Map.<Integer, T9>ofEntries()))));

    }

    // findWithDefault

    @Test

    public void testFindwithdefaultFindExisting() {

        assertEquals(

            "b",

            hydra.lib.maps.FindWithDefault.apply(
  "default",
  2,
  java.util.Map.ofEntries(
    java.util.Map.entry(
      1,
      "a"),
    java.util.Map.entry(
      2,
      "b"))));

    }

    @Test

    public void testFindwithdefaultUseDefault() {

        assertEquals(

            "default",

            hydra.lib.maps.FindWithDefault.apply(
  "default",
  3,
  java.util.Map.ofEntries(
    java.util.Map.entry(
      1,
      "a"),
    java.util.Map.entry(
      2,
      "b"))));

    }

    // fromList

    @Test

    public void testFromlistCreateFromPairs() {

        assertEquals(

            java.util.Map.ofEntries(
  java.util.Map.entry(
    1,
    "a"),
  java.util.Map.entry(
    2,
    "b")),

            hydra.lib.maps.FromList.apply(java.util.List.of(
  (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(1, "a"))),
  (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(2, "b"))))));

    }

    @Test

    public void testFromlistDuplicateKeys() {

        assertEquals(

            java.util.Map.ofEntries(java.util.Map.entry(
  1,
  "b")),

            hydra.lib.maps.FromList.apply(java.util.List.of(
  (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(1, "a"))),
  (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(1, "b"))))));

    }

    @Test

    public <T0, T1> void testFromlistEmptyList() {

        assertEquals(

            (java.util.Map<T0, T1>) ((java.util.Map<T0, T1>) (java.util.Map.<T0, T1>ofEntries())),

            hydra.lib.maps.FromList.apply((java.util.List<hydra.util.Pair<T0, T1>>) (java.util.List.<hydra.util.Pair<T0, T1>>of())));

    }

    // insert

    @Test

    public void testInsertInsertNewKey() {

        assertEquals(

            java.util.Map.ofEntries(
  java.util.Map.entry(
    1,
    "a"),
  java.util.Map.entry(
    2,
    "b"),
  java.util.Map.entry(
    3,
    "c")),

            hydra.lib.maps.Insert.apply(
  3,
  "c",
  java.util.Map.ofEntries(
    java.util.Map.entry(
      1,
      "a"),
    java.util.Map.entry(
      2,
      "b"))));

    }

    @Test

    public void testInsertUpdateExisting() {

        assertEquals(

            java.util.Map.ofEntries(
  java.util.Map.entry(
    1,
    "a"),
  java.util.Map.entry(
    2,
    "updated")),

            hydra.lib.maps.Insert.apply(
  2,
  "updated",
  java.util.Map.ofEntries(
    java.util.Map.entry(
      1,
      "a"),
    java.util.Map.entry(
      2,
      "b"))));

    }

    @Test

    public void testInsertInsertIntoEmpty() {

        assertEquals(

            java.util.Map.ofEntries(java.util.Map.entry(
  1,
  "x")),

            hydra.lib.maps.Insert.apply(
  1,
  "x",
  (java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (java.util.Map.<Integer, String>ofEntries()))));

    }

    // keys

    @Test

    public void testKeysGetAllKeys() {

        assertEquals(

            java.util.List.of(
  1,
  2,
  3),

            hydra.lib.maps.Keys.apply(java.util.Map.ofEntries(
  java.util.Map.entry(
    1,
    "a"),
  java.util.Map.entry(
    2,
    "b"),
  java.util.Map.entry(
    3,
    "c"))));

    }

    @Test

    public void testKeysUnsortedKeys() {

        assertEquals(

            java.util.List.of(
  1,
  2,
  3),

            hydra.lib.maps.Keys.apply(java.util.Map.ofEntries(
  java.util.Map.entry(
    1,
    "a"),
  java.util.Map.entry(
    2,
    "b"),
  java.util.Map.entry(
    3,
    "c"))));

    }

    @Test

    public <T0, T2, T3> void testKeysEmptyMap() {

        assertEquals(

            (java.util.List<T0>) (java.util.List.<T0>of()),

            hydra.lib.maps.Keys.apply((java.util.Map<T2, T3>) ((java.util.Map<T2, T3>) (java.util.Map.<T2, T3>ofEntries()))));

    }

    // lookup

    @Test

    public void testLookupFindExistingKey() {

        assertEquals(

            hydra.util.Maybe.just("b"),

            hydra.lib.maps.Lookup.apply(
  2,
  java.util.Map.ofEntries(
    java.util.Map.entry(
      1,
      "a"),
    java.util.Map.entry(
      2,
      "b"))));

    }

    @Test

    public <T0> void testLookupKeyNotFound() {

        assertEquals(

            (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()),

            hydra.lib.maps.Lookup.apply(
  3,
  java.util.Map.ofEntries(
    java.util.Map.entry(
      1,
      "a"),
    java.util.Map.entry(
      2,
      "b"))));

    }

    @Test

    public <T0, T4> void testLookupLookupInEmpty() {

        assertEquals(

            (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()),

            hydra.lib.maps.Lookup.apply(
  1,
  (java.util.Map<Integer, T4>) ((java.util.Map<Integer, T4>) (java.util.Map.<Integer, T4>ofEntries()))));

    }

    // map

    @Test

    public void testMapMapOverValues() {

        assertEquals(

            java.util.Map.ofEntries(
  java.util.Map.entry(
    1,
    "A"),
  java.util.Map.entry(
    2,
    "B")),

            hydra.lib.maps.Map.apply(
  (java.util.function.Function<String, String>) (s -> hydra.lib.strings.ToUpper.apply(s)),
  java.util.Map.ofEntries(
    java.util.Map.entry(
      1,
      "a"),
    java.util.Map.entry(
      2,
      "b"))));

    }

    @Test

    public <T0, T1, T6> void testMapMapEmpty() {

        assertEquals(

            (java.util.Map<T0, T1>) ((java.util.Map<T0, T1>) (java.util.Map.<T0, T1>ofEntries())),

            hydra.lib.maps.Map.apply(
  (java.util.function.Function<String, String>) (s -> hydra.lib.strings.ToUpper.apply(s)),
  (java.util.Map<T6, String>) ((java.util.Map<T6, String>) (java.util.Map.<T6, String>ofEntries()))));

    }

    // mapKeys

    @Test

    public void testMapkeysDoubleKeys() {

        assertEquals(

            java.util.Map.ofEntries(
  java.util.Map.entry(
    2,
    "a"),
  java.util.Map.entry(
    4,
    "b")),

            hydra.lib.maps.MapKeys.apply(
  (java.util.function.Function<Integer, Integer>) (k -> hydra.lib.math.Mul.apply(
    k,
    2)),
  java.util.Map.ofEntries(
    java.util.Map.entry(
      1,
      "a"),
    java.util.Map.entry(
      2,
      "b"))));

    }

    @Test

    public <T0, T1, T8> void testMapkeysEmptyMap() {

        assertEquals(

            (java.util.Map<T0, T1>) ((java.util.Map<T0, T1>) (java.util.Map.<T0, T1>ofEntries())),

            hydra.lib.maps.MapKeys.apply(
  (java.util.function.Function<Integer, Integer>) (k -> hydra.lib.math.Mul.apply(
    k,
    2)),
  (java.util.Map<Integer, T8>) ((java.util.Map<Integer, T8>) (java.util.Map.<Integer, T8>ofEntries()))));

    }

    // member

    @Test

    public void testMemberKeyExists() {

        assertEquals(

            true,

            hydra.lib.maps.Member.apply(
  2,
  java.util.Map.ofEntries(
    java.util.Map.entry(
      1,
      "a"),
    java.util.Map.entry(
      2,
      "b"))));

    }

    @Test

    public void testMemberKeyMissing() {

        assertEquals(

            false,

            hydra.lib.maps.Member.apply(
  3,
  java.util.Map.ofEntries(
    java.util.Map.entry(
      1,
      "a"),
    java.util.Map.entry(
      2,
      "b"))));

    }

    @Test

    public <T4> void testMemberEmptyMap() {

        assertEquals(

            false,

            hydra.lib.maps.Member.apply(
  1,
  (java.util.Map<Integer, T4>) ((java.util.Map<Integer, T4>) (java.util.Map.<Integer, T4>ofEntries()))));

    }

    // null

    @Test

    public <T2, T3> void testNullEmptyMap() {

        assertEquals(

            true,

            hydra.lib.maps.Null.apply((java.util.Map<T2, T3>) ((java.util.Map<T2, T3>) (java.util.Map.<T2, T3>ofEntries()))));

    }

    @Test

    public void testNullNonNegemptyMap() {

        assertEquals(

            false,

            hydra.lib.maps.Null.apply(java.util.Map.ofEntries(java.util.Map.entry(
  1,
  "a"))));

    }

    // remove

    @Test

    public void testRemoveRemoveExisting() {

        assertEquals(

            java.util.Map.ofEntries(
  java.util.Map.entry(
    1,
    "a"),
  java.util.Map.entry(
    3,
    "c")),

            hydra.lib.maps.Delete.apply(
  2,
  java.util.Map.ofEntries(
    java.util.Map.entry(
      1,
      "a"),
    java.util.Map.entry(
      2,
      "b"),
    java.util.Map.entry(
      3,
      "c"))));

    }

    @Test

    public void testRemoveRemoveNonNegexisting() {

        assertEquals(

            java.util.Map.ofEntries(
  java.util.Map.entry(
    1,
    "a"),
  java.util.Map.entry(
    2,
    "b")),

            hydra.lib.maps.Delete.apply(
  4,
  java.util.Map.ofEntries(
    java.util.Map.entry(
      1,
      "a"),
    java.util.Map.entry(
      2,
      "b"))));

    }

    @Test

    public <T0, T1, T4> void testRemoveRemoveFromEmpty() {

        assertEquals(

            (java.util.Map<T0, T1>) ((java.util.Map<T0, T1>) (java.util.Map.<T0, T1>ofEntries())),

            hydra.lib.maps.Delete.apply(
  1,
  (java.util.Map<Integer, T4>) ((java.util.Map<Integer, T4>) (java.util.Map.<Integer, T4>ofEntries()))));

    }

    // singleton

    @Test

    public void testSingletonSingleEntry() {

        assertEquals(

            java.util.Map.ofEntries(java.util.Map.entry(
  42,
  "hello")),

            hydra.lib.maps.Singleton.apply(
  42,
  "hello"));

    }

    // size

    @Test

    public void testSizeThreeEntries() {

        assertEquals(

            3,

            hydra.lib.maps.Size.apply(java.util.Map.ofEntries(
  java.util.Map.entry(
    1,
    "a"),
  java.util.Map.entry(
    2,
    "b"),
  java.util.Map.entry(
    3,
    "c"))));

    }

    @Test

    public void testSizeSingleEntry() {

        assertEquals(

            1,

            hydra.lib.maps.Size.apply(java.util.Map.ofEntries(java.util.Map.entry(
  42,
  "test"))));

    }

    @Test

    public <T2, T3> void testSizeEmptyMap() {

        assertEquals(

            0,

            hydra.lib.maps.Size.apply((java.util.Map<T2, T3>) ((java.util.Map<T2, T3>) (java.util.Map.<T2, T3>ofEntries()))));

    }

    // toList

    @Test

    public void testTolistConvertToPairs() {

        assertEquals(

            java.util.List.of(
  (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(1, "a"))),
  (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(2, "b")))),

            hydra.lib.maps.ToList.apply(java.util.Map.ofEntries(
  java.util.Map.entry(
    1,
    "a"),
  java.util.Map.entry(
    2,
    "b"))));

    }

    @Test

    public void testTolistUnsortedKeys() {

        assertEquals(

            java.util.List.of(
  (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(1, "a"))),
  (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(2, "b"))),
  (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(3, "c")))),

            hydra.lib.maps.ToList.apply(java.util.Map.ofEntries(
  java.util.Map.entry(
    1,
    "a"),
  java.util.Map.entry(
    2,
    "b"),
  java.util.Map.entry(
    3,
    "c"))));

    }

    @Test

    public <T0, T2, T3> void testTolistEmptyMap() {

        assertEquals(

            (java.util.List<T0>) (java.util.List.<T0>of()),

            hydra.lib.maps.ToList.apply((java.util.Map<T2, T3>) ((java.util.Map<T2, T3>) (java.util.Map.<T2, T3>ofEntries()))));

    }

    // union

    @Test

    public void testUnionUnionTwoMaps() {

        assertEquals(

            java.util.Map.ofEntries(
  java.util.Map.entry(
    1,
    "a"),
  java.util.Map.entry(
    2,
    "b"),
  java.util.Map.entry(
    3,
    "c")),

            hydra.lib.maps.Union.apply(
  java.util.Map.ofEntries(
    java.util.Map.entry(
      1,
      "a"),
    java.util.Map.entry(
      2,
      "b")),
  java.util.Map.ofEntries(
    java.util.Map.entry(
      2,
      "x"),
    java.util.Map.entry(
      3,
      "c"))));

    }

    @Test

    public void testUnionUnionWithEmpty() {

        assertEquals(

            java.util.Map.ofEntries(java.util.Map.entry(
  1,
  "a")),

            hydra.lib.maps.Union.apply(
  java.util.Map.ofEntries(java.util.Map.entry(
    1,
    "a")),
  (java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (java.util.Map.<Integer, String>ofEntries()))));

    }

    @Test

    public void testUnionEmptyWithMap() {

        assertEquals(

            java.util.Map.ofEntries(java.util.Map.entry(
  1,
  "a")),

            hydra.lib.maps.Union.apply(
  (java.util.Map<Integer, String>) ((java.util.Map<Integer, String>) (java.util.Map.<Integer, String>ofEntries())),
  java.util.Map.ofEntries(java.util.Map.entry(
    1,
    "a"))));

    }
}

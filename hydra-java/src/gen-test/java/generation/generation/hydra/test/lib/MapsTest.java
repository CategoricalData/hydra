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

    public void testBimapEmptyMap() {

        assertEquals(

            (java.util.Map<java.lang.Object, java.lang.Object>) ((java.util.Map<java.lang.Object, java.lang.Object>) (java.util.Map.<java.lang.Object, java.lang.Object>ofEntries())),

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

    public void testElemsEmptyMap() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.maps.Elems.apply((java.util.Map<java.lang.Object, java.lang.Object>) ((java.util.Map<java.lang.Object, java.lang.Object>) (java.util.Map.<java.lang.Object, java.lang.Object>ofEntries()))));

    }

    // empty

    @Test

    public void testEmptyEmptyMap() {

        assertEquals(

            (java.util.Map<java.lang.Object, java.lang.Object>) ((java.util.Map<java.lang.Object, java.lang.Object>) (java.util.Map.<java.lang.Object, java.lang.Object>ofEntries())),

            (java.util.Map<java.lang.Object, java.lang.Object>) ((java.util.Map<java.lang.Object, java.lang.Object>) (hydra.lib.maps.Empty.<java.lang.Object, java.lang.Object>apply())));

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

    public void testFilterFilterAll() {

        assertEquals(

            (java.util.Map<java.lang.Object, java.lang.Object>) ((java.util.Map<java.lang.Object, java.lang.Object>) (java.util.Map.<java.lang.Object, java.lang.Object>ofEntries())),

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

    public void testFilterEmptyMap() {

        assertEquals(

            (java.util.Map<java.lang.Object, java.lang.Object>) ((java.util.Map<java.lang.Object, java.lang.Object>) (java.util.Map.<java.lang.Object, java.lang.Object>ofEntries())),

            hydra.lib.maps.Filter.apply(
  (java.util.function.Function<String, Boolean>) (v -> hydra.lib.equality.Equal.apply(
    hydra.lib.strings.CharAt.apply(
      0,
      v),
    97)),
  (java.util.Map<java.lang.Object, String>) ((java.util.Map<java.lang.Object, String>) (java.util.Map.<java.lang.Object, String>ofEntries()))));

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

    public void testFilterwithkeyFilterAll() {

        assertEquals(

            (java.util.Map<java.lang.Object, java.lang.Object>) ((java.util.Map<java.lang.Object, java.lang.Object>) (java.util.Map.<java.lang.Object, java.lang.Object>ofEntries())),

            hydra.lib.maps.FilterWithKey.apply(
  (java.util.function.Function<Integer, java.util.function.Function<String, Boolean>>) (k -> (java.util.function.Function<String, Boolean>) (v -> hydra.lib.equality.Gt.apply(
    k,
    1))),
  java.util.Map.ofEntries(java.util.Map.entry(
    1,
    "a"))));

    }

    @Test

    public void testFilterwithkeyEmptyMap() {

        assertEquals(

            (java.util.Map<java.lang.Object, java.lang.Object>) ((java.util.Map<java.lang.Object, java.lang.Object>) (java.util.Map.<java.lang.Object, java.lang.Object>ofEntries())),

            hydra.lib.maps.FilterWithKey.apply(
  (java.util.function.Function<Integer, java.util.function.Function<java.lang.Object, Boolean>>) (k -> (java.util.function.Function<java.lang.Object, Boolean>) (v -> hydra.lib.equality.Gt.apply(
    k,
    1))),
  (java.util.Map<Integer, java.lang.Object>) ((java.util.Map<Integer, java.lang.Object>) (java.util.Map.<Integer, java.lang.Object>ofEntries()))));

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
  (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(1, "a"))),
  (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(2, "b"))))));

    }

    @Test

    public void testFromlistDuplicateKeys() {

        assertEquals(

            java.util.Map.ofEntries(java.util.Map.entry(
  1,
  "b")),

            hydra.lib.maps.FromList.apply(java.util.List.of(
  (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(1, "a"))),
  (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(1, "b"))))));

    }

    @Test

    public void testFromlistEmptyList() {

        assertEquals(

            (java.util.Map<java.lang.Object, java.lang.Object>) ((java.util.Map<java.lang.Object, java.lang.Object>) (java.util.Map.<java.lang.Object, java.lang.Object>ofEntries())),

            hydra.lib.maps.FromList.apply((java.util.List<hydra.util.Tuple.Tuple2<java.lang.Object, java.lang.Object>>) (java.util.List.<hydra.util.Tuple.Tuple2<java.lang.Object, java.lang.Object>>of())));

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

    public void testKeysEmptyMap() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.maps.Keys.apply((java.util.Map<java.lang.Object, java.lang.Object>) ((java.util.Map<java.lang.Object, java.lang.Object>) (java.util.Map.<java.lang.Object, java.lang.Object>ofEntries()))));

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

    public void testLookupKeyNotFound() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

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

    public void testLookupLookupInEmpty() {

        assertEquals(

            (hydra.util.Maybe<java.lang.Object>) (hydra.util.Maybe.<java.lang.Object>nothing()),

            hydra.lib.maps.Lookup.apply(
  1,
  (java.util.Map<Integer, java.lang.Object>) ((java.util.Map<Integer, java.lang.Object>) (java.util.Map.<Integer, java.lang.Object>ofEntries()))));

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

    public void testMapMapEmpty() {

        assertEquals(

            (java.util.Map<java.lang.Object, java.lang.Object>) ((java.util.Map<java.lang.Object, java.lang.Object>) (java.util.Map.<java.lang.Object, java.lang.Object>ofEntries())),

            hydra.lib.maps.Map.apply(
  (java.util.function.Function<String, String>) (s -> hydra.lib.strings.ToUpper.apply(s)),
  (java.util.Map<java.lang.Object, String>) ((java.util.Map<java.lang.Object, String>) (java.util.Map.<java.lang.Object, String>ofEntries()))));

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

    public void testMapkeysEmptyMap() {

        assertEquals(

            (java.util.Map<java.lang.Object, java.lang.Object>) ((java.util.Map<java.lang.Object, java.lang.Object>) (java.util.Map.<java.lang.Object, java.lang.Object>ofEntries())),

            hydra.lib.maps.MapKeys.apply(
  (java.util.function.Function<Integer, Integer>) (k -> hydra.lib.math.Mul.apply(
    k,
    2)),
  (java.util.Map<Integer, java.lang.Object>) ((java.util.Map<Integer, java.lang.Object>) (java.util.Map.<Integer, java.lang.Object>ofEntries()))));

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

    public void testMemberEmptyMap() {

        assertEquals(

            false,

            hydra.lib.maps.Member.apply(
  1,
  (java.util.Map<Integer, java.lang.Object>) ((java.util.Map<Integer, java.lang.Object>) (java.util.Map.<Integer, java.lang.Object>ofEntries()))));

    }

    // null

    @Test

    public void testNullEmptyMap() {

        assertEquals(

            true,

            hydra.lib.maps.Null.apply((java.util.Map<java.lang.Object, java.lang.Object>) ((java.util.Map<java.lang.Object, java.lang.Object>) (java.util.Map.<java.lang.Object, java.lang.Object>ofEntries()))));

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

    public void testRemoveRemoveFromEmpty() {

        assertEquals(

            (java.util.Map<java.lang.Object, java.lang.Object>) ((java.util.Map<java.lang.Object, java.lang.Object>) (java.util.Map.<java.lang.Object, java.lang.Object>ofEntries())),

            hydra.lib.maps.Delete.apply(
  1,
  (java.util.Map<Integer, java.lang.Object>) ((java.util.Map<Integer, java.lang.Object>) (java.util.Map.<Integer, java.lang.Object>ofEntries()))));

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

    public void testSizeEmptyMap() {

        assertEquals(

            0,

            hydra.lib.maps.Size.apply((java.util.Map<java.lang.Object, java.lang.Object>) ((java.util.Map<java.lang.Object, java.lang.Object>) (java.util.Map.<java.lang.Object, java.lang.Object>ofEntries()))));

    }

    // toList

    @Test

    public void testTolistConvertToPairs() {

        assertEquals(

            java.util.List.of(
  (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(1, "a"))),
  (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(2, "b")))),

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
  (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(1, "a"))),
  (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(2, "b"))),
  (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(3, "c")))),

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

    public void testTolistEmptyMap() {

        assertEquals(

            (java.util.List<java.lang.Object>) (java.util.List.<java.lang.Object>of()),

            hydra.lib.maps.ToList.apply((java.util.Map<java.lang.Object, java.lang.Object>) ((java.util.Map<java.lang.Object, java.lang.Object>) (java.util.Map.<java.lang.Object, java.lang.Object>ofEntries()))));

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

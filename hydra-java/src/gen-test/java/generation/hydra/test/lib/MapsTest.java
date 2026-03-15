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

            hydra.util.PersistentMap.ofEntries(
  hydra.util.PersistentMap.entry(
    1,
    "a"),
  hydra.util.PersistentMap.entry(
    2,
    "b"),
  hydra.util.PersistentMap.entry(
    3,
    "new")),

            hydra.lib.maps.Alter.apply(
  (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Maybe<String>>) (opt -> hydra.util.Maybe.just("new")),
  3,
  hydra.util.PersistentMap.ofEntries(
    hydra.util.PersistentMap.entry(
      1,
      "a"),
    hydra.util.PersistentMap.entry(
      2,
      "b"))));

    }

    @Test

    public void testAlterUpdateExistingKey() {

        assertEquals(

            hydra.util.PersistentMap.ofEntries(
  hydra.util.PersistentMap.entry(
    1,
    "a"),
  hydra.util.PersistentMap.entry(
    2,
    "updated")),

            hydra.lib.maps.Alter.apply(
  (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Maybe<String>>) (opt -> hydra.util.Maybe.just("updated")),
  2,
  hydra.util.PersistentMap.ofEntries(
    hydra.util.PersistentMap.entry(
      1,
      "a"),
    hydra.util.PersistentMap.entry(
      2,
      "b"))));

    }

    @Test

    public void testAlterDeleteKey() {

        assertEquals(

            hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
  1,
  "a")),

            hydra.lib.maps.Alter.apply(
  (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Maybe<String>>) (opt -> (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())),
  2,
  hydra.util.PersistentMap.ofEntries(
    hydra.util.PersistentMap.entry(
      1,
      "a"),
    hydra.util.PersistentMap.entry(
      2,
      "b"))));

    }

    // bimap

    @Test

    public void testBimapTransformBoth() {

        assertEquals(

            hydra.util.PersistentMap.ofEntries(
  hydra.util.PersistentMap.entry(
    2,
    "A"),
  hydra.util.PersistentMap.entry(
    4,
    "B")),

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
      "b"))));

    }

    @Test

    public <T0, T1> void testBimapEmptyMap() {

        assertEquals(

            (hydra.util.PersistentMap<T0, T1>) ((hydra.util.PersistentMap<T0, T1>) (hydra.util.PersistentMap.<T0, T1>empty())),

            hydra.lib.maps.Bimap.apply(
  (java.util.function.Function<Integer, Integer>) (k -> hydra.lib.math.Mul.apply(
    k,
    2)),
  (java.util.function.Function<String, String>) (v -> hydra.lib.strings.ToUpper.apply(v)),
  (hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.util.PersistentMap.<Integer, String>empty()))));

    }

    // elems

    @Test

    public void testElemsGetAllElements() {

        assertEquals(

            hydra.util.ConsList.of(
  "a",
  "b"),

            hydra.lib.maps.Elems.apply(hydra.util.PersistentMap.ofEntries(
  hydra.util.PersistentMap.entry(
    1,
    "a"),
  hydra.util.PersistentMap.entry(
    2,
    "b"))));

    }

    @Test

    public void testElemsUnsortedKeys() {

        assertEquals(

            hydra.util.ConsList.of(
  "a",
  "b",
  "c"),

            hydra.lib.maps.Elems.apply(hydra.util.PersistentMap.ofEntries(
  hydra.util.PersistentMap.entry(
    1,
    "a"),
  hydra.util.PersistentMap.entry(
    2,
    "b"),
  hydra.util.PersistentMap.entry(
    3,
    "c"))));

    }

    @Test

    public <T0, T2, T3> void testElemsEmptyMap() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>of()),

            hydra.lib.maps.Elems.apply((hydra.util.PersistentMap<T2, T3>) ((hydra.util.PersistentMap<T2, T3>) (hydra.util.PersistentMap.<T2, T3>empty()))));

    }

    // empty

    @Test

    public <T0, T1> void testEmptyEmptyMap() {

        assertEquals(

            (hydra.util.PersistentMap<T0, T1>) ((hydra.util.PersistentMap<T0, T1>) (hydra.util.PersistentMap.<T0, T1>empty())),

            (hydra.util.PersistentMap<T0, T1>) ((hydra.util.PersistentMap<T0, T1>) (hydra.lib.maps.Empty.<T0, T1>apply())));

    }

    // filter

    @Test

    public void testFilterFilterValuesStartingWithA() {

        assertEquals(

            hydra.util.PersistentMap.ofEntries(
  hydra.util.PersistentMap.entry(
    1,
    "a"),
  hydra.util.PersistentMap.entry(
    3,
    "ab")),

            hydra.lib.maps.Filter.apply(
  (java.util.function.Function<String, Boolean>) (v -> hydra.lib.equality.Equal.apply(
    hydra.lib.strings.CharAt.apply(
      0,
      v),
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
      "ab"))));

    }

    @Test

    public <T0, T1> void testFilterFilterAll() {

        assertEquals(

            (hydra.util.PersistentMap<T0, T1>) ((hydra.util.PersistentMap<T0, T1>) (hydra.util.PersistentMap.<T0, T1>empty())),

            hydra.lib.maps.Filter.apply(
  (java.util.function.Function<String, Boolean>) (v -> hydra.lib.equality.Equal.apply(
    hydra.lib.strings.CharAt.apply(
      0,
      v),
    97)),
  hydra.util.PersistentMap.ofEntries(
    hydra.util.PersistentMap.entry(
      1,
      "b"),
    hydra.util.PersistentMap.entry(
      2,
      "c"))));

    }

    @Test

    public <T0, T1, T9> void testFilterEmptyMap() {

        assertEquals(

            (hydra.util.PersistentMap<T0, T1>) ((hydra.util.PersistentMap<T0, T1>) (hydra.util.PersistentMap.<T0, T1>empty())),

            hydra.lib.maps.Filter.apply(
  (java.util.function.Function<String, Boolean>) (v -> hydra.lib.equality.Equal.apply(
    hydra.lib.strings.CharAt.apply(
      0,
      v),
    97)),
  (hydra.util.PersistentMap<T9, String>) ((hydra.util.PersistentMap<T9, String>) (hydra.util.PersistentMap.<T9, String>empty()))));

    }

    // filterWithKey

    @Test

    public void testFilterwithkeyFilterByKey1() {

        assertEquals(

            hydra.util.PersistentMap.ofEntries(
  hydra.util.PersistentMap.entry(
    2,
    "b"),
  hydra.util.PersistentMap.entry(
    3,
    "c")),

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
      "c"))));

    }

    @Test

    public <T0, T1> void testFilterwithkeyFilterAll() {

        assertEquals(

            (hydra.util.PersistentMap<T0, T1>) ((hydra.util.PersistentMap<T0, T1>) (hydra.util.PersistentMap.<T0, T1>empty())),

            hydra.lib.maps.FilterWithKey.apply(
  (java.util.function.Function<Integer, java.util.function.Function<String, Boolean>>) (k -> (java.util.function.Function<String, Boolean>) (v -> hydra.lib.equality.Gt.apply(
    k,
    1))),
  hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
    1,
    "a"))));

    }

    @Test

    public <T0, T1, T9> void testFilterwithkeyEmptyMap() {

        assertEquals(

            (hydra.util.PersistentMap<T0, T1>) ((hydra.util.PersistentMap<T0, T1>) (hydra.util.PersistentMap.<T0, T1>empty())),

            hydra.lib.maps.FilterWithKey.apply(
  (java.util.function.Function<Integer, java.util.function.Function<T9, Boolean>>) (k -> (java.util.function.Function<T9, Boolean>) (v -> hydra.lib.equality.Gt.apply(
    k,
    1))),
  (hydra.util.PersistentMap<Integer, T9>) ((hydra.util.PersistentMap<Integer, T9>) (hydra.util.PersistentMap.<Integer, T9>empty()))));

    }

    // findWithDefault

    @Test

    public void testFindwithdefaultFindExisting() {

        assertEquals(

            "b",

            hydra.lib.maps.FindWithDefault.applyLazy(
  () -> "default",
  2,
  hydra.util.PersistentMap.ofEntries(
    hydra.util.PersistentMap.entry(
      1,
      "a"),
    hydra.util.PersistentMap.entry(
      2,
      "b"))));

    }

    @Test

    public void testFindwithdefaultUseDefault() {

        assertEquals(

            "default",

            hydra.lib.maps.FindWithDefault.applyLazy(
  () -> "default",
  3,
  hydra.util.PersistentMap.ofEntries(
    hydra.util.PersistentMap.entry(
      1,
      "a"),
    hydra.util.PersistentMap.entry(
      2,
      "b"))));

    }

    // fromList

    @Test

    public void testFromlistCreateFromPairs() {

        assertEquals(

            hydra.util.PersistentMap.ofEntries(
  hydra.util.PersistentMap.entry(
    1,
    "a"),
  hydra.util.PersistentMap.entry(
    2,
    "b")),

            hydra.lib.maps.FromList.apply(hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(1, "a"))),
  (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(2, "b"))))));

    }

    @Test

    public void testFromlistDuplicateKeys() {

        assertEquals(

            hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
  1,
  "b")),

            hydra.lib.maps.FromList.apply(hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(1, "a"))),
  (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(1, "b"))))));

    }

    @Test

    public <T0, T1> void testFromlistEmptyList() {

        assertEquals(

            (hydra.util.PersistentMap<T0, T1>) ((hydra.util.PersistentMap<T0, T1>) (hydra.util.PersistentMap.<T0, T1>empty())),

            hydra.lib.maps.FromList.apply((hydra.util.ConsList<hydra.util.Pair<T0, T1>>) (hydra.util.ConsList.<hydra.util.Pair<T0, T1>>of())));

    }

    // insert

    @Test

    public void testInsertInsertNewKey() {

        assertEquals(

            hydra.util.PersistentMap.ofEntries(
  hydra.util.PersistentMap.entry(
    1,
    "a"),
  hydra.util.PersistentMap.entry(
    2,
    "b"),
  hydra.util.PersistentMap.entry(
    3,
    "c")),

            hydra.lib.maps.Insert.apply(
  3,
  "c",
  hydra.util.PersistentMap.ofEntries(
    hydra.util.PersistentMap.entry(
      1,
      "a"),
    hydra.util.PersistentMap.entry(
      2,
      "b"))));

    }

    @Test

    public void testInsertUpdateExisting() {

        assertEquals(

            hydra.util.PersistentMap.ofEntries(
  hydra.util.PersistentMap.entry(
    1,
    "a"),
  hydra.util.PersistentMap.entry(
    2,
    "updated")),

            hydra.lib.maps.Insert.apply(
  2,
  "updated",
  hydra.util.PersistentMap.ofEntries(
    hydra.util.PersistentMap.entry(
      1,
      "a"),
    hydra.util.PersistentMap.entry(
      2,
      "b"))));

    }

    @Test

    public void testInsertInsertIntoEmpty() {

        assertEquals(

            hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
  1,
  "x")),

            hydra.lib.maps.Insert.apply(
  1,
  "x",
  (hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.util.PersistentMap.<Integer, String>empty()))));

    }

    // keys

    @Test

    public void testKeysGetAllKeys() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2,
  3),

            hydra.lib.maps.Keys.apply(hydra.util.PersistentMap.ofEntries(
  hydra.util.PersistentMap.entry(
    1,
    "a"),
  hydra.util.PersistentMap.entry(
    2,
    "b"),
  hydra.util.PersistentMap.entry(
    3,
    "c"))));

    }

    @Test

    public void testKeysUnsortedKeys() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2,
  3),

            hydra.lib.maps.Keys.apply(hydra.util.PersistentMap.ofEntries(
  hydra.util.PersistentMap.entry(
    1,
    "a"),
  hydra.util.PersistentMap.entry(
    2,
    "b"),
  hydra.util.PersistentMap.entry(
    3,
    "c"))));

    }

    @Test

    public <T0, T2, T3> void testKeysEmptyMap() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>of()),

            hydra.lib.maps.Keys.apply((hydra.util.PersistentMap<T2, T3>) ((hydra.util.PersistentMap<T2, T3>) (hydra.util.PersistentMap.<T2, T3>empty()))));

    }

    // lookup

    @Test

    public void testLookupFindExistingKey() {

        assertEquals(

            hydra.util.Maybe.just("b"),

            hydra.lib.maps.Lookup.apply(
  2,
  hydra.util.PersistentMap.ofEntries(
    hydra.util.PersistentMap.entry(
      1,
      "a"),
    hydra.util.PersistentMap.entry(
      2,
      "b"))));

    }

    @Test

    public <T0> void testLookupKeyNotFound() {

        assertEquals(

            (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()),

            hydra.lib.maps.Lookup.apply(
  3,
  hydra.util.PersistentMap.ofEntries(
    hydra.util.PersistentMap.entry(
      1,
      "a"),
    hydra.util.PersistentMap.entry(
      2,
      "b"))));

    }

    @Test

    public <T0, T4> void testLookupLookupInEmpty() {

        assertEquals(

            (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()),

            hydra.lib.maps.Lookup.apply(
  1,
  (hydra.util.PersistentMap<Integer, T4>) ((hydra.util.PersistentMap<Integer, T4>) (hydra.util.PersistentMap.<Integer, T4>empty()))));

    }

    // map

    @Test

    public void testMapMapOverValues() {

        assertEquals(

            hydra.util.PersistentMap.ofEntries(
  hydra.util.PersistentMap.entry(
    1,
    "A"),
  hydra.util.PersistentMap.entry(
    2,
    "B")),

            hydra.lib.maps.Map.apply(
  (java.util.function.Function<String, String>) (s -> hydra.lib.strings.ToUpper.apply(s)),
  hydra.util.PersistentMap.ofEntries(
    hydra.util.PersistentMap.entry(
      1,
      "a"),
    hydra.util.PersistentMap.entry(
      2,
      "b"))));

    }

    @Test

    public <T0, T1, T6> void testMapMapEmpty() {

        assertEquals(

            (hydra.util.PersistentMap<T0, T1>) ((hydra.util.PersistentMap<T0, T1>) (hydra.util.PersistentMap.<T0, T1>empty())),

            hydra.lib.maps.Map.apply(
  (java.util.function.Function<String, String>) (s -> hydra.lib.strings.ToUpper.apply(s)),
  (hydra.util.PersistentMap<T6, String>) ((hydra.util.PersistentMap<T6, String>) (hydra.util.PersistentMap.<T6, String>empty()))));

    }

    // mapKeys

    @Test

    public void testMapkeysDoubleKeys() {

        assertEquals(

            hydra.util.PersistentMap.ofEntries(
  hydra.util.PersistentMap.entry(
    2,
    "a"),
  hydra.util.PersistentMap.entry(
    4,
    "b")),

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
      "b"))));

    }

    @Test

    public <T0, T1, T8> void testMapkeysEmptyMap() {

        assertEquals(

            (hydra.util.PersistentMap<T0, T1>) ((hydra.util.PersistentMap<T0, T1>) (hydra.util.PersistentMap.<T0, T1>empty())),

            hydra.lib.maps.MapKeys.apply(
  (java.util.function.Function<Integer, Integer>) (k -> hydra.lib.math.Mul.apply(
    k,
    2)),
  (hydra.util.PersistentMap<Integer, T8>) ((hydra.util.PersistentMap<Integer, T8>) (hydra.util.PersistentMap.<Integer, T8>empty()))));

    }

    // member

    @Test

    public void testMemberKeyExists() {

        assertEquals(

            true,

            hydra.lib.maps.Member.apply(
  2,
  hydra.util.PersistentMap.ofEntries(
    hydra.util.PersistentMap.entry(
      1,
      "a"),
    hydra.util.PersistentMap.entry(
      2,
      "b"))));

    }

    @Test

    public void testMemberKeyMissing() {

        assertEquals(

            false,

            hydra.lib.maps.Member.apply(
  3,
  hydra.util.PersistentMap.ofEntries(
    hydra.util.PersistentMap.entry(
      1,
      "a"),
    hydra.util.PersistentMap.entry(
      2,
      "b"))));

    }

    @Test

    public <T4> void testMemberEmptyMap() {

        assertEquals(

            false,

            hydra.lib.maps.Member.apply(
  1,
  (hydra.util.PersistentMap<Integer, T4>) ((hydra.util.PersistentMap<Integer, T4>) (hydra.util.PersistentMap.<Integer, T4>empty()))));

    }

    // null

    @Test

    public <T2, T3> void testNullEmptyMap() {

        assertEquals(

            true,

            hydra.lib.maps.Null.apply((hydra.util.PersistentMap<T2, T3>) ((hydra.util.PersistentMap<T2, T3>) (hydra.util.PersistentMap.<T2, T3>empty()))));

    }

    @Test

    public void testNullNonNegemptyMap() {

        assertEquals(

            false,

            hydra.lib.maps.Null.apply(hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
  1,
  "a"))));

    }

    // remove

    @Test

    public void testRemoveRemoveExisting() {

        assertEquals(

            hydra.util.PersistentMap.ofEntries(
  hydra.util.PersistentMap.entry(
    1,
    "a"),
  hydra.util.PersistentMap.entry(
    3,
    "c")),

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
      "c"))));

    }

    @Test

    public void testRemoveRemoveNonNegexisting() {

        assertEquals(

            hydra.util.PersistentMap.ofEntries(
  hydra.util.PersistentMap.entry(
    1,
    "a"),
  hydra.util.PersistentMap.entry(
    2,
    "b")),

            hydra.lib.maps.Delete.apply(
  4,
  hydra.util.PersistentMap.ofEntries(
    hydra.util.PersistentMap.entry(
      1,
      "a"),
    hydra.util.PersistentMap.entry(
      2,
      "b"))));

    }

    @Test

    public <T0, T1, T4> void testRemoveRemoveFromEmpty() {

        assertEquals(

            (hydra.util.PersistentMap<T0, T1>) ((hydra.util.PersistentMap<T0, T1>) (hydra.util.PersistentMap.<T0, T1>empty())),

            hydra.lib.maps.Delete.apply(
  1,
  (hydra.util.PersistentMap<Integer, T4>) ((hydra.util.PersistentMap<Integer, T4>) (hydra.util.PersistentMap.<Integer, T4>empty()))));

    }

    // singleton

    @Test

    public void testSingletonSingleEntry() {

        assertEquals(

            hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
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

            hydra.lib.maps.Size.apply(hydra.util.PersistentMap.ofEntries(
  hydra.util.PersistentMap.entry(
    1,
    "a"),
  hydra.util.PersistentMap.entry(
    2,
    "b"),
  hydra.util.PersistentMap.entry(
    3,
    "c"))));

    }

    @Test

    public void testSizeSingleEntry() {

        assertEquals(

            1,

            hydra.lib.maps.Size.apply(hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
  42,
  "test"))));

    }

    @Test

    public <T2, T3> void testSizeEmptyMap() {

        assertEquals(

            0,

            hydra.lib.maps.Size.apply((hydra.util.PersistentMap<T2, T3>) ((hydra.util.PersistentMap<T2, T3>) (hydra.util.PersistentMap.<T2, T3>empty()))));

    }

    // toList

    @Test

    public void testTolistConvertToPairs() {

        assertEquals(

            hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(1, "a"))),
  (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(2, "b")))),

            hydra.lib.maps.ToList.apply(hydra.util.PersistentMap.ofEntries(
  hydra.util.PersistentMap.entry(
    1,
    "a"),
  hydra.util.PersistentMap.entry(
    2,
    "b"))));

    }

    @Test

    public void testTolistUnsortedKeys() {

        assertEquals(

            hydra.util.ConsList.of(
  (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(1, "a"))),
  (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(2, "b"))),
  (hydra.util.Pair<Integer, String>) ((hydra.util.Pair<Integer, String>) (new hydra.util.Pair<Integer, String>(3, "c")))),

            hydra.lib.maps.ToList.apply(hydra.util.PersistentMap.ofEntries(
  hydra.util.PersistentMap.entry(
    1,
    "a"),
  hydra.util.PersistentMap.entry(
    2,
    "b"),
  hydra.util.PersistentMap.entry(
    3,
    "c"))));

    }

    @Test

    public <T0, T2, T3> void testTolistEmptyMap() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>of()),

            hydra.lib.maps.ToList.apply((hydra.util.PersistentMap<T2, T3>) ((hydra.util.PersistentMap<T2, T3>) (hydra.util.PersistentMap.<T2, T3>empty()))));

    }

    // union

    @Test

    public void testUnionUnionTwoMaps() {

        assertEquals(

            hydra.util.PersistentMap.ofEntries(
  hydra.util.PersistentMap.entry(
    1,
    "a"),
  hydra.util.PersistentMap.entry(
    2,
    "b"),
  hydra.util.PersistentMap.entry(
    3,
    "c")),

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
      "c"))));

    }

    @Test

    public void testUnionUnionWithEmpty() {

        assertEquals(

            hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
  1,
  "a")),

            hydra.lib.maps.Union.apply(
  hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
    1,
    "a")),
  (hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.util.PersistentMap.<Integer, String>empty()))));

    }

    @Test

    public void testUnionEmptyWithMap() {

        assertEquals(

            hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
  1,
  "a")),

            hydra.lib.maps.Union.apply(
  (hydra.util.PersistentMap<Integer, String>) ((hydra.util.PersistentMap<Integer, String>) (hydra.util.PersistentMap.<Integer, String>empty())),
  hydra.util.PersistentMap.ofEntries(hydra.util.PersistentMap.entry(
    1,
    "a"))));

    }
}

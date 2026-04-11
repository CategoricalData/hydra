package hydra.util;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

public class PersistentMapTest {

    @Test
    public void testEmpty() {
        PersistentMap<String, Integer> m = PersistentMap.empty();
        assertTrue(m.isEmpty());
        assertEquals(0, m.size());
        assertTrue(m.lookup("a").isNothing());
        assertNull(m.get("a"));
        assertFalse(m.containsKey("a"));
    }

    @Test
    public void testSingleton() {
        PersistentMap<String, Integer> m = PersistentMap.singleton("a", 1);
        assertFalse(m.isEmpty());
        assertEquals(1, m.size());
        assertEquals(1, (int) m.get("a"));
        assertTrue(m.containsKey("a"));
        assertFalse(m.containsKey("b"));
    }

    @Test
    public void testInsertAndLookup() {
        PersistentMap<String, Integer> m = PersistentMap.<String, Integer>empty()
                .insert("b", 2)
                .insert("a", 1)
                .insert("c", 3);
        assertEquals(3, m.size());
        assertEquals(1, (int) m.get("a"));
        assertEquals(2, (int) m.get("b"));
        assertEquals(3, (int) m.get("c"));
    }

    @Test
    public void testInsertOverwrite() {
        PersistentMap<String, Integer> m1 = PersistentMap.<String, Integer>empty()
                .insert("a", 1);
        PersistentMap<String, Integer> m2 = m1.insert("a", 2);
        assertEquals(1, (int) m1.get("a"));
        assertEquals(2, (int) m2.get("a"));
        assertEquals(1, m1.size());
        assertEquals(1, m2.size());
    }

    @Test
    public void testInsertSameValueSharesStructure() {
        PersistentMap<String, Integer> m1 = PersistentMap.<String, Integer>empty()
                .insert("a", 1);
        PersistentMap<String, Integer> m2 = m1.insert("a", 1);
        assertSame(m1, m2);
    }

    @Test
    public void testStructuralSharing() {
        PersistentMap<Integer, String> m1 = PersistentMap.<Integer, String>empty()
                .insert(1, "one")
                .insert(2, "two")
                .insert(3, "three");
        PersistentMap<Integer, String> m2 = m1.insert(4, "four");
        // Both maps should be valid
        assertEquals(3, m1.size());
        assertEquals(4, m2.size());
        assertEquals("one", m1.get(1));
        assertEquals("one", m2.get(1));
        assertNull(m1.get(4));
        assertEquals("four", m2.get(4));
    }

    @Test
    public void testDelete() {
        PersistentMap<String, Integer> m1 = PersistentMap.<String, Integer>empty()
                .insert("a", 1)
                .insert("b", 2)
                .insert("c", 3);
        PersistentMap<String, Integer> m2 = m1.delete("b");
        assertEquals(3, m1.size());
        assertEquals(2, m2.size());
        assertEquals(2, (int) m1.get("b"));
        assertNull(m2.get("b"));
        assertEquals(1, (int) m2.get("a"));
        assertEquals(3, (int) m2.get("c"));
    }

    @Test
    public void testDeleteNonexistent() {
        PersistentMap<String, Integer> m1 = PersistentMap.<String, Integer>empty()
                .insert("a", 1);
        PersistentMap<String, Integer> m2 = m1.delete("b");
        assertSame(m1, m2);
    }

    @Test
    public void testUnionLeftBiased() {
        PersistentMap<String, Integer> m1 = PersistentMap.<String, Integer>empty()
                .insert("a", 1)
                .insert("b", 2);
        PersistentMap<String, Integer> m2 = PersistentMap.<String, Integer>empty()
                .insert("b", 20)
                .insert("c", 30);
        PersistentMap<String, Integer> u = m1.union(m2);
        assertEquals(3, u.size());
        assertEquals(1, (int) u.get("a"));
        assertEquals(2, (int) u.get("b")); // m1's value wins
        assertEquals(30, (int) u.get("c"));
    }

    @Test
    public void testUnionWithEmpty() {
        PersistentMap<String, Integer> m = PersistentMap.<String, Integer>empty()
                .insert("a", 1);
        assertSame(m, m.union(PersistentMap.empty()));
        assertSame(m, PersistentMap.<String, Integer>empty().union(m));
    }

    @Test
    public void testToListSorted() {
        PersistentMap<String, Integer> m = PersistentMap.<String, Integer>empty()
                .insert("c", 3)
                .insert("a", 1)
                .insert("b", 2);
        List<Pair<String, Integer>> list = m.toList();
        assertEquals(3, list.size());
        assertEquals("a", list.get(0).first);
        assertEquals("b", list.get(1).first);
        assertEquals("c", list.get(2).first);
    }

    @Test
    public void testKeys() {
        PersistentMap<String, Integer> m = PersistentMap.<String, Integer>empty()
                .insert("c", 3)
                .insert("a", 1)
                .insert("b", 2);
        List<String> keys = m.keys();
        assertEquals(List.of("a", "b", "c"), keys);
    }

    @Test
    public void testValues() {
        PersistentMap<String, Integer> m = PersistentMap.<String, Integer>empty()
                .insert("a", 1)
                .insert("b", 2)
                .insert("c", 3);
        List<Integer> values = m.values();
        assertEquals(List.of(1, 2, 3), values);
    }

    @Test
    public void testMapValues() {
        PersistentMap<String, Integer> m = PersistentMap.<String, Integer>empty()
                .insert("a", 1)
                .insert("b", 2);
        PersistentMap<String, String> mapped = m.mapValues(v -> "val" + v);
        assertEquals("val1", mapped.get("a"));
        assertEquals("val2", mapped.get("b"));
        assertEquals(2, mapped.size());
    }

    @Test
    public void testFilter() {
        PersistentMap<String, Integer> m = PersistentMap.<String, Integer>empty()
                .insert("a", 1)
                .insert("b", 2)
                .insert("c", 3);
        PersistentMap<String, Integer> filtered = m.filter(v -> v > 1);
        assertEquals(2, filtered.size());
        assertNull(filtered.get("a"));
        assertEquals(2, (int) filtered.get("b"));
        assertEquals(3, (int) filtered.get("c"));
    }

    @Test
    public void testFilterWithKey() {
        PersistentMap<String, Integer> m = PersistentMap.<String, Integer>empty()
                .insert("a", 1)
                .insert("b", 2)
                .insert("c", 3);
        PersistentMap<String, Integer> filtered = m.filterWithKey((k, v) -> k.compareTo("b") >= 0 && v > 1);
        assertEquals(2, filtered.size());
        assertEquals(2, (int) filtered.get("b"));
        assertEquals(3, (int) filtered.get("c"));
    }

    @Test
    public void testFindWithDefault() {
        PersistentMap<String, Integer> m = PersistentMap.singleton("a", 1);
        assertEquals(1, (int) m.findWithDefault(0, "a"));
        assertEquals(0, (int) m.findWithDefault(0, "b"));
    }

    @Test
    public void testFromPairList() {
        List<Pair<String, Integer>> pairs = List.of(
                new Pair<>("b", 2),
                new Pair<>("a", 1),
                new Pair<>("c", 3));
        PersistentMap<String, Integer> m = PersistentMap.fromPairList(pairs);
        assertEquals(3, m.size());
        assertEquals(1, (int) m.get("a"));
        assertEquals(2, (int) m.get("b"));
        assertEquals(3, (int) m.get("c"));
    }

    @Test
    public void testFromPairListDuplicates() {
        List<Pair<String, Integer>> pairs = List.of(
                new Pair<>("a", 1),
                new Pair<>("a", 2));
        PersistentMap<String, Integer> m = PersistentMap.fromPairList(pairs);
        assertEquals(1, m.size());
        assertEquals(2, (int) m.get("a")); // Last wins
    }

    @Test
    public void testFromMap() {
        Map<String, Integer> source = Map.of("a", 1, "b", 2, "c", 3);
        PersistentMap<String, Integer> m = PersistentMap.fromMap(source);
        assertEquals(3, m.size());
        assertEquals(1, (int) m.get("a"));
        assertEquals(2, (int) m.get("b"));
        assertEquals(3, (int) m.get("c"));
    }

    @Test
    public void testAlter() {
        PersistentMap<String, Integer> m = PersistentMap.<String, Integer>empty()
                .insert("a", 1);

        // Modify existing
        PersistentMap<String, Integer> m2 = m.alter(
                mv -> mv.isJust() ? Maybe.just(mv.fromJust() + 10) : Maybe.nothing(), "a");
        assertEquals(11, (int) m2.get("a"));

        // Insert new
        PersistentMap<String, Integer> m3 = m.alter(
                mv -> Maybe.just(99), "b");
        assertEquals(99, (int) m3.get("b"));

        // Delete existing
        PersistentMap<String, Integer> m4 = m.alter(
                mv -> Maybe.nothing(), "a");
        assertEquals(0, m4.size());
    }

    @Test
    public void testBimap() {
        PersistentMap<String, Integer> m = PersistentMap.<String, Integer>empty()
                .insert("a", 1)
                .insert("b", 2);
        PersistentMap<String, String> mapped = m.bimap(k -> k.toUpperCase(), v -> "v" + v);
        assertEquals(2, mapped.size());
        assertEquals("v1", mapped.get("A"));
        assertEquals("v2", mapped.get("B"));
    }

    @Test
    public void testMapKeys() {
        PersistentMap<String, Integer> m = PersistentMap.<String, Integer>empty()
                .insert("a", 1)
                .insert("b", 2);
        PersistentMap<String, Integer> mapped = m.mapKeys(k -> k.toUpperCase());
        assertEquals(2, mapped.size());
        assertEquals(1, (int) mapped.get("A"));
        assertEquals(2, (int) mapped.get("B"));
    }

    @Test
    public void testEquality() {
        PersistentMap<String, Integer> m1 = PersistentMap.<String, Integer>empty()
                .insert("a", 1)
                .insert("b", 2);
        PersistentMap<String, Integer> m2 = PersistentMap.<String, Integer>empty()
                .insert("b", 2)
                .insert("a", 1);
        assertEquals(m1, m2);
        assertEquals(m1.hashCode(), m2.hashCode());
    }

    @Test
    public void testLargeMap() {
        PersistentMap<Integer, Integer> m = PersistentMap.empty();
        for (int i = 0; i < 10000; i++) {
            m = m.insert(i, i * 10);
        }
        assertEquals(10000, m.size());
        for (int i = 0; i < 10000; i++) {
            assertEquals(i * 10, (int) m.get(i));
        }
        // Verify sorted order
        List<Integer> keys = m.keys();
        for (int i = 0; i < 10000; i++) {
            assertEquals(i, (int) keys.get(i));
        }
    }

    @Test
    public void testLargeMapDelete() {
        PersistentMap<Integer, Integer> m = PersistentMap.empty();
        for (int i = 0; i < 1000; i++) {
            m = m.insert(i, i);
        }
        // Delete even numbers
        PersistentMap<Integer, Integer> m2 = m;
        for (int i = 0; i < 1000; i += 2) {
            m2 = m2.delete(i);
        }
        assertEquals(500, m2.size());
        for (int i = 0; i < 1000; i++) {
            if (i % 2 == 0) {
                assertNull(m2.get(i));
            } else {
                assertEquals(i, (int) m2.get(i));
            }
        }
        // Original map unchanged
        assertEquals(1000, m.size());
    }

    @Test
    public void testIteratorOrder() {
        PersistentMap<Integer, String> m = PersistentMap.<Integer, String>empty()
                .insert(5, "e")
                .insert(3, "c")
                .insert(1, "a")
                .insert(4, "d")
                .insert(2, "b");
        int expected = 1;
        for (Map.Entry<Integer, String> entry : m) {
            assertEquals(expected, (int) entry.getKey());
            expected++;
        }
    }
}

package hydra.util;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class PersistentSetTest {

    @Test
    public void testEmpty() {
        PersistentSet<String> s = PersistentSet.empty();
        assertTrue(s.isEmpty());
        assertEquals(0, s.size());
        assertFalse(s.member("a"));
    }

    @Test
    public void testSingleton() {
        PersistentSet<String> s = PersistentSet.singleton("a");
        assertEquals(1, s.size());
        assertTrue(s.member("a"));
        assertFalse(s.member("b"));
    }

    @Test
    public void testInsert() {
        PersistentSet<String> s = PersistentSet.<String>empty()
                .insert("b")
                .insert("a")
                .insert("c");
        assertEquals(3, s.size());
        assertTrue(s.member("a"));
        assertTrue(s.member("b"));
        assertTrue(s.member("c"));
    }

    @Test
    public void testInsertDuplicate() {
        PersistentSet<String> s1 = PersistentSet.<String>empty()
                .insert("a");
        PersistentSet<String> s2 = s1.insert("a");
        assertSame(s1, s2);
    }

    @Test
    public void testDelete() {
        PersistentSet<String> s1 = PersistentSet.<String>empty()
                .insert("a")
                .insert("b")
                .insert("c");
        PersistentSet<String> s2 = s1.delete("b");
        assertEquals(3, s1.size());
        assertEquals(2, s2.size());
        assertTrue(s1.member("b"));
        assertFalse(s2.member("b"));
    }

    @Test
    public void testUnion() {
        PersistentSet<String> s1 = PersistentSet.<String>empty()
                .insert("a").insert("b");
        PersistentSet<String> s2 = PersistentSet.<String>empty()
                .insert("b").insert("c");
        PersistentSet<String> u = s1.union(s2);
        assertEquals(3, u.size());
        assertTrue(u.member("a"));
        assertTrue(u.member("b"));
        assertTrue(u.member("c"));
    }

    @Test
    public void testIntersection() {
        PersistentSet<String> s1 = PersistentSet.<String>empty()
                .insert("a").insert("b").insert("c");
        PersistentSet<String> s2 = PersistentSet.<String>empty()
                .insert("b").insert("c").insert("d");
        PersistentSet<String> i = s1.intersection(s2);
        assertEquals(2, i.size());
        assertFalse(i.member("a"));
        assertTrue(i.member("b"));
        assertTrue(i.member("c"));
        assertFalse(i.member("d"));
    }

    @Test
    public void testDifference() {
        PersistentSet<String> s1 = PersistentSet.<String>empty()
                .insert("a").insert("b").insert("c");
        PersistentSet<String> s2 = PersistentSet.<String>empty()
                .insert("b").insert("d");
        PersistentSet<String> d = s1.difference(s2);
        assertEquals(2, d.size());
        assertTrue(d.member("a"));
        assertFalse(d.member("b"));
        assertTrue(d.member("c"));
    }

    @Test
    public void testToListSorted() {
        PersistentSet<Integer> s = PersistentSet.<Integer>empty()
                .insert(3).insert(1).insert(2);
        assertEquals(List.of(1, 2, 3), s.toList());
    }

    @Test
    public void testFromList() {
        PersistentSet<String> s = PersistentSet.fromList(List.of("c", "a", "b", "a"));
        assertEquals(3, s.size());
        assertEquals(List.of("a", "b", "c"), s.toList());
    }

    @Test
    public void testMap() {
        PersistentSet<String> s = PersistentSet.<String>empty()
                .insert("a").insert("b");
        PersistentSet<String> mapped = s.map(x -> x.toUpperCase());
        assertEquals(2, mapped.size());
        assertTrue(mapped.member("A"));
        assertTrue(mapped.member("B"));
    }

    @Test
    public void testFilter() {
        PersistentSet<Integer> s = PersistentSet.<Integer>empty()
                .insert(1).insert(2).insert(3).insert(4);
        PersistentSet<Integer> evens = s.filter(x -> x % 2 == 0);
        assertEquals(2, evens.size());
        assertTrue(evens.member(2));
        assertTrue(evens.member(4));
    }

    @Test
    public void testUnions() {
        PersistentSet<Integer> s1 = PersistentSet.fromList(List.of(1, 2));
        PersistentSet<Integer> s2 = PersistentSet.fromList(List.of(2, 3));
        PersistentSet<Integer> s3 = PersistentSet.fromList(List.of(3, 4));
        PersistentSet<Integer> u = PersistentSet.unions(List.of(s1, s2, s3));
        assertEquals(4, u.size());
        assertEquals(List.of(1, 2, 3, 4), u.toList());
    }

    @Test
    public void testEquality() {
        PersistentSet<String> s1 = PersistentSet.<String>empty()
                .insert("a").insert("b");
        PersistentSet<String> s2 = PersistentSet.<String>empty()
                .insert("b").insert("a");
        assertEquals(s1, s2);
        assertEquals(s1.hashCode(), s2.hashCode());
    }

    @Test
    public void testLargeSet() {
        PersistentSet<Integer> s = PersistentSet.empty();
        for (int i = 0; i < 10000; i++) {
            s = s.insert(i);
        }
        assertEquals(10000, s.size());
        for (int i = 0; i < 10000; i++) {
            assertTrue(s.member(i));
        }
    }
}

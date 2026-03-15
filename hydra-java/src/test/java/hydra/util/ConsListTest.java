package hydra.util;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class ConsListTest {

    @Test
    public void testEmpty() {
        ConsList<String> l = ConsList.empty();
        assertTrue(l.isEmpty());
        assertEquals(0, l.size());
    }

    @Test
    public void testCons() {
        ConsList<String> l = ConsList.cons("a", ConsList.empty());
        assertFalse(l.isEmpty());
        assertEquals(1, l.size());
        assertEquals("a", l.head());
    }

    @Test
    public void testHeadTail() {
        ConsList<Integer> l = ConsList.of(1, 2, 3);
        assertEquals(1, l.head());
        assertEquals(2, l.tail().head());
        assertEquals(3, l.tail().tail().head());
        assertTrue(l.tail().tail().tail().isEmpty());
    }

    @Test
    public void testSafeHead() {
        assertTrue(ConsList.empty().safeHead().isNothing());
        assertTrue(ConsList.singleton("x").safeHead().isJust());
        assertEquals("x", ConsList.singleton("x").safeHead().fromJust());
    }

    @Test
    public void testSize() {
        assertEquals(0, ConsList.empty().size());
        assertEquals(1, ConsList.singleton(1).size());
        assertEquals(3, ConsList.of(1, 2, 3).size());
    }

    @Test
    public void testStructuralSharing() {
        ConsList<Integer> tail = ConsList.of(2, 3);
        ConsList<Integer> l1 = ConsList.cons(1, tail);
        ConsList<Integer> l2 = ConsList.cons(0, tail);
        assertSame(l1.tail(), l2.tail());
        assertSame(tail, l1.tail());
    }

    @Test
    public void testGet() {
        ConsList<String> l = ConsList.of("a", "b", "c");
        assertEquals("a", l.get(0));
        assertEquals("b", l.get(1));
        assertEquals("c", l.get(2));
    }

    @Test
    public void testGetOutOfBounds() {
        assertThrows(IndexOutOfBoundsException.class, () -> ConsList.of(1).get(1));
        assertThrows(IndexOutOfBoundsException.class, () -> ConsList.of(1).get(-1));
    }

    @Test
    public void testLast() {
        assertEquals(3, ConsList.of(1, 2, 3).last());
        assertEquals(1, ConsList.singleton(1).last());
    }

    @Test
    public void testInit() {
        ConsList<Integer> l = ConsList.of(1, 2, 3).init();
        assertEquals(2, l.size());
        assertEquals(1, l.head());
        assertEquals(2, l.tail().head());
    }

    @Test
    public void testTake() {
        ConsList<Integer> l = ConsList.of(1, 2, 3, 4, 5);
        ConsList<Integer> taken = l.take(3);
        assertEquals(3, taken.size());
        assertEquals(1, taken.head());
        assertEquals(3, taken.tail().tail().head());

        assertEquals(0, l.take(0).size());
        assertEquals(5, l.take(10).size());
    }

    @Test
    public void testDrop() {
        ConsList<Integer> l = ConsList.of(1, 2, 3, 4, 5);
        ConsList<Integer> dropped = l.drop(2);
        assertEquals(3, dropped.size());
        assertEquals(3, dropped.head());
    }

    @Test
    public void testDropSharesStructure() {
        ConsList<Integer> l = ConsList.of(1, 2, 3, 4, 5);
        ConsList<Integer> dropped = l.drop(2);
        // dropped should be the same object as l.tail().tail()
        assertSame(l.tail().tail(), dropped);
    }

    @Test
    public void testReverse() {
        ConsList<Integer> l = ConsList.of(1, 2, 3);
        ConsList<Integer> r = l.reverse();
        assertEquals(3, r.head());
        assertEquals(2, r.tail().head());
        assertEquals(1, r.tail().tail().head());
    }

    @Test
    public void testMap() {
        ConsList<Integer> l = ConsList.of(1, 2, 3);
        ConsList<String> mapped = l.map(i -> "v" + i);
        assertEquals(3, mapped.size());
        assertEquals("v1", mapped.head());
        assertEquals("v2", mapped.tail().head());
        assertEquals("v3", mapped.tail().tail().head());
    }

    @Test
    public void testFilter() {
        ConsList<Integer> l = ConsList.of(1, 2, 3, 4, 5);
        ConsList<Integer> evens = l.filter(i -> i % 2 == 0);
        assertEquals(2, evens.size());
        assertEquals(2, evens.head());
        assertEquals(4, evens.tail().head());
    }

    @Test
    public void testFoldl() {
        ConsList<Integer> l = ConsList.of(1, 2, 3);
        int sum = l.foldl((acc, x) -> acc + x, 0);
        assertEquals(6, sum);
    }

    @Test
    public void testFoldr() {
        ConsList<String> l = ConsList.of("a", "b", "c");
        String result = l.foldr((x, acc) -> x + acc, "");
        assertEquals("abc", result);
    }

    @Test
    public void testConcat() {
        ConsList<Integer> l1 = ConsList.of(1, 2);
        ConsList<Integer> l2 = ConsList.of(3, 4);
        ConsList<Integer> combined = l1.concat(l2);
        assertEquals(4, combined.size());
        assertEquals(List.of(1, 2, 3, 4), combined.toArrayList());
    }

    @Test
    public void testConcatSharesTail() {
        ConsList<Integer> l1 = ConsList.of(1, 2);
        ConsList<Integer> l2 = ConsList.of(3, 4);
        ConsList<Integer> combined = l1.concat(l2);
        // The tail portion (3,4) should be shared with l2
        assertSame(l2, combined.tail().tail());
    }

    @Test
    public void testConcatEmpty() {
        ConsList<Integer> l = ConsList.of(1, 2);
        assertSame(l, l.concat(ConsList.empty()));
        assertSame(l, ConsList.<Integer>empty().concat(l));
    }

    @Test
    public void testConcatAll() {
        ConsList<ConsList<Integer>> lists = ConsList.of(
                ConsList.of(1, 2),
                ConsList.of(3),
                ConsList.of(4, 5));
        ConsList<Integer> flat = ConsList.concatAll(lists);
        assertEquals(List.of(1, 2, 3, 4, 5), flat.toArrayList());
    }

    @Test
    public void testContains() {
        ConsList<Integer> l = ConsList.of(1, 2, 3);
        assertTrue(l.contains(2));
        assertFalse(l.contains(4));
    }

    @Test
    public void testFromList() {
        List<String> javaList = List.of("a", "b", "c");
        ConsList<String> l = ConsList.fromList(javaList);
        assertEquals(3, l.size());
        assertEquals("a", l.head());
        assertEquals("c", l.tail().tail().head());
    }

    @Test
    public void testToArrayList() {
        ConsList<Integer> l = ConsList.of(1, 2, 3);
        ArrayList<Integer> al = l.toArrayList();
        assertEquals(List.of(1, 2, 3), al);
    }

    @Test
    public void testStream() {
        ConsList<Integer> l = ConsList.of(1, 2, 3, 4, 5);
        int sum = l.stream().mapToInt(i -> i).sum();
        assertEquals(15, sum);
    }

    @Test
    public void testIterator() {
        ConsList<Integer> l = ConsList.of(1, 2, 3);
        List<Integer> collected = new ArrayList<>();
        for (int i : l) {
            collected.add(i);
        }
        assertEquals(List.of(1, 2, 3), collected);
    }

    @Test
    public void testEquality() {
        ConsList<Integer> l1 = ConsList.of(1, 2, 3);
        ConsList<Integer> l2 = ConsList.of(1, 2, 3);
        ConsList<Integer> l3 = ConsList.of(1, 2);
        assertEquals(l1, l2);
        assertNotEquals(l1, l3);
        assertEquals(l1.hashCode(), l2.hashCode());
    }

    @Test
    public void testLargeList() {
        ConsList<Integer> l = ConsList.empty();
        for (int i = 9999; i >= 0; i--) {
            l = ConsList.cons(i, l);
        }
        assertEquals(10000, l.size());
        assertEquals(0, l.head());
        assertEquals(9999, l.last());
    }

    @Test
    public void testLargeListConsPerformance() {
        // Verify that building a list via repeated cons is O(n), not O(n^2)
        ConsList<Integer> l = ConsList.empty();
        for (int i = 0; i < 100000; i++) {
            l = ConsList.cons(i, l);
        }
        assertEquals(100000, l.size());
        assertEquals(99999, l.head());
    }
}

package hydra.lib;

import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import static hydra.lib.Lists.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class ListsTest {
    @Test
    public void applyIsCorrect() {
        assertEquals(Arrays.asList("ONE", "TWO", "THREE", "one", "two", "three"),
                apply(Arrays.asList(String::toUpperCase, String::toLowerCase), Arrays.asList("One", "Two", "Three")));
    }

    @Test
    public void bindIsCorrect() {
        Function<Integer, List<Integer>> mapping = n -> (n%2 == 0) ? Arrays.asList(n, n) : Collections.singletonList(n);

        assertEquals(Arrays.asList(1,2,2,3,4,4), bind(Arrays.asList(1,2,3,4), mapping));
    }

    @Test
    public void concatIsCorrect() {
        assertEquals(Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8), concat(Arrays.asList(
                Arrays.asList(1, 2, 3), Arrays.asList(4, 5), Arrays.asList(6, 7, 8), Collections.emptyList())));

        assertEquals(Collections.emptyList(), concat(Collections.emptyList()));
    }

    @Test
    public void headIsCorrect() {
        assertEquals(1, head(Arrays.asList(1, 2, 3)));

        assertThrows(IndexOutOfBoundsException.class, () -> head(Collections.emptyList()));
    }

    @Test
    public void intercalateIsCorrect() {
        assertEquals(Arrays.asList(1, 2, 3, 0, 0, 4, 5, 0, 0, 6, 7, 8), intercalate(Arrays.asList(0, 0),
                Arrays.asList(Arrays.asList(1, 2, 3), Arrays.asList(4, 5), Arrays.asList(6, 7, 8))));

        assertEquals(Collections.emptyList(), intercalate(Arrays.asList(0, 0), Collections.emptyList()));
    }

    @Test
    public void intersperseIsCorrect() {
        assertEquals(Arrays.asList("one", "and", "two", "and", "three"),
                intersperse("and", Arrays.asList("one", "two", "three")));

        assertEquals(Collections.emptyList(), intersperse("and", Collections.emptyList()));
    }

    @Test
    public void lastIsCorrect() {
        assertEquals(3, last(Arrays.asList(1, 2, 3)));

        assertThrows(IndexOutOfBoundsException.class, () -> last(Collections.emptyList()));
    }

    @Test
    public void lengthIsCorrect() {
        assertEquals(3, length(Arrays.asList(1, 2, 3)));
        assertEquals(0, length(Collections.emptyList()));
    }

    @Test
    public void mapIsCorrect() {
        assertEquals(Arrays.asList("ONE", "TWO"), map(String::toUpperCase, Arrays.asList("one", "two")));
    }

    @Test
    public void pureIsCorrect() {
        assertEquals(Collections.singletonList("one"), pure("one"));
    }
}

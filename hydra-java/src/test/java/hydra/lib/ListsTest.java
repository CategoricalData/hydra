package hydra.lib;

import org.junit.jupiter.api.Test;

import hydra.lib.strings.ToUpper;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import hydra.lib.lists.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class ListsTest {
    @Test
    public void applyIsCorrect() {
        assertEquals(Arrays.asList("ONE", "TWO", "THREE", "one", "two", "three"),
                Apply.apply(Arrays.asList(String::toUpperCase, String::toLowerCase), Arrays.asList("One", "Two", "Three")));
    }

    @Test
    public void bindIsCorrect() {
        Function<Integer, List<Integer>> mapping = n -> (n%2 == 0) ? Arrays.asList(n, n) : Collections.singletonList(n);

        assertEquals(Arrays.asList(1,2,2,3,4,4), Bind.apply(Arrays.asList(1,2,3,4), mapping));
    }

    @Test
    public void concatIsCorrect() {
        assertEquals(Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8), Concat.apply(Arrays.asList(
                Arrays.asList(1, 2, 3), Arrays.asList(4, 5), Arrays.asList(6, 7, 8), Collections.emptyList())));

        assertEquals(Collections.emptyList(), Concat.apply(Collections.emptyList()));
    }

    @Test
    public void headIsCorrect() {
        assertEquals(1, Head.apply(Arrays.asList(1, 2, 3)));

        assertThrows(IndexOutOfBoundsException.class, () -> Head.apply(Collections.emptyList()));
    }

    @Test
    public void intercalateIsCorrect() {
        assertEquals(Arrays.asList(1, 2, 3, 0, 0, 4, 5, 0, 0, 6, 7, 8), Intercalate.apply(Arrays.asList(0, 0),
                Arrays.asList(Arrays.asList(1, 2, 3), Arrays.asList(4, 5), Arrays.asList(6, 7, 8))));

        assertEquals(Collections.emptyList(), Intercalate.apply(Arrays.asList(0, 0), Collections.emptyList()));
    }

    @Test
    public void intersperseIsCorrect() {
        assertEquals(Arrays.asList("one", "and", "two", "and", "three"),
                Intersperse.apply("and", Arrays.asList("one", "two", "three")));

        assertEquals(Collections.emptyList(), Intersperse.apply("and", Collections.emptyList()));
    }

    @Test
    public void lastIsCorrect() {
        assertEquals(3, Last.apply(Arrays.asList(1, 2, 3)));

        assertThrows(IndexOutOfBoundsException.class, () -> Last.apply(Collections.emptyList()));
    }

    @Test
    public void lengthIsCorrect() {
        assertEquals(3, Length.apply(Arrays.asList(1, 2, 3)));
        assertEquals(0, Length.apply(Collections.emptyList()));
    }

    @Test
    public void mapIsCorrect() {
        assertEquals(Arrays.asList("ONE", "TWO"), Map.apply(String::toUpperCase, Arrays.asList("one", "two")));
    }

    @Test
    public void pureIsCorrect() {
        assertEquals(Collections.singletonList("one"), Pure.apply("one"));
    }
}

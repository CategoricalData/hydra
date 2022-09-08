package hydra.lib;

import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;

import static hydra.lib.Strings.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class StringsTest {
    @Test
    public void catIsCorrect() {
        assertEquals("onetwothree", cat(Arrays.asList("one", "two", "three")));
        assertEquals("one", cat(Arrays.asList("", "one", "", "")));
        assertEquals("", cat(Collections.emptyList()));
    }

    @Test
    public void lengthIsCorrect() {
        assertEquals(0, length(""));
        assertEquals(1, length("a"));
        assertEquals(3, length("one"));
    }

    @Test
    public void splitOnIsCorrect() {
        assertEquals(Arrays.asList("Mi", "i", "ippi"), splitOn("ss", "Mississippi"));
        assertEquals(Arrays.asList("", ""), splitOn("Mississippi", "Mississippi"));

        assertEquals(Arrays.asList("one", "two", "three"), splitOn(" ", "one two three"));
        assertEquals(Arrays.asList("", "one", "two", "three", ""), splitOn(" ", " one two three "));
        assertEquals(Arrays.asList("", "", "one", "two", "three", ""), splitOn(" ", "  one two three "));
        assertEquals(Arrays.asList("", "one two three "), splitOn("  ", "  one two three "));

        assertEquals(Arrays.asList("", "a"), splitOn("aa", "aaa"));

        assertEquals(Collections.singletonList(""), splitOn("a", ""));

        assertEquals(Arrays.asList("", "a", "b", "c"), splitOn("", "abc"));
        assertEquals(Collections.singletonList(""), splitOn("", ""));
    }

    @Test
    // TODO: test the behavior of toLower for extended characters. Currently, the implementation just inherits Java's String.toLower()
    public void toLowerIsCorrect() {
        assertEquals("one two three", toLower("One TWO threE"));
        assertEquals("abc123", toLower("AbC123"));
    }

    @Test
    // TODO: test the behavior of toUpper for extended characters. Currently, the implementation just inherits Java's String.toUpper()
    public void toUpperIsCorrect() {
        assertEquals("ONE TWO THREE", toUpper("One TWO threE"));
        assertEquals("ABC123", toUpper("AbC123"));
    }
}

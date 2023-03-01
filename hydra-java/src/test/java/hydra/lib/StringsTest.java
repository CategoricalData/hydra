package hydra.lib;

import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;

// import static hydra.dsl.prims.Strings.*;
import hydra.lib.strings.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class StringsTest {
    @Test
    public void catIsCorrect() {
        assertEquals("onetwothree", Cat.apply(Arrays.asList("one", "two", "three")));
        assertEquals("one", Cat.apply(Arrays.asList("", "one", "", "")));
        assertEquals("", Cat.apply(Collections.emptyList()));
    }

    @Test
    public void lengthIsCorrect() {
        assertEquals(0, Length.apply(""));
        assertEquals(1, Length.apply("a"));
        assertEquals(3, Length.apply("one"));
    }

    @Test
    public void splitOnIsCorrect() {
        assertEquals(Arrays.asList("Mi", "i", "ippi"), SplitOn.apply("ss", "Mississippi"));
        assertEquals(Arrays.asList("", ""), SplitOn.apply("Mississippi", "Mississippi"));

        assertEquals(Arrays.asList("one", "two", "three"), SplitOn.apply(" ", "one two three"));
        assertEquals(Arrays.asList("", "one", "two", "three", ""), SplitOn.apply(" ", " one two three "));
        assertEquals(Arrays.asList("", "", "one", "two", "three", ""), SplitOn.apply(" ", "  one two three "));
        assertEquals(Arrays.asList("", "one two three "), SplitOn.apply("  ", "  one two three "));

        assertEquals(Arrays.asList("", "a"), SplitOn.apply("aa", "aaa"));

        assertEquals(Collections.singletonList(""), SplitOn.apply("a", ""));

        assertEquals(Arrays.asList("", "a", "b", "c"), SplitOn.apply("", "abc"));
        assertEquals(Collections.singletonList(""), SplitOn.apply("", ""));
    }

    @Test
    // TODO: test the behavior of toLower for extended characters. Currently, the implementation just inherits Java's String.toLower()
    public void toLowerIsCorrect() {
        assertEquals("one two three", ToLower.apply("One TWO threE"));
        assertEquals("abc123", ToLower.apply("AbC123"));
    }

    @Test
    // TODO: test the behavior of toUpper for extended characters. Currently, the implementation just inherits Java's String.toUpper()
    public void toUpperIsCorrect() {
        assertEquals("ONE TWO THREE", ToUpper.apply("One TWO threE"));
        assertEquals("ABC123", ToUpper.apply("AbC123"));
    }
}

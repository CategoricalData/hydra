package hydra.lib;

import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.function.Function;
import java.util.HashSet;
import java.util.Set;

import hydra.lib.sets.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class SetsTest {
    @Test
    public void containsIsCorrect() {
        HashSet<Integer> testSet = new HashSet(Arrays.asList(1, 2, 3));

        assertEquals(true, Contains.apply(1, testSet));
        assertEquals(false, Contains.apply(4, testSet));
    }

    @Test
    public void emptyIsCorrect() {
        Set<Integer> testSet = Empty.apply();
        
        assertEquals(true, testSet.isEmpty());
    }

    @Test
    public void fromListIsCorrect() {
        Set<Integer> testSet = FromList.apply(Arrays.asList(1, 2, 3, 1));
        Set<Integer> comparisonSet = new HashSet(Arrays.asList(1, 2, 3));

        assertEquals(testSet, comparisonSet);        
    }

    @Test
    public void insertIsCorrect() {
        HashSet<Integer> testSet = new HashSet(Arrays.asList(1, 2));
        Set<Integer> newSet = Insert.apply(3, testSet);

        assertEquals(testSet, new HashSet(Arrays.asList(1, 2)));
        assertEquals(newSet, new HashSet(Arrays.asList(1, 2, 3)));
    }

    @Test
    public void isEmptyIsCorrect() {
        HashSet<Integer> emptySet = new HashSet();
        HashSet<Integer> nonEmptySet = new HashSet(Arrays.asList(1));

        assertEquals(true, IsEmpty.apply(emptySet));
        assertEquals(false, IsEmpty.apply(nonEmptySet));
    }

    @Test
    public void mapIsCorrect() {
        HashSet<Integer> testSet = new HashSet(Arrays.asList(1, 2, 3));
        Function<Integer, String> mapping = x -> "a".repeat(x);
        Set<String> newSet = Map.apply(mapping, testSet);

        assertEquals(newSet, new HashSet(Arrays.asList("a", "aa", "aaa")));
        assertEquals(testSet, new HashSet(Arrays.asList(1, 2, 3)));
    }

    @Test
    public void removeIsCorrect() {
        HashSet<Integer> testSet = new HashSet(Arrays.asList(1, 2, 3));
        HashSet<Integer> emptySet = new HashSet();
        Set<Integer> newSet1 = Remove.apply(4, testSet);
        Set<Integer> newSet2 = Remove.apply(3, testSet);
        Set<Integer> newSet3 = Remove.apply(1, emptySet);
    
        assertEquals(testSet, new HashSet(Arrays.asList(1, 2, 3)));
        assertEquals(emptySet, new HashSet());
        assertEquals(newSet1, new HashSet(Arrays.asList(1, 2, 3)));
        assertEquals(newSet2, new HashSet(Arrays.asList(1, 2)));
        assertEquals(newSet3, new HashSet());
        assertEquals(false, emptySet == newSet3);
    }

    @Test
    public void singletonIsCorrect() {
        Set<Integer> newSet = Singleton.apply(1);

        assertEquals(newSet, new HashSet(Arrays.asList(1)));
    }

    @Test
    public void sizeIsCorrect() {
        HashSet<Integer> testSet = new HashSet(Arrays.asList(1, 2, 3));
        HashSet<Integer> emptySet = new HashSet();

        assertEquals(Size.apply(testSet), 3);
        assertEquals(Size.apply(emptySet), 0);
    }

    @Test
    public void toListIsCorrect() {
        HashSet<Integer> testSet = new HashSet(Arrays.asList(1, 2, 3));
        HashSet<Integer> emptySet = new HashSet();

        assertEquals(ToList.apply(testSet), Arrays.asList(1, 2, 3));
        assertEquals(ToList.apply(emptySet), Arrays.asList());
    }
}

// Note: this is an automatically generated file. Do not edit.
// hydra.lib.sets primitives

package generation.hydra.test.lib;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import java.util.*;
import hydra.util.*;

public class SetsTest {

    // empty

    @Test

    public <T0> void testEmptyEmptySet() {

        assertEquals(

            (java.util.Set<T0>) (java.util.Set.<T0>of()),

            (java.util.Set<T0>) (hydra.lib.sets.Empty.<T0>apply()));

    }

    // singleton

    @Test

    public void testSingletonSingleElement() {

        assertEquals(

            java.util.stream.Stream.of(42).collect(java.util.stream.Collectors.toSet()),

            hydra.lib.sets.Singleton.apply(42));

    }

    // fromList

    @Test

    public void testFromlistCreateFromList() {

        assertEquals(

            java.util.stream.Stream.of(
  1,
  2,
  3).collect(java.util.stream.Collectors.toSet()),

            hydra.lib.sets.FromList.apply(java.util.List.of(
  1,
  2,
  3)));

    }

    @Test

    public void testFromlistDuplicatesRemoved() {

        assertEquals(

            java.util.stream.Stream.of(
  1,
  2,
  3).collect(java.util.stream.Collectors.toSet()),

            hydra.lib.sets.FromList.apply(java.util.List.of(
  1,
  2,
  1,
  3)));

    }

    @Test

    public <T0, T1> void testFromlistEmptyList() {

        assertEquals(

            (java.util.Set<T0>) (java.util.Set.<T0>of()),

            hydra.lib.sets.FromList.apply((java.util.List<T1>) (java.util.List.<T1>of())));

    }

    // toList

    @Test

    public void testTolistConvertToList() {

        assertEquals(

            java.util.List.of(
  1,
  2,
  3),

            hydra.lib.sets.ToList.apply(java.util.stream.Stream.of(
  1,
  2,
  3).collect(java.util.stream.Collectors.toSet())));

    }

    @Test

    public void testTolistUnsortedInput() {

        assertEquals(

            java.util.List.of(
  1,
  2,
  3),

            hydra.lib.sets.ToList.apply(java.util.stream.Stream.of(
  1,
  2,
  3).collect(java.util.stream.Collectors.toSet())));

    }

    @Test

    public <T0, T1> void testTolistEmptySet() {

        assertEquals(

            (java.util.List<T0>) (java.util.List.<T0>of()),

            hydra.lib.sets.ToList.apply((java.util.Set<T1>) (java.util.Set.<T1>of())));

    }

    // insert

    @Test

    public void testInsertInsertNewElement() {

        assertEquals(

            java.util.stream.Stream.of(
  1,
  2,
  3,
  4).collect(java.util.stream.Collectors.toSet()),

            hydra.lib.sets.Insert.apply(
  4,
  java.util.stream.Stream.of(
    1,
    2,
    3).collect(java.util.stream.Collectors.toSet())));

    }

    @Test

    public void testInsertInsertExistingElement() {

        assertEquals(

            java.util.stream.Stream.of(
  1,
  2,
  3).collect(java.util.stream.Collectors.toSet()),

            hydra.lib.sets.Insert.apply(
  2,
  java.util.stream.Stream.of(
    1,
    2,
    3).collect(java.util.stream.Collectors.toSet())));

    }

    @Test

    public void testInsertInsertIntoEmpty() {

        assertEquals(

            java.util.stream.Stream.of(1).collect(java.util.stream.Collectors.toSet()),

            hydra.lib.sets.Insert.apply(
  1,
  (java.util.Set<Integer>) (java.util.Set.<Integer>of())));

    }

    // delete

    @Test

    public void testDeleteDeleteExisting() {

        assertEquals(

            java.util.stream.Stream.of(
  1,
  3).collect(java.util.stream.Collectors.toSet()),

            hydra.lib.sets.Delete.apply(
  2,
  java.util.stream.Stream.of(
    1,
    2,
    3).collect(java.util.stream.Collectors.toSet())));

    }

    @Test

    public void testDeleteDeleteNonNegexisting() {

        assertEquals(

            java.util.stream.Stream.of(
  1,
  2,
  3).collect(java.util.stream.Collectors.toSet()),

            hydra.lib.sets.Delete.apply(
  4,
  java.util.stream.Stream.of(
    1,
    2,
    3).collect(java.util.stream.Collectors.toSet())));

    }

    @Test

    public <T0> void testDeleteDeleteFromEmpty() {

        assertEquals(

            (java.util.Set<T0>) (java.util.Set.<T0>of()),

            hydra.lib.sets.Delete.apply(
  1,
  (java.util.Set<Integer>) (java.util.Set.<Integer>of())));

    }

    // member

    @Test

    public void testMemberElementExists() {

        assertEquals(

            true,

            hydra.lib.sets.Member.apply(
  2,
  java.util.stream.Stream.of(
    1,
    2,
    3).collect(java.util.stream.Collectors.toSet())));

    }

    @Test

    public void testMemberElementMissing() {

        assertEquals(

            false,

            hydra.lib.sets.Member.apply(
  4,
  java.util.stream.Stream.of(
    1,
    2,
    3).collect(java.util.stream.Collectors.toSet())));

    }

    @Test

    public void testMemberEmptySet() {

        assertEquals(

            false,

            hydra.lib.sets.Member.apply(
  1,
  (java.util.Set<Integer>) (java.util.Set.<Integer>of())));

    }

    // size

    @Test

    public void testSizeThreeElements() {

        assertEquals(

            3,

            hydra.lib.sets.Size.apply(java.util.stream.Stream.of(
  1,
  2,
  3).collect(java.util.stream.Collectors.toSet())));

    }

    @Test

    public void testSizeSingleElement() {

        assertEquals(

            1,

            hydra.lib.sets.Size.apply(java.util.stream.Stream.of(42).collect(java.util.stream.Collectors.toSet())));

    }

    @Test

    public <T1> void testSizeEmptySet() {

        assertEquals(

            0,

            hydra.lib.sets.Size.apply((java.util.Set<T1>) (java.util.Set.<T1>of())));

    }

    // null

    @Test

    public <T1> void testNullEmptySet() {

        assertEquals(

            true,

            hydra.lib.sets.Null.apply((java.util.Set<T1>) (java.util.Set.<T1>of())));

    }

    @Test

    public void testNullNonNegemptySet() {

        assertEquals(

            false,

            hydra.lib.sets.Null.apply(java.util.stream.Stream.of(
  1,
  2).collect(java.util.stream.Collectors.toSet())));

    }

    // union

    @Test

    public void testUnionUnionTwoSets() {

        assertEquals(

            java.util.stream.Stream.of(
  1,
  2,
  3).collect(java.util.stream.Collectors.toSet()),

            hydra.lib.sets.Union.apply(
  java.util.stream.Stream.of(
    1,
    2).collect(java.util.stream.Collectors.toSet()),
  java.util.stream.Stream.of(
    2,
    3).collect(java.util.stream.Collectors.toSet())));

    }

    @Test

    public void testUnionUnionWithEmpty() {

        assertEquals(

            java.util.stream.Stream.of(
  1,
  2).collect(java.util.stream.Collectors.toSet()),

            hydra.lib.sets.Union.apply(
  java.util.stream.Stream.of(
    1,
    2).collect(java.util.stream.Collectors.toSet()),
  (java.util.Set<Integer>) (java.util.Set.<Integer>of())));

    }

    @Test

    public void testUnionEmptyWithNonNegempty() {

        assertEquals(

            java.util.stream.Stream.of(
  1,
  2).collect(java.util.stream.Collectors.toSet()),

            hydra.lib.sets.Union.apply(
  (java.util.Set<Integer>) (java.util.Set.<Integer>of()),
  java.util.stream.Stream.of(
    1,
    2).collect(java.util.stream.Collectors.toSet())));

    }

    // unions

    @Test

    public void testUnionsUnionOfMultipleSets() {

        assertEquals(

            java.util.stream.Stream.of(
  1,
  2,
  3,
  4).collect(java.util.stream.Collectors.toSet()),

            hydra.lib.sets.Unions.apply(java.util.List.of(
  java.util.stream.Stream.of(
    1,
    2).collect(java.util.stream.Collectors.toSet()),
  java.util.stream.Stream.of(
    2,
    3).collect(java.util.stream.Collectors.toSet()),
  java.util.stream.Stream.of(
    3,
    4).collect(java.util.stream.Collectors.toSet()))));

    }

    @Test

    public void testUnionsUnionWithEmptySets() {

        assertEquals(

            java.util.stream.Stream.of(
  1,
  2,
  3).collect(java.util.stream.Collectors.toSet()),

            hydra.lib.sets.Unions.apply(java.util.List.of(
  java.util.stream.Stream.of(
    1,
    2).collect(java.util.stream.Collectors.toSet()),
  (java.util.Set<Integer>) (java.util.Set.<Integer>of()),
  java.util.stream.Stream.of(3).collect(java.util.stream.Collectors.toSet()))));

    }

    @Test

    public <T0> void testUnionsEmptyListOfSets() {

        assertEquals(

            (java.util.Set<T0>) (java.util.Set.<T0>of()),

            hydra.lib.sets.Unions.apply((java.util.List<java.util.Set<T0>>) (java.util.List.<java.util.Set<T0>>of())));

    }

    @Test

    public void testUnionsSingleSet() {

        assertEquals(

            java.util.stream.Stream.of(
  1,
  2,
  3).collect(java.util.stream.Collectors.toSet()),

            hydra.lib.sets.Unions.apply(java.util.List.of(java.util.stream.Stream.of(
  1,
  2,
  3).collect(java.util.stream.Collectors.toSet()))));

    }

    // intersection

    @Test

    public void testIntersectionCommonElements() {

        assertEquals(

            java.util.stream.Stream.of(
  2,
  3).collect(java.util.stream.Collectors.toSet()),

            hydra.lib.sets.Intersection.apply(
  java.util.stream.Stream.of(
    1,
    2,
    3).collect(java.util.stream.Collectors.toSet()),
  java.util.stream.Stream.of(
    2,
    3,
    4).collect(java.util.stream.Collectors.toSet())));

    }

    @Test

    public <T0> void testIntersectionNoCommonElements() {

        assertEquals(

            (java.util.Set<T0>) (java.util.Set.<T0>of()),

            hydra.lib.sets.Intersection.apply(
  java.util.stream.Stream.of(
    1,
    2).collect(java.util.stream.Collectors.toSet()),
  java.util.stream.Stream.of(
    3,
    4).collect(java.util.stream.Collectors.toSet())));

    }

    @Test

    public <T0> void testIntersectionIntersectionWithEmpty() {

        assertEquals(

            (java.util.Set<T0>) (java.util.Set.<T0>of()),

            hydra.lib.sets.Intersection.apply(
  java.util.stream.Stream.of(
    1,
    2).collect(java.util.stream.Collectors.toSet()),
  (java.util.Set<Integer>) (java.util.Set.<Integer>of())));

    }

    // difference

    @Test

    public void testDifferenceRemoveElements() {

        assertEquals(

            java.util.stream.Stream.of(
  1,
  3).collect(java.util.stream.Collectors.toSet()),

            hydra.lib.sets.Difference.apply(
  java.util.stream.Stream.of(
    1,
    2,
    3).collect(java.util.stream.Collectors.toSet()),
  java.util.stream.Stream.of(
    2,
    4).collect(java.util.stream.Collectors.toSet())));

    }

    @Test

    public void testDifferenceNoOverlap() {

        assertEquals(

            java.util.stream.Stream.of(
  1,
  2).collect(java.util.stream.Collectors.toSet()),

            hydra.lib.sets.Difference.apply(
  java.util.stream.Stream.of(
    1,
    2).collect(java.util.stream.Collectors.toSet()),
  java.util.stream.Stream.of(
    3,
    4).collect(java.util.stream.Collectors.toSet())));

    }

    @Test

    public void testDifferenceDifferenceWithEmpty() {

        assertEquals(

            java.util.stream.Stream.of(
  1,
  2).collect(java.util.stream.Collectors.toSet()),

            hydra.lib.sets.Difference.apply(
  java.util.stream.Stream.of(
    1,
    2).collect(java.util.stream.Collectors.toSet()),
  (java.util.Set<Integer>) (java.util.Set.<Integer>of())));

    }

    // map

    @Test

    public void testMapMapFunction() {

        assertEquals(

            java.util.stream.Stream.of(
  2,
  4,
  6).collect(java.util.stream.Collectors.toSet()),

            hydra.lib.sets.Map.apply(
  (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
    x,
    2)),
  java.util.stream.Stream.of(
    1,
    2,
    3).collect(java.util.stream.Collectors.toSet())));

    }

    @Test

    public <T0> void testMapMapOnEmpty() {

        assertEquals(

            (java.util.Set<T0>) (java.util.Set.<T0>of()),

            hydra.lib.sets.Map.apply(
  (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
    x,
    2)),
  (java.util.Set<Integer>) (java.util.Set.<Integer>of())));

    }
}

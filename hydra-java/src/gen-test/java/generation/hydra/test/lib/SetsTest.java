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

            (hydra.util.PersistentSet<T0>) (hydra.util.PersistentSet.<T0>empty()),

            (hydra.util.PersistentSet<T0>) (hydra.lib.sets.Empty.<T0>apply()));

    }

    // singleton

    @Test

    public void testSingletonSingleElement() {

        assertEquals(

            hydra.util.PersistentSet.of(42),

            hydra.lib.sets.Singleton.apply(42));

    }

    // fromList

    @Test

    public void testFromlistCreateFromList() {

        assertEquals(

            hydra.util.PersistentSet.of(
  1,
  2,
  3),

            hydra.lib.sets.FromList.apply(hydra.util.ConsList.of(
  1,
  2,
  3)));

    }

    @Test

    public void testFromlistDuplicatesRemoved() {

        assertEquals(

            hydra.util.PersistentSet.of(
  1,
  2,
  3),

            hydra.lib.sets.FromList.apply(hydra.util.ConsList.of(
  1,
  2,
  1,
  3)));

    }

    @Test

    public <T0, T1> void testFromlistEmptyList() {

        assertEquals(

            (hydra.util.PersistentSet<T0>) (hydra.util.PersistentSet.<T0>empty()),

            hydra.lib.sets.FromList.apply((hydra.util.ConsList<T1>) (hydra.util.ConsList.<T1>of())));

    }

    // toList

    @Test

    public void testTolistConvertToList() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2,
  3),

            hydra.lib.sets.ToList.apply(hydra.util.PersistentSet.of(
  1,
  2,
  3)));

    }

    @Test

    public void testTolistUnsortedInput() {

        assertEquals(

            hydra.util.ConsList.of(
  1,
  2,
  3),

            hydra.lib.sets.ToList.apply(hydra.util.PersistentSet.of(
  1,
  2,
  3)));

    }

    @Test

    public <T0, T1> void testTolistEmptySet() {

        assertEquals(

            (hydra.util.ConsList<T0>) (hydra.util.ConsList.<T0>of()),

            hydra.lib.sets.ToList.apply((hydra.util.PersistentSet<T1>) (hydra.util.PersistentSet.<T1>empty())));

    }

    // insert

    @Test

    public void testInsertInsertNewElement() {

        assertEquals(

            hydra.util.PersistentSet.of(
  1,
  2,
  3,
  4),

            hydra.lib.sets.Insert.apply(
  4,
  hydra.util.PersistentSet.of(
    1,
    2,
    3)));

    }

    @Test

    public void testInsertInsertExistingElement() {

        assertEquals(

            hydra.util.PersistentSet.of(
  1,
  2,
  3),

            hydra.lib.sets.Insert.apply(
  2,
  hydra.util.PersistentSet.of(
    1,
    2,
    3)));

    }

    @Test

    public void testInsertInsertIntoEmpty() {

        assertEquals(

            hydra.util.PersistentSet.of(1),

            hydra.lib.sets.Insert.apply(
  1,
  (hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty())));

    }

    // delete

    @Test

    public void testDeleteDeleteExisting() {

        assertEquals(

            hydra.util.PersistentSet.of(
  1,
  3),

            hydra.lib.sets.Delete.apply(
  2,
  hydra.util.PersistentSet.of(
    1,
    2,
    3)));

    }

    @Test

    public void testDeleteDeleteNonNegexisting() {

        assertEquals(

            hydra.util.PersistentSet.of(
  1,
  2,
  3),

            hydra.lib.sets.Delete.apply(
  4,
  hydra.util.PersistentSet.of(
    1,
    2,
    3)));

    }

    @Test

    public <T0> void testDeleteDeleteFromEmpty() {

        assertEquals(

            (hydra.util.PersistentSet<T0>) (hydra.util.PersistentSet.<T0>empty()),

            hydra.lib.sets.Delete.apply(
  1,
  (hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty())));

    }

    // member

    @Test

    public void testMemberElementExists() {

        assertEquals(

            true,

            hydra.lib.sets.Member.apply(
  2,
  hydra.util.PersistentSet.of(
    1,
    2,
    3)));

    }

    @Test

    public void testMemberElementMissing() {

        assertEquals(

            false,

            hydra.lib.sets.Member.apply(
  4,
  hydra.util.PersistentSet.of(
    1,
    2,
    3)));

    }

    @Test

    public void testMemberEmptySet() {

        assertEquals(

            false,

            hydra.lib.sets.Member.apply(
  1,
  (hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty())));

    }

    // size

    @Test

    public void testSizeThreeElements() {

        assertEquals(

            3,

            hydra.lib.sets.Size.apply(hydra.util.PersistentSet.of(
  1,
  2,
  3)));

    }

    @Test

    public void testSizeSingleElement() {

        assertEquals(

            1,

            hydra.lib.sets.Size.apply(hydra.util.PersistentSet.of(42)));

    }

    @Test

    public <T1> void testSizeEmptySet() {

        assertEquals(

            0,

            hydra.lib.sets.Size.apply((hydra.util.PersistentSet<T1>) (hydra.util.PersistentSet.<T1>empty())));

    }

    // null

    @Test

    public <T1> void testNullEmptySet() {

        assertEquals(

            true,

            hydra.lib.sets.Null.apply((hydra.util.PersistentSet<T1>) (hydra.util.PersistentSet.<T1>empty())));

    }

    @Test

    public void testNullNonNegemptySet() {

        assertEquals(

            false,

            hydra.lib.sets.Null.apply(hydra.util.PersistentSet.of(
  1,
  2)));

    }

    // union

    @Test

    public void testUnionUnionTwoSets() {

        assertEquals(

            hydra.util.PersistentSet.of(
  1,
  2,
  3),

            hydra.lib.sets.Union.apply(
  hydra.util.PersistentSet.of(
    1,
    2),
  hydra.util.PersistentSet.of(
    2,
    3)));

    }

    @Test

    public void testUnionUnionWithEmpty() {

        assertEquals(

            hydra.util.PersistentSet.of(
  1,
  2),

            hydra.lib.sets.Union.apply(
  hydra.util.PersistentSet.of(
    1,
    2),
  (hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty())));

    }

    @Test

    public void testUnionEmptyWithNonNegempty() {

        assertEquals(

            hydra.util.PersistentSet.of(
  1,
  2),

            hydra.lib.sets.Union.apply(
  (hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty()),
  hydra.util.PersistentSet.of(
    1,
    2)));

    }

    // unions

    @Test

    public void testUnionsUnionOfMultipleSets() {

        assertEquals(

            hydra.util.PersistentSet.of(
  1,
  2,
  3,
  4),

            hydra.lib.sets.Unions.apply(hydra.util.ConsList.of(
  hydra.util.PersistentSet.of(
    1,
    2),
  hydra.util.PersistentSet.of(
    2,
    3),
  hydra.util.PersistentSet.of(
    3,
    4))));

    }

    @Test

    public void testUnionsUnionWithEmptySets() {

        assertEquals(

            hydra.util.PersistentSet.of(
  1,
  2,
  3),

            hydra.lib.sets.Unions.apply(hydra.util.ConsList.of(
  hydra.util.PersistentSet.of(
    1,
    2),
  (hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty()),
  hydra.util.PersistentSet.of(3))));

    }

    @Test

    public <T0> void testUnionsEmptyListOfSets() {

        assertEquals(

            (hydra.util.PersistentSet<T0>) (hydra.util.PersistentSet.<T0>empty()),

            hydra.lib.sets.Unions.apply((hydra.util.ConsList<hydra.util.PersistentSet<T0>>) (hydra.util.ConsList.<hydra.util.PersistentSet<T0>>of())));

    }

    @Test

    public void testUnionsSingleSet() {

        assertEquals(

            hydra.util.PersistentSet.of(
  1,
  2,
  3),

            hydra.lib.sets.Unions.apply(hydra.util.ConsList.of(hydra.util.PersistentSet.of(
  1,
  2,
  3))));

    }

    // intersection

    @Test

    public void testIntersectionCommonElements() {

        assertEquals(

            hydra.util.PersistentSet.of(
  2,
  3),

            hydra.lib.sets.Intersection.apply(
  hydra.util.PersistentSet.of(
    1,
    2,
    3),
  hydra.util.PersistentSet.of(
    2,
    3,
    4)));

    }

    @Test

    public <T0> void testIntersectionNoCommonElements() {

        assertEquals(

            (hydra.util.PersistentSet<T0>) (hydra.util.PersistentSet.<T0>empty()),

            hydra.lib.sets.Intersection.apply(
  hydra.util.PersistentSet.of(
    1,
    2),
  hydra.util.PersistentSet.of(
    3,
    4)));

    }

    @Test

    public <T0> void testIntersectionIntersectionWithEmpty() {

        assertEquals(

            (hydra.util.PersistentSet<T0>) (hydra.util.PersistentSet.<T0>empty()),

            hydra.lib.sets.Intersection.apply(
  hydra.util.PersistentSet.of(
    1,
    2),
  (hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty())));

    }

    // difference

    @Test

    public void testDifferenceRemoveElements() {

        assertEquals(

            hydra.util.PersistentSet.of(
  1,
  3),

            hydra.lib.sets.Difference.apply(
  hydra.util.PersistentSet.of(
    1,
    2,
    3),
  hydra.util.PersistentSet.of(
    2,
    4)));

    }

    @Test

    public void testDifferenceNoOverlap() {

        assertEquals(

            hydra.util.PersistentSet.of(
  1,
  2),

            hydra.lib.sets.Difference.apply(
  hydra.util.PersistentSet.of(
    1,
    2),
  hydra.util.PersistentSet.of(
    3,
    4)));

    }

    @Test

    public void testDifferenceDifferenceWithEmpty() {

        assertEquals(

            hydra.util.PersistentSet.of(
  1,
  2),

            hydra.lib.sets.Difference.apply(
  hydra.util.PersistentSet.of(
    1,
    2),
  (hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty())));

    }

    // map

    @Test

    public void testMapMapFunction() {

        assertEquals(

            hydra.util.PersistentSet.of(
  2,
  4,
  6),

            hydra.lib.sets.Map.apply(
  (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
    x,
    2)),
  hydra.util.PersistentSet.of(
    1,
    2,
    3)));

    }

    @Test

    public <T0> void testMapMapOnEmpty() {

        assertEquals(

            (hydra.util.PersistentSet<T0>) (hydra.util.PersistentSet.<T0>empty()),

            hydra.lib.sets.Map.apply(
  (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
    x,
    2)),
  (hydra.util.PersistentSet<Integer>) (hydra.util.PersistentSet.<Integer>empty())));

    }
}

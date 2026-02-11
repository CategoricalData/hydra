// Note: this is an automatically generated file. Do not edit.
// hydra.lib.pairs primitives

package generation.hydra.test.lib;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import java.util.*;
import hydra.util.*;

public class PairsTest {

    // bimap

    @Test

    public void testBimapTransformBothElements() {

        assertEquals(

            (hydra.util.Tuple.Tuple2<Integer, Integer>) ((hydra.util.Tuple.Tuple2<Integer, Integer>) (new hydra.util.Tuple.Tuple2<Integer, Integer>(10, 2))),

            hydra.lib.pairs.Bimap.apply(
  (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
    x,
    2)),
  (java.util.function.Function<String, Integer>) (s -> hydra.lib.strings.Length.apply(s)),
  (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(5, "ab")))));

    }

    @Test

    public void testBimapWithZero() {

        assertEquals(

            (hydra.util.Tuple.Tuple2<Integer, Integer>) ((hydra.util.Tuple.Tuple2<Integer, Integer>) (new hydra.util.Tuple.Tuple2<Integer, Integer>(0, 5))),

            hydra.lib.pairs.Bimap.apply(
  (java.util.function.Function<Integer, Integer>) (x -> hydra.lib.math.Mul.apply(
    x,
    2)),
  (java.util.function.Function<String, Integer>) (s -> hydra.lib.strings.Length.apply(s)),
  (hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(0, "hello")))));

    }

    // first

    @Test

    public void testFirstExtractFirstElement() {

        assertEquals(

            42,

            hydra.lib.pairs.First.apply((hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(42, "hello")))));

    }

    @Test

    public void testFirstWithZero() {

        assertEquals(

            0,

            hydra.lib.pairs.First.apply((hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(0, "world")))));

    }

    @Test

    public void testFirstNegativeNumber() {

        assertEquals(

            -5,

            hydra.lib.pairs.First.apply((hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(-5, "test")))));

    }

    // second

    @Test

    public void testSecondExtractSecondElement() {

        assertEquals(

            "hello",

            hydra.lib.pairs.Second.apply((hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(42, "hello")))));

    }

    @Test

    public void testSecondEmptyString() {

        assertEquals(

            "",

            hydra.lib.pairs.Second.apply((hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(0, "")))));

    }

    @Test

    public void testSecondLongString() {

        assertEquals(

            "testing",

            hydra.lib.pairs.Second.apply((hydra.util.Tuple.Tuple2<Integer, String>) ((hydra.util.Tuple.Tuple2<Integer, String>) (new hydra.util.Tuple.Tuple2<Integer, String>(123, "testing")))));

    }
}

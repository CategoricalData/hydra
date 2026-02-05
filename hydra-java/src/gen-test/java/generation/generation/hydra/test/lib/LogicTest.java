// Note: this is an automatically generated file. Do not edit.
// hydra.lib.logic primitives

package generation.hydra.test.lib;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import java.util.*;
import hydra.util.*;

public class LogicTest {

    // and

    @Test

    public void testAndTrueAndTrue() {

        assertEquals(

            true,

            hydra.lib.logic.And.apply(
  true,
  true));

    }

    @Test

    public void testAndTrueAndFalse() {

        assertEquals(

            false,

            hydra.lib.logic.And.apply(
  true,
  false));

    }

    @Test

    public void testAndFalseAndTrue() {

        assertEquals(

            false,

            hydra.lib.logic.And.apply(
  false,
  true));

    }

    @Test

    public void testAndFalseAndFalse() {

        assertEquals(

            false,

            hydra.lib.logic.And.apply(
  false,
  false));

    }

    // ifElse

    // boolean values

    @Test

    public void testIfelseBooleanValuesTrueConditionReturnsThen() {

        assertEquals(

            true,

            hydra.lib.logic.IfElse.lazy(
  true,
  () -> true,
  () -> false));

    }

    @Test

    public void testIfelseBooleanValuesFalseConditionReturnsElse() {

        assertEquals(

            false,

            hydra.lib.logic.IfElse.lazy(
  false,
  () -> true,
  () -> false));

    }

    // integer values

    @Test

    public void testIfelseIntegerValuesTrueSelectsFirstInt() {

        assertEquals(

            42,

            hydra.lib.logic.IfElse.lazy(
  true,
  () -> 42,
  () -> 0));

    }

    @Test

    public void testIfelseIntegerValuesFalseSelectsSecondInt() {

        assertEquals(

            0,

            hydra.lib.logic.IfElse.lazy(
  false,
  () -> 42,
  () -> 0));

    }

    // string values

    @Test

    public void testIfelseStringValuesTrueSelectsFirstString() {

        assertEquals(

            "yes",

            hydra.lib.logic.IfElse.lazy(
  true,
  () -> "yes",
  () -> "no"));

    }

    @Test

    public void testIfelseStringValuesFalseSelectsSecondString() {

        assertEquals(

            "no",

            hydra.lib.logic.IfElse.lazy(
  false,
  () -> "yes",
  () -> "no"));

    }

    // not

    @Test

    public void testNotNotTrue() {

        assertEquals(

            false,

            hydra.lib.logic.Not.apply(true));

    }

    @Test

    public void testNotNotFalse() {

        assertEquals(

            true,

            hydra.lib.logic.Not.apply(false));

    }

    // or

    @Test

    public void testOrTrueOrTrue() {

        assertEquals(

            true,

            hydra.lib.logic.Or.apply(
  true,
  true));

    }

    @Test

    public void testOrTrueOrFalse() {

        assertEquals(

            true,

            hydra.lib.logic.Or.apply(
  true,
  false));

    }

    @Test

    public void testOrFalseOrTrue() {

        assertEquals(

            true,

            hydra.lib.logic.Or.apply(
  false,
  true));

    }

    @Test

    public void testOrFalseOrFalse() {

        assertEquals(

            false,

            hydra.lib.logic.Or.apply(
  false,
  false));

    }
}

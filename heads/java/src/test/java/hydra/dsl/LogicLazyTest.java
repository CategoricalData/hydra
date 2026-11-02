package hydra.core.overlay.java.lib.logic;

import org.junit.jupiter.api.Test;

import java.util.function.Supplier;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * Confirms that the thunk-accepting {@code applyLazy} overloads of {@code and}/{@code or}
 * (#802) actually short-circuit: the second operand's supplier must not be invoked when the
 * first operand already determines the result.
 */
public class LogicLazyTest {

    private static Supplier<Boolean> explodingSupplier() {
        return () -> {
            throw new AssertionError("second operand was forced despite being short-circuited");
        };
    }

    @Test
    public void andShortCircuitsOnFalse() {
        assertFalse(And.applyLazy(false, explodingSupplier()));
    }

    @Test
    public void andEvaluatesSecondOperandWhenFirstIsTrue() {
        assertTrue(And.applyLazy(true, () -> true));
        assertFalse(And.applyLazy(true, () -> false));
    }

    @Test
    public void orShortCircuitsOnTrue() {
        assertTrue(Or.applyLazy(true, explodingSupplier()));
    }

    @Test
    public void orEvaluatesSecondOperandWhenFirstIsFalse() {
        assertTrue(Or.applyLazy(false, () -> true));
        assertFalse(Or.applyLazy(false, () -> false));
    }
}

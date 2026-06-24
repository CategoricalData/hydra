package hydra.overlay.java.dsl.prims;

import hydra.core.Term;
import hydra.overlay.java.lib.math.Add;
import hydra.overlay.java.lib.math.Max;
import hydra.overlay.java.lib.math.Min;
import hydra.overlay.java.lib.math.Mul;
import hydra.overlay.java.lib.math.Negate;
import hydra.overlay.java.lib.math.Sub;

/**
 * DSL interface providing mathematical primitive operations.
 */
public interface Math {
    /**
     * Returns a term representing the add primitive operation.
     *
     * @return a term for adding two numeric values
     */
    static Term add() {
        return new Add().term();
    }

    /**
     * Returns a term representing the max primitive operation.
     *
     * @return a term for computing the maximum of two values
     */
    static Term max() {
        return new Max().term();
    }

    /**
     * Returns a term representing the min primitive operation.
     *
     * @return a term for computing the minimum of two values
     */
    static Term min() {
        return new Min().term();
    }

    /**
     * Returns a term representing the mul primitive operation.
     *
     * @return a term for multiplying two numeric values
     */
    static Term mul() {
        return new Mul().term();
    }

    /**
     * Returns a term representing the neg primitive operation.
     *
     * @return a term for negating a numeric value
     */
    static Term neg() {
        return new Negate().term();
    }

    /**
     * Returns a term representing the sub primitive operation.
     *
     * @return a term for subtracting two numeric values
     */
    static Term sub() {
        return new Sub().term();
    }
}

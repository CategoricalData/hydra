package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.math.Add;
import hydra.lib.math.Div;
import hydra.lib.math.Mod;
import hydra.lib.math.Mul;
import hydra.lib.math.Neg;
import hydra.lib.math.Rem;
import hydra.lib.math.Sub;

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
     * Returns a term representing the div primitive operation.
     *
     * @return a term for dividing two numeric values
     */
    static Term div() {
        return new Div().term();
    }

    /**
     * Returns a term representing the mod primitive operation.
     *
     * @return a term for computing the modulus of two values
     */
    static Term mod() {
        return new Mod().term();
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
        return new Neg().term();
    }

    /**
     * Returns a term representing the rem primitive operation.
     *
     * @return a term for computing the remainder of two values
     */
    static Term rem() {
        return new Rem().term();
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

package hydra.overlay.java.dsl.prims;

import hydra.core.Term;
import hydra.overlay.java.lib.equality.Compare;
import hydra.overlay.java.lib.equality.Equal;
import hydra.overlay.java.lib.equality.Gt;
import hydra.overlay.java.lib.equality.Gte;
import hydra.overlay.java.lib.equality.Identity;
import hydra.overlay.java.lib.equality.Lt;
import hydra.overlay.java.lib.equality.Lte;
import hydra.overlay.java.lib.equality.Max;
import hydra.overlay.java.lib.equality.Min;

/**
 * DSL interface providing equality and comparison primitive operations.
 */
public interface Equality {
    /**
     * Returns a term representing the compare primitive operation.
     *
     * @return a term for comparing two values
     */
    static Term compare() {
        return new Compare().term();
    }

    /**
     * Returns a term representing the equal primitive operation.
     *
     * @return a term for testing equality of two values
     */
    static Term equal() {
        return new Equal().term();
    }

    /**
     * Returns a term representing the greater-than primitive operation.
     *
     * @return a term for testing if one value is greater than another
     */
    static Term gt() {
        return new Gt().term();
    }

    /**
     * Returns a term representing the greater-than-or-equal primitive operation.
     *
     * @return a term for testing if one value is greater than or equal to another
     */
    static Term gte() {
        return new Gte().term();
    }

    /**
     * Returns a term representing the identity primitive operation.
     *
     * @return a term for the identity function
     */
    static Term identity() {
        return new Identity().term();
    }

    /**
     * Returns a term representing the less-than primitive operation.
     *
     * @return a term for testing if one value is less than another
     */
    static Term lt() {
        return new Lt().term();
    }

    /**
     * Returns a term representing the less-than-or-equal primitive operation.
     *
     * @return a term for testing if one value is less than or equal to another
     */
    static Term lte() {
        return new Lte().term();
    }

    /**
     * Returns a term representing the max primitive operation.
     *
     * @return a term for finding the maximum of two values
     */
    static Term max() {
        return new Max().term();
    }

    /**
     * Returns a term representing the min primitive operation.
     *
     * @return a term for finding the minimum of two values
     */
    static Term min() {
        return new Min().term();
    }
}

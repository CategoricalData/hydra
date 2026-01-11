package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.pairs.Bimap;
import hydra.lib.pairs.First;
import hydra.lib.pairs.Second;

/**
 * DSL interface providing pair primitive operations.
 */
public interface Pairs {
    /**
     * Returns a term representing the bimap primitive operation for pairs.
     * Applies two functions to the first and second elements of a pair respectively.
     *
     * @return a term for bimap over pairs
     */
    static Term bimap() {
        return new Bimap().term();
    }

    /**
     * Returns a term representing the first primitive operation for pairs.
     * Extracts the first element of a pair.
     *
     * @return a term for extracting the first element
     */
    static Term first() {
        return new First().term();
    }

    /**
     * Returns a term representing the second primitive operation for pairs.
     * Extracts the second element of a pair.
     *
     * @return a term for extracting the second element
     */
    static Term second() {
        return new Second().term();
    }
}

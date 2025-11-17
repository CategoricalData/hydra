package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.strings.Length;
import hydra.lib.strings.Cat;
import hydra.lib.strings.SplitOn;
import hydra.lib.strings.ToLower;
import hydra.lib.strings.ToUpper;

/**
 * DSL interface providing string primitive operations.
 */
public interface Strings {
    /**
     * Returns a term representing the cat (concatenate) primitive operation for strings.
     *
     * @return a term for concatenating two strings
     */
    static Term cat() {
        return new Cat().term();
    }

    /**
     * Returns a term representing the length primitive operation for strings.
     *
     * @return a term for getting the length of a string
     */
    static Term length() {
        return new Length().term();
    }

    /**
     * Returns a term representing the splitOn primitive operation for strings.
     *
     * @return a term for splitting a string on a delimiter
     */
    static Term splitOn() {
        return new SplitOn().term();
    }

    /**
     * Returns a term representing the toLower primitive operation for strings.
     *
     * @return a term for converting a string to lowercase
     */
    static Term toLower() {
        return new ToLower().term();
    }

    /**
     * Returns a term representing the toUpper primitive operation for strings.
     *
     * @return a term for converting a string to uppercase
     */
    static Term toUpper() {
        return new ToUpper().term();
    }
}

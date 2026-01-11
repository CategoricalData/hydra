package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.chars.IsAlphaNum;
import hydra.lib.chars.IsLower;
import hydra.lib.chars.IsSpace;
import hydra.lib.chars.IsUpper;
import hydra.lib.chars.ToLower;
import hydra.lib.chars.ToUpper;

/**
 * DSL interface providing character primitive operations.
 */
public interface Chars {
    /**
     * Returns a term representing the isAlphaNum primitive operation.
     * Tests if a character is alphanumeric.
     *
     * @return a term for testing if alphanumeric
     */
    static Term isAlphaNum() {
        return new IsAlphaNum().term();
    }

    /**
     * Returns a term representing the isLower primitive operation.
     * Tests if a character is lowercase.
     *
     * @return a term for testing if lowercase
     */
    static Term isLower() {
        return new IsLower().term();
    }

    /**
     * Returns a term representing the isSpace primitive operation.
     * Tests if a character is whitespace.
     *
     * @return a term for testing if whitespace
     */
    static Term isSpace() {
        return new IsSpace().term();
    }

    /**
     * Returns a term representing the isUpper primitive operation.
     * Tests if a character is uppercase.
     *
     * @return a term for testing if uppercase
     */
    static Term isUpper() {
        return new IsUpper().term();
    }

    /**
     * Returns a term representing the toLower primitive operation.
     * Converts a character to lowercase.
     *
     * @return a term for converting to lowercase
     */
    static Term toLower() {
        return new ToLower().term();
    }

    /**
     * Returns a term representing the toUpper primitive operation.
     * Converts a character to uppercase.
     *
     * @return a term for converting to uppercase
     */
    static Term toUpper() {
        return new ToUpper().term();
    }
}

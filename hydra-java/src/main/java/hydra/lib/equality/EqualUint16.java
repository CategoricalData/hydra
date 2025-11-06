package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


/**
 * Tests equality of two 16-bit unsigned integers.
 */
public class EqualUint16 extends EqualityFunction<Character> {
    public EqualUint16() {
        super(PrimitiveType.uint16(), Relation.EQUALS);
    }

    /**
     * Applies the EqualUint16 operation.
     * @param second the second
     * @return the result
     */
        public static Function<Character, Boolean> apply(Character second) {
        return first -> apply(first, second);
    }

    /**
     * Applies the EqualUint16 operation.
     * @param first the first
     * @param second the second
     * @return the result
     */
        public static Boolean apply(Character first, Character second) {
        return first.equals(second);
    }
}

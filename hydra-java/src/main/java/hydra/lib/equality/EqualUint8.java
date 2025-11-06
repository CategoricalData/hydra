package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


/**
 * Tests equality of two 8-bit unsigned integers.
 */
public class EqualUint8 extends EqualityFunction<Character> {
    public EqualUint8() {
        super(PrimitiveType.uint8(), Relation.EQUALS);
    }

    /**
     * Applies the EqualUint8 operation.
     * @param second the second
     * @return the result
     */
        public static Function<Character, Boolean> apply(Character second) {
        return first -> apply(first, second);
    }

    /**
     * Applies the EqualUint8 operation.
     * @param first the first
     * @param second the second
     * @return the result
     */
        public static Boolean apply(Character first, Character second) {
        return first.equals(second);
    }
}

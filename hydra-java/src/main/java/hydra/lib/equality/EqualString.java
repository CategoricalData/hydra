package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


/**
 * Tests equality of two strings.
 */
public class EqualString extends EqualityFunction<String> {
    public EqualString() {
        super(PrimitiveType.string(), Relation.EQUALS);
    }

    /**
     * Applies the EqualString operation.
     * @param second the second
     * @return the result
     */
        public static Function<String, Boolean> apply(String second) {
        return first -> apply(first, second);
    }

    /**
     * Applies the EqualString operation.
     * @param first the first
     * @param second the second
     * @return the result
     */
        public static Boolean apply(String first, String second) {
        return first.equals(second);
    }
}

package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


/**
 * Tests equality of two binary strings.
 */
public class EqualBinary extends EqualityFunction<String> {
    public EqualBinary() {
        super(PrimitiveType.binary(), Relation.EQUALS);
    }

    /**
     * Applies the EqualBinary operation.
     * @param second the second
     * @return the result
     */
        public static Function<String, Boolean> apply(String second) {
        return first -> apply(first, second);
    }

    /**
     * Applies the EqualBinary operation.
     * @param first the first
     * @param second the second
     * @return the result
     */
        public static Boolean apply(String first, String second) {
        return first.equals(second);
    }
}

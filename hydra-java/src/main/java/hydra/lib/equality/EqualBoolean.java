package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


/**
 * Tests equality of two boolean values.
 */
public class EqualBoolean extends EqualityFunction<Boolean> {
    public EqualBoolean() {
        super(PrimitiveType.boolean_(), Relation.EQUALS);
    }

    /**
     * Applies the EqualBoolean operation.
     * @param second the second
     * @return the result
     */
        public static Function<Boolean, Boolean> apply(Boolean second) {
        return first -> apply(first, second);
    }

    /**
     * Applies the EqualBoolean operation.
     * @param first the first
     * @param second the second
     * @return the result
     */
        public static Boolean apply(Boolean first, Boolean second) {
        return first.equals(second);
    }
}

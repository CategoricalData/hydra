package hydra.lib.equality;

import hydra.core.Type;
import hydra.lib.PrimitiveType;

import java.util.function.Function;


/**
 * Tests equality of two types.
 */
public class EqualType extends EqualityFunction<Type> {
    public EqualType() {
        super(PrimitiveType.type(), Relation.EQUALS);
    }

    /**
     * Applies the EqualType operation.
     * @param second the second
     * @return the result
     */
        public static  Function<Type, Boolean> apply(Type second) {
        return first -> apply(first, second);
    }

    /**
     * Applies the EqualType operation.
     * @param first the first
     * @param second the second
     * @return the result
     */
        public static  Boolean apply(Type first, Type second) {
        return first.equals(second);
    }
}

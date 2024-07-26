package hydra.lib.equality;

import hydra.core.Type;
import hydra.lib.PrimitiveType;

import java.util.function.Function;


public class EqualType extends EqualityFunction<Type> {
    public EqualType() {
        super(PrimitiveType.type(), Relation.EQUALS);
    }

    public static  Function<Type, Boolean> apply(Type second) {
        return first -> apply(first, second);
    }

    public static  Boolean apply(Type first, Type second) {
        return first.equals(second);
    }
}

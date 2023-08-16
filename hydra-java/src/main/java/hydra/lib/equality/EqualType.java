package hydra.lib.equality;

import hydra.core.Type;
import hydra.lib.PrimitiveType;

import java.util.function.Function;


public class EqualType<A> extends EqualityFunction<A, Type<A>> {
    public EqualType() {
        super(PrimitiveType.type());
    }

    public static <A> Function<Type<A>, Boolean> apply(Type<A> second) {
        return first -> apply(first, second);
    }

    public static <A> Boolean apply(Type<A> first, Type<A> second) {
        return first.equals(second);
    }
}

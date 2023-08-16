package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


public class EqualUint16<A> extends EqualityFunction<A, Character> {
    public EqualUint16() {
        super(PrimitiveType.uint16());
    }

    public static Function<Character, Boolean> apply(Character second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Character first, Character second) {
        return 0 == first.compareTo(second);
    }
}

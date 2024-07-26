package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


public class EqualUint16 extends EqualityFunction<Character> {
    public EqualUint16() {
        super(PrimitiveType.uint16(), Relation.EQUALS);
    }

    public static Function<Character, Boolean> apply(Character second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Character first, Character second) {
        return first.equals(second);
    }
}

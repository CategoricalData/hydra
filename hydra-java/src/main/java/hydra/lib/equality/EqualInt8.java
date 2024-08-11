package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


public class EqualInt8 extends EqualityFunction<Byte> {
    public EqualInt8() {
        super(PrimitiveType.int8(), Relation.EQUALS);
    }

    public static Function<Byte, Boolean> apply(Byte second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Byte first, Byte second) {
        return first.equals(second);
    }
}

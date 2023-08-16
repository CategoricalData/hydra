package hydra.lib.equality;

import hydra.lib.PrimitiveType;

import java.util.function.Function;


public class EqualUint8<A> extends EqualityFunction<A, Byte> {
    public EqualUint8() {
        super(PrimitiveType.uint8());
    }

    public static Function<Byte, Boolean> apply(Byte second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Byte first, Byte second) {
        return 0 == first.compareTo(second);
    }
}

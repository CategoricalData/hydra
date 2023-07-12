package hydra.lib.literals;

import hydra.compute.Flow;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.graph.Graph;
import java.util.function.Function;

import static hydra.dsl.Types.*;


public class EqualUint8<A> extends EqualityFunction<A, Byte> {
    @Override
    protected String typeName() {
        return "Uint8";
    }

    @Override
    protected Type<A> literalType() {
        return uint8();
    }

    @Override
    protected Flow<Graph<A>, Byte> expect(Term<A> term) {
        return Expect.uint8(term);
    }

    @Override
    protected boolean checkEqual(Byte first, Byte second) {
        return apply(first, second);
    }

    public static Function<Byte, Boolean> apply(Byte second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Byte first, Byte second) {
        return 0 == first.compareTo(second);
    }
}

package hydra.lib.literals;

import hydra.compute.Flow;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.graph.Graph;
import java.util.function.Function;

import static hydra.dsl.Types.*;


public class EqualInt8<A> extends EqualityFunction<A, Short> {
    @Override
    protected String typeName() {
        return "Int8";
    }

    @Override
    protected Type<A> literalType() {
        return int8();
    }

    @Override
    protected Flow<Graph<A>, Short> expect(Term<A> term) {
        return Expect.int8(term);
    }

    @Override
    protected boolean checkEqual(Short first, Short second) {
        return apply(first, second);
    }

    public static Function<Short, Boolean> apply(Short second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Short first, Short second) {
        return 0 == first.compareTo(second);
    }
}

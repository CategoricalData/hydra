package hydra.lib.literals;

import hydra.compute.Flow;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.graph.Graph;
import java.util.function.Function;

import static hydra.dsl.Types.*;


public class EqualInt32<A> extends EqualityFunction<A, Integer> {
    @Override
    protected String typeName() {
        return "Int32";
    }

    @Override
    protected Type<A> literalType() {
        return int32();
    }

    @Override
    protected Flow<Graph<A>, Integer> expect(Term<A> term) {
        return Expect.int32(term);
    }

    @Override
    protected boolean checkEqual(Integer first, Integer second) {
        return apply(first, second);
    }

    public static Function<Integer, Boolean> apply(Integer second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Integer first, Integer second) {
        return 0 == first.compareTo(second);
    }
}

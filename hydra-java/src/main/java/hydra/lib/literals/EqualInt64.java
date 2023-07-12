package hydra.lib.literals;

import hydra.compute.Flow;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.graph.Graph;
import java.util.function.Function;

import static hydra.dsl.Types.*;


public class EqualInt64<A> extends EqualityFunction<A, Long> {
    @Override
    protected String typeName() {
        return "Int64";
    }

    @Override
    protected Type<A> literalType() {
        return int64();
    }

    @Override
    protected Flow<Graph<A>, Long> expect(Term<A> term) {
        return Expect.int64(term);
    }

    @Override
    protected boolean checkEqual(Long first, Long second) {
        return apply(first, second);
    }

    public static Function<Long, Boolean> apply(Long second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Long first, Long second) {
        return 0 == first.compareTo(second);
    }
}

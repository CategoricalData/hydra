package hydra.lib.literals;

import hydra.compute.Flow;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.graph.Graph;
import java.math.BigInteger;
import java.util.function.Function;

import static hydra.dsl.Types.*;


public class EqualBigint<A> extends EqualityFunction<A, BigInteger> {
    @Override
    protected String typeName() {
        return "Bigint";
    }

    @Override
    protected Type<A> datatype() {
        return bigint();
    }

    @Override
    protected Flow<Graph<A>, BigInteger> expect(Term<A> term) {
        return Expect.bigint(term);
    }

    @Override
    protected boolean checkEqual(BigInteger first, BigInteger second) {
        return apply(first, second);
    }

    public static Function<BigInteger, Boolean> apply(BigInteger second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(BigInteger first, BigInteger second) {
        return 0 == first.compareTo(second);
    }
}

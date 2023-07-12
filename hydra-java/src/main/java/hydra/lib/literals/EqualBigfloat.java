package hydra.lib.literals;

import hydra.compute.Flow;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.graph.Graph;
import java.util.function.Function;

import static hydra.dsl.Types.*;


public class EqualBigfloat<A> extends EqualityFunction<A, Double> {
    @Override
    protected String typeName() {
        return "Bigfloat";
    }

    @Override
    protected Type<A> literalType() {
        return bigfloat();
    }

    @Override
    protected Flow<Graph<A>, Double> expect(Term<A> term) {
        return Expect.bigfloat(term);
    }

    @Override
    protected boolean checkEqual(Double first, Double second) {
        return apply(first, second);
    }

    public static Function<Double, Boolean> apply(Double second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Double first, Double second) {
        return 0 == first.compareTo(second);
    }
}

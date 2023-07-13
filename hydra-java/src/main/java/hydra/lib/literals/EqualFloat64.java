package hydra.lib.literals;

import hydra.compute.Flow;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.graph.Graph;
import java.util.function.Function;

import static hydra.dsl.Types.*;


public class EqualFloat64<A> extends EqualityFunction<A, Double> {
    @Override
    protected String typeName() {
        return "Float64";
    }

    @Override
    protected Type<A> datatype() {
        return float64();
    }

    @Override
    protected Flow<Graph<A>, Double> expect(Term<A> term) {
        return Expect.float64(term);
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

package hydra.lib.literals;

import hydra.compute.Flow;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.graph.Graph;
import java.util.function.Function;

import static hydra.dsl.Types.*;


public class EqualFloat32<A> extends EqualityFunction<A, Float> {
    @Override
    protected String typeName() {
        return "Float32";
    }

    @Override
    protected Type<A> literalType() {
        return float32();
    }

    @Override
    protected Flow<Graph<A>, Float> expect(Term<A> term) {
        return Expect.float32(term);
    }

    @Override
    protected boolean checkEqual(Float first, Float second) {
        return apply(first, second);
    }

    public static Function<Float, Boolean> apply(Float second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Float first, Float second) {
        return 0 == first.compareTo(second);
    }
}

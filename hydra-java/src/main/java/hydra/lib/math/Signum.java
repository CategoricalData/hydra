package hydra.lib.math;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.scheme;


/**
 * Returns the sign of a number.
 */
public class Signum extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.math.signum");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(int32(), int32()));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.int32(args.get(0)),
            (arg0) -> Terms.int32(apply(arg0)));
    }

    /**
     * Returns -1, 0, or 1 based on the sign.
     * @param num the number
     * @return the sign
     */
        public static Integer apply(Integer num) {
        return Integer.signum(num);
    }
}

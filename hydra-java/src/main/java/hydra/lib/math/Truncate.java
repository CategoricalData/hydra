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

import java.math.BigInteger;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.bigint;
import static hydra.dsl.Types.float64;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;


/**
 * Truncates the decimal part.
 */
public class Truncate extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.math.truncate");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(float64(), bigint()));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.float64(args.get(0)),
            (arg0) -> Terms.bigint(apply(arg0)));
    }

    /**
     * Removes the decimal part.
     * @param x the value
     * @return the truncated value
     */
        public static BigInteger apply(Double x) {
        return BigInteger.valueOf(x.longValue());
    }
}

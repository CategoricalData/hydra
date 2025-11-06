package hydra.lib.literals;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Flows;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.float64;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;


/**
 * Primitive function: ShowFloat64.
 */
public class ShowFloat64 extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.literals.showFloat64");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(float64(), string()));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.float64(args.get(0)),
            (Function<Double, Term>) d -> Terms.string(apply(d)));
    }

    /**
     * Applies the ShowFloat64 operation.
     * @param value the value
     * @return the result
     */
        public static String apply(Double value) {
        return Double.toString(value);
    }
}

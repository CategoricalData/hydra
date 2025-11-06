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

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int64;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;


/**
 * Primitive function: ShowInt64.
 */
public class ShowInt64 extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.literals.showInt64");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(int64(), string()));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.int64(args.get(0)),
            (Function<Long, Term>) l -> Terms.string(apply(l)));
    }

    /**
     * Applies the ShowInt64 operation.
     * @param value the value
     * @return the result
     */
        public static String apply(Long value) {
        return Long.toString(value);
    }
}

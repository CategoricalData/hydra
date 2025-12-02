package hydra.lib.pairs;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Tuple;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.pair;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.variable;


/**
 * Extracts the second element of a pair.
 */
public class Second extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.pairs.second");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme("a", "b", function(pair(variable("a"), variable("b")), "b"));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.pair(Flows::pure, Flows::pure, args.get(0)),
                (Function<Tuple.Tuple2<Term, Term>, Term>) pair -> apply(pair));
    }

    /**
     * Apply the function to extract the second element of a pair.
     * @param <A> the first element type
     * @param <B> the second element type
     * @param pair the pair
     * @return the second element
     */
    public static <A, B> B apply(Tuple.Tuple2<A, B> pair) {
        return pair.object2;
    }
}

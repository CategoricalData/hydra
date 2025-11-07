package hydra.lib.sets;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.set;


/**
 * Creates a map from a list of pairs.
 */
public class FromList extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.sets.fromList");
    }

    @Override
    public TypeScheme type() {
        return scheme("x", function(list("x"), set("x")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.list(Flows::pure, args.get(0)), arg -> Terms.set(apply(arg)));
    }

    /**
     * Creates a map from key-value pairs.
     * @param arg the pairs
     * @return the map
     */
        public static <X> Set<X> apply(List<X> arg) {
        return new HashSet<>(arg);
    }
}

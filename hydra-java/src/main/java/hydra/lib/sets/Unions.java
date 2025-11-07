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
 * Computes the union of multiple sets.
 */
public class Unions extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.sets.unions");
    }

    @Override
    public TypeScheme type() {
        return scheme("x", function(list(set("x")), set("x")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(
                Expect.list(t -> Expect.set(Flows::pure, t), args.get(0)),
                sets -> Terms.set(apply(sets)));
    }

    /**
     * Combines multiple sets.
     * @param sets the sets
     * @return the union
     */
        public static <X> Set<X> apply(List<Set<X>> sets) {
        Set<X> result = new HashSet<>();
        for (Set<X> s : sets) {
            result.addAll(s);
        }
        return result;
    }
}

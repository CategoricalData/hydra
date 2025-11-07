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

import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.set;


/**
 * Checks if a list is empty.
 */
public class Null extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.sets.null");
    }

    @Override
    public TypeScheme type() {
        return scheme("x", function(set("x"), boolean_()));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.set(Flows::pure, args.get(0)), arg -> Terms.boolean_(apply(arg)));
    }

    /**
     * Checks if the list is empty.
     * @param arg the list
     * @return true if empty, false otherwise
     */
        public static <X> Boolean apply(Set<X> arg) {
        return arg.isEmpty();
    }
}

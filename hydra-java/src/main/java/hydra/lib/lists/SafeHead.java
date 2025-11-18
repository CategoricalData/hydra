package hydra.lib.lists;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Flows.map;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;


/**
 * Safely returns the first element.
 */
public class SafeHead extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.safeHead");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list("a"), optional("a")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> map(Expect.list(Flows::pure, args.get(0)), l -> Terms.optional(apply(l)));
    }

    /**
     * Apply the function to it single argument.
     * @param <X> the element type
     * @param list the list to get the head from
     * @return an optional containing the first element, or empty if the list is empty
     */
    public static <X> Maybe<X> apply(List<X> list) {
        if (list.isEmpty()) {
            return Maybe.nothing();
        } else {
            return Maybe.just(list.get(0));
        }
    }
}

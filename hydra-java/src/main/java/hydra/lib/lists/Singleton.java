package hydra.lib.lists;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;


/**
 * Creates a singleton list.
 */
public class Singleton extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.singleton");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function("a", list("a")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> pure(Terms.list(Collections.singletonList(args.get(0))));
    }

    /**
     * Creates a list with one element.
     * @param elem the value
     * @return the singleton list
     */
        public static <X> List<X> apply(X elem) {
        return Collections.singletonList(elem);
    }
}

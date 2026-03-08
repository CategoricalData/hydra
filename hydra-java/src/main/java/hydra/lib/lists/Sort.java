package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.schemeOrd;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.OtherError;
import hydra.util.Either;


/**
 * Sorts a list.
 */
public class Sort extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.sort");
    }

    @Override
    public TypeScheme type() {
        return schemeOrd("a", function(list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<List<Term>, Term>) lst -> {
                List<Term> sorted = new ArrayList<>(lst);
                sorted.sort(hydra.lib.equality.Compare::compareTerms);
                return Terms.list(sorted);
            }, hydra.extract.core.Core.list(cx, graph, args.get(0)));
    }

    /**
     * Sorts the list in ascending order.
     * @param <X> the element type (expected to be Comparable at runtime)
     * @param lst the list to sort
     * @return the sorted list
     */
    @SuppressWarnings("unchecked")
    public static <X> List<X> apply(List<X> lst) {
        List<X> result = new ArrayList<>(lst);
        result.sort((a, b) -> ((Comparable) a).compareTo(b));
        return result;
    }
}

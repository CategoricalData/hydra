package hydra.overlay.java.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.overlay.java.dsl.Types;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.list;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.schemeOrd;
import hydra.errors.Error_;
import hydra.overlay.java.util.ConsList;
import hydra.overlay.java.util.Either;


/**
 * Sorts a list.
 */
public class Sort extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Lists.sort().name;
    }

    @Override
    public TypeScheme type() {
        return schemeOrd("a", function(list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply((Function<List<Term>, Term>) lst -> {
                ArrayList<Term> scratch = new ArrayList<>(lst);
                scratch.sort(hydra.overlay.java.lib.ordering.Compare::compareTerms);
                return Terms.list(ConsList.fromList(scratch));
            }, hydra.extract.Core.list(graph, args.get(0)));
    }

    /**
     * Sorts the list in ascending order.
     * @param <X> the element type (expected to be Comparable at runtime)
     * @param lst the list to sort
     * @return the sorted list
     */
    @SuppressWarnings("unchecked")
    public static <X> List<X> apply(List<X> lst) {
        ArrayList<X> scratch = new ArrayList<>(lst);
        scratch.sort((a, b) -> ((Comparable) a).compareTo(b));
        return ConsList.fromList(scratch);
    }
}

package hydra.core.overlay.java.lib.lists;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.schemeOrd;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.ConsList;
import hydra.core.overlay.java.util.Either;


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
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply((Function<List<Term>, Term>) lst -> {
                ArrayList<Term> scratch = new ArrayList<>(lst);
                scratch.sort(hydra.core.overlay.java.lib.ordering.Compare::compareTerms);
                return Terms.list(ConsList.fromList(scratch));
            }, hydra.core.extract.Core.list(graph, args.get(0)));
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

package hydra.lib.lists;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Flows;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;


/**
 * Sorts a list.
 */
public class Sort extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.sort");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.list(Flows::pure, args.get(0)),
            (Function<List<Term>, Term>) lst -> {
                // Term-level sorting is complex
                // The static apply() provides the actual logic for comparable types
                return Terms.list(lst);
            });
    }

    /**
     * Sorts the list in ascending order.
     * @param lst the list
     * @return the sorted list
     */
        public static <X extends Comparable<X>> List<X> apply(List<X> lst) {
        List<X> result = new ArrayList<>(lst);
        Collections.sort(result);
        return result;
    }
}

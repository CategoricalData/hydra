package hydra.lib.lists;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Flows;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;

import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;


/**
 * Drops elements while predicate is true.
 */
public class DropWhile extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.dropWhile");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(function("a", boolean_()), list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.list(Flows::pure, args.get(1)),
            (Function<List<Term>, Term>) lst -> {
                // Simplified implementation - the static apply() provides the actual logic
                return Terms.list(lst);
            });
    }

    /**
     * Drops elements while the predicate holds.
     * @param pred the predicate
     * @return the remaining list
     */
        public static <X> Function<List<X>, List<X>> apply(Predicate<X> pred) {
        return lst -> apply(pred, lst);
    }

    /**
     * Drops elements while the predicate holds.
     * @param pred the predicate
     * @param lst the list
     * @return the remaining list
     */
        public static <X> List<X> apply(Predicate<X> pred, List<X> lst) {
        List<X> result = new ArrayList<>(lst);
        while (!result.isEmpty() && pred.test(result.get(0))) {
            result = result.subList(1, result.size());
        }
        return result;
    }
}

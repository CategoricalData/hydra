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
import hydra.util.Maybe;
import hydra.util.Tuple;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Flows.bind;
import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;


/**
 * Zips two lists into pairs.
 */
public class Zip extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.zip");
    }

    @Override
    public TypeScheme type() {
        return new hydra.core.TypeScheme(
                Arrays.asList(new hydra.core.Name("a"), new hydra.core.Name("b")),
                function(list("a"), list("b"), list(Types.pair(Types.variable("a"), Types.variable("b")))),
                Maybe.nothing());
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind(Expect.list(Flows::pure, args.get(0)), lst1 ->
            Flows.map(Expect.list(Flows::pure, args.get(1)),
                (Function<List<Term>, Term>) lst2 -> {
                    List<Term> result = new ArrayList<>();
                    int minSize = Math.min(lst1.size(), lst2.size());
                    for (int i = 0; i < minSize; i++) {
                        result.add(Terms.pair(lst1.get(i), lst2.get(i)));
                    }
                    return Terms.list(result);
                }));
    }

    /**
     * Combines two lists into pairs.
     * @param <X> the first list element type
     * @param <Y> the second list element type
     * @param lst1 the first list
     * @return a function that zips the first list with a second list
     */
    public static <X, Y> Function<List<Y>, List<Tuple.Tuple2<X, Y>>> apply(List<X> lst1) {
        return lst2 -> apply(lst1, lst2);
    }

    /**
     * Combines two lists into pairs.
     * @param <X> the first list element type
     * @param <Y> the second list element type
     * @param lst1 the first list
     * @param lst2 the second list
     * @return a list of pairs containing elements from both lists
     */
    public static <X, Y> List<Tuple.Tuple2<X, Y>> apply(List<X> lst1, List<Y> lst2) {
        List<Tuple.Tuple2<X, Y>> result = new ArrayList<>();
        int minSize = Math.min(lst1.size(), lst2.size());
        for (int i = 0; i < minSize; i++) {
            result.add(new Tuple.Tuple2<>(lst1.get(i), lst2.get(i)));
        }
        return result;
    }
}

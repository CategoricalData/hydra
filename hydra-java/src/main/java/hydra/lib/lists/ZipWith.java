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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

import static hydra.dsl.Flows.bind;
import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;


/**
 * Zips two lists with a function.
 */
public class ZipWith extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.zipWith");
    }

    @Override
    public TypeScheme type() {
        return new hydra.core.TypeScheme(
                Arrays.asList(new hydra.core.Name("a"), new hydra.core.Name("b"), new hydra.core.Name("c")),
                function(function(Types.var("a"), Types.var("b"), Types.var("c")), list("a"), list("b"), list("c")),
                Maybe.nothing());
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind(Expect.list(Flows::pure, args.get(1)), lst1 ->
            bind(Expect.list(Flows::pure, args.get(2)), lst2 -> {
                Term f = args.get(0);
                int minSize = Math.min(lst1.size(), lst2.size());
                Flow<Graph, List<Term>> resultFlow = pure(new ArrayList<>());
                for (int i = 0; i < minSize; i++) {
                    Term application = Terms.apply(Terms.apply(f, lst1.get(i)), lst2.get(i));
                    resultFlow = bind(resultFlow, acc ->
                        Flows.map(hydra.reduction.Reduction.reduceTerm(true, application), result -> {
                            acc.add(result);
                            return acc;
                        }));
                }
                return Flows.map(resultFlow, Terms::list);
            }));
    }

    /**
     * Combines two lists using a function.
     * @param <X> the first list element type
     * @param <Y> the second list element type
     * @param <Z> the result element type
     * @param f the combining function
     * @return a curried function that takes two lists and combines them
     */
    public static <X, Y, Z> Function<List<X>, Function<List<Y>, List<Z>>> apply(BiFunction<X, Y, Z> f) {
        return lst1 -> lst2 -> apply(f, lst1, lst2);
    }

    /**
     * Combines two lists using a function.
     * @param <X> the first list element type
     * @param <Y> the second list element type
     * @param <Z> the result element type
     * @param f the combining function
     * @param lst1 the first list
     * @param lst2 the second list
     * @return a list of elements created by applying the function to pairs
     */
    public static <X, Y, Z> List<Z> apply(BiFunction<X, Y, Z> f, List<X> lst1, List<Y> lst2) {
        List<Z> result = new ArrayList<>();
        int minSize = Math.min(lst1.size(), lst2.size());
        for (int i = 0; i < minSize; i++) {
            result.add(f.apply(lst1.get(i), lst2.get(i)));
        }
        return result;
    }

    /**
     * Combines two lists using a curried function.
     * @param <X> the first list element type
     * @param <Y> the second list element type
     * @param <Z> the result element type
     * @param f the combining function (curried)
     * @param lst1 the first list
     * @param lst2 the second list
     * @return a list of elements created by applying the function to pairs
     */
    public static <X, Y, Z> List<Z> apply(Function<X, Function<Y, Z>> f, List<X> lst1, List<Y> lst2) {
        return apply((x, y) -> f.apply(x).apply(y), lst1, lst2);
    }
}

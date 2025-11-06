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

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.variable;


/**
 * Left fold with flow function.
 */
public class Foldl extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.foldl");
    }

    @Override
    public TypeScheme type() {
        return scheme("a","b",
                function(function("b", "a", "b"), variable("b"), list("a"), variable("b")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            Term mapping = args.get(0);
            Term init = args.get(1);
            return Flows.map(Expect.list(Flows::pure, args.get(2)), xs -> {
                Term cur = init;
                for (Term x : xs) {
                    cur = Terms.apply(mapping, cur, x);
                }
                return cur;
            });
        };
    }

    /**
     * Performs a left fold in flow context.
     * @param mapping the function
     * @return the flow of result
     */
        public static <X, Y> Function<Y, Function<List<X>, Y>> apply(Function<Y, Function<X, Y>> mapping) {
        return y -> xs -> apply(mapping, y, xs);
    }

    /**
     * Performs a left fold in flow context.
     * @param mapping the function
     * @param init the initial
     * @return the flow of result
     */
        public static <X, Y> Function<List<X>, Y> apply(Function<Y, Function<X, Y>> mapping, Y init) {
        return xs -> apply(mapping, init, xs);
    }

    /**
     * Performs a left fold in flow context.
     * @param mapping the function
     * @param init the initial
     * @param xs the list
     * @return the flow of result
     */
        public static <X, Y> Y apply(Function<Y, Function<X, Y>> mapping, Y init, List<X> xs) {
        Y cur = init;
        for (X x : xs) {
            cur = mapping.apply(cur).apply(x);
        }
        return cur;
    }
}

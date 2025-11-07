package hydra.lib.flows;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

import static hydra.dsl.Flows.bind;
import static hydra.dsl.Flows.pure;


/**
 * Left fold with flow function.
 */
public class Foldl extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.flows.foldl");
    }

    @Override
    public TypeScheme type() {
        return Types.scheme("s", "a", "b",
                Types.function(
                        Types.function(Types.var("a"), Types.var("b"), Types.flow("s", "a")),
                        Types.var("a"),
                        Types.list("b"),
                        Types.flow("s", "a")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind(Expect.list(Flows::pure, args.get(2)), list -> {
            Flow<Graph, Term> result = pure(args.get(1));
            for (Term item : list) {
                result = bind(result, acc ->
                    pure(Terms.apply(Terms.apply(args.get(0), acc), item)));
            }
            return result;
        });
    }

    /**
     * Performs a left fold in flow context.
     * @return the flow of result
     */
        public static <S, A, B> Function<A, Function<List<B>, Flow<S, A>>> apply(
            BiFunction<A, B, Flow<S, A>> f) {
        return initial -> list -> apply(f, initial, list);
    }

    /**
     * Performs a left fold in flow context.
     * @param f the function
     * @param initial the initial
     * @param list the list
     * @return the flow of result
     */
        public static <S, A, B> Flow<S, A> apply(BiFunction<A, B, Flow<S, A>> f, A initial, List<B> list) {
        Flow<S, A> result = pure(initial);
        for (B item : list) {
            result = bind(result, acc -> f.apply(acc, item));
        }
        return result;
    }
}

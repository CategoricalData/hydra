package hydra.lib.pairs;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Flows;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Tuple;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.pair;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.variable;

/**
 * Map over both elements of a pair.
 */
public class Bimap extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.pairs.bimap");

    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return NAME;
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme("a", "b", "c", "d",
            function(
                function(variable("a"), variable("c")),
                function(variable("b"), variable("d")),
                pair(variable("a"), variable("b")),
                pair(variable("c"), variable("d"))));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            Term forFirst = args.get(0);
            Term forSecond = args.get(1);
            return Flows.map(Expect.pair(Flows::pure, Flows::pure, args.get(2)),
                (Function<Tuple.Tuple2<Term, Term>, Term>) p ->
                    Terms.pair(Terms.apply(forFirst, p.object1), Terms.apply(forSecond, p.object2)));
        };
    }

    /**
     * Map over both elements of a pair.
     *
     * @param <A> the original first type
     * @param <B> the original second type
     * @param <C> the new first type
     * @param <D> the new second type
     * @param firstFn the function to apply to the first element
     * @param secondFn the function to apply to the second element
     * @param pair the pair to map over
     * @return the mapped pair
     */
    public static <A, B, C, D> Tuple.Tuple2<C, D> apply(
            Function<A, C> firstFn,
            Function<B, D> secondFn,
            Tuple.Tuple2<A, B> pair) {
        return new Tuple.Tuple2<>(firstFn.apply(pair.object1), secondFn.apply(pair.object2));
    }
}

package hydra.lib.flows;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Opt;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Flows.bind;
import static hydra.dsl.Flows.pure;


/**
 * Maps a flow function over Maybe.
 */
public class MapMaybe extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.flows.mapMaybe");
    }

    @Override
    public TypeScheme type() {
        return Types.scheme("s", "x", "y",
                Types.function(
                        Types.function("x", Types.flow("s", "y")),
                        Types.optional("x"),
                        Types.flow("s", Types.optional("y"))));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind(Expect.optional(Flows::pure, args.get(1)), opt ->
            opt.isPresent()
                ? bind(pure(Terms.apply(args.get(0), opt.get())), result ->
                    pure(Terms.optional(Opt.of(result))))
                : pure(Terms.optional(Opt.empty())));
    }

    /**
     * Applies a flow function to Maybe value.
     * @param <S> the state type
     * @param <X> the input type
     * @param <Y> the output type
     * @param f the function
     * @return the flow of Maybe
     */
    public static <S, X, Y> Function<Opt<X>, Flow<S, Opt<Y>>> apply(Function<X, Flow<S, Y>> f) {
        return opt -> apply(f, opt);
    }

    /**
     * Applies a flow function to Maybe value.
     * @param <S> the state type
     * @param <X> the input type
     * @param <Y> the output type
     * @param f the function
     * @param opt the maybeValue
     * @return the flow of Maybe
     */
    public static <S, X, Y> Flow<S, Opt<Y>> apply(Function<X, Flow<S, Y>> f, Opt<X> opt) {
        return Flows.mapM(opt, f);
    }
}

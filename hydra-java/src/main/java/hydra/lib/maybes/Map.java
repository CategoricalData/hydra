package hydra.lib.maybes;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Opt;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Flows.bind;
import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;


/**
 * Maps a function over a flow.
 */
public class Map extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.maybes.map"
     */
    public Name name() {
        return new Name("hydra.lib.maybes.map");
    }

    /**
     * Returns the type scheme of this primitive function.
     * @return the type scheme for mapping a function over an optional value
     */
    @Override
    public TypeScheme type() {
        return scheme("a", "b",
                function(function("a", "b"), optional("a"), optional("b")));
    }

    /**
     * Returns the implementation of this primitive function.
     * @return a function that maps a function over an optional value
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind(Expect.optional(instance -> pure(Terms.apply(args.get(0), instance)), args.get(1)),
            opt -> pure(Terms.optional(opt)));
    }

    /**
     * Maps a function over an optional value. Curried version.
     * @param <X> the input type
     * @param <Y> the output type
     * @param f the function to map
     * @return a function that takes an optional and returns a mapped optional
     */
    public static <X, Y> Function<Opt<X>, Opt<Y>> apply(Function<X, Y> f) {
        return (optionalArg) -> apply(f, optionalArg);
    }

    /**
     * Maps a function over an optional value.
     * @param <X> the input type
     * @param <Y> the output type
     * @param f the function to map
     * @param optionalArg the optional value to map over
     * @return an optional containing the mapped value if present, otherwise empty
     */
    public static <X, Y> Opt<Y> apply(Function<X, Y> f, Opt<X> optionalArg) {
        if (!optionalArg.isPresent()) {
            return Opt.empty();
        }

        X arg = optionalArg.get();

        return Opt.of(f.apply(arg));
    }
}

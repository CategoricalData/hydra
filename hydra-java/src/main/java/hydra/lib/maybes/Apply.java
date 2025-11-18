package hydra.lib.maybes;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

import static hydra.dsl.Flows.map2;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;


/**
 * Applies a function in a flow context.
 */
public class Apply extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.maybes.apply"
     */
    public Name name() {
        return new Name("hydra.lib.maybes.apply");
    }

    /**
     * Returns the type scheme of this primitive function.
     * @return the type scheme for applying an optional function to an optional value
     */
    @Override
    public TypeScheme type() {
        return scheme("a", "b",
            function(optional(function("a", "b")), optional("a"), optional("b")));
    }

    /**
     * Returns the implementation of this primitive function.
     * @return a function that applies an optional function to an optional argument
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> map2(Expect.optional(Flows::pure, args.get(0)), Expect.optional(Flows::pure, args.get(1)),
            (BiFunction<Maybe<Term>, Maybe<Term>, Term>) (optionalF, optionalArg) ->
                (optionalF.isJust() && optionalArg.isJust())
                    ? Terms.optional(Maybe.just(Terms.apply(optionalF.fromJust(), optionalArg.fromJust())))
                    : Terms.optional(Maybe.nothing()));
    }

    /**
     * Applies a function within a flow context. Curried version.
     * @param <X> the input type
     * @param <Y> the output type
     * @param optionalF the optional function to apply
     * @return a function that takes an optional argument and returns an optional result
     */
    public static <X, Y> Function<Maybe<X>, Maybe<Y>> apply(Maybe<Function<X, Y>> optionalF) {
        return (optionalArg) -> apply(optionalF, optionalArg);
    }

    /**
     * Applies an optional function to an optional argument.
     * @param <X> the input type
     * @param <Y> the output type
     * @param optionalF the optional function to apply
     * @param optionalArg the optional argument
     * @return the optional result of applying the function to the argument, or empty if either is empty
     */
    public static <X, Y> Maybe<Y> apply(Maybe<Function<X, Y>> optionalF, Maybe<X> optionalArg) {
        if (!optionalF.isJust() || !optionalArg.isJust()) {
            return Maybe.nothing();
        }

        Function<X, Y> f = optionalF.fromJust();
        X arg = optionalArg.fromJust();

        return Maybe.just(f.apply(arg));
    }
}

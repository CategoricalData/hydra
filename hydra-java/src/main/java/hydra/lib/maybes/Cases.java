package hydra.lib.maybes;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.OtherError;
import hydra.util.Either;


/**
 * Pattern matches on Maybe.
 */
public class Cases extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.maybes.cases"
     */
    public Name name() {
        return new Name("hydra.lib.maybes.cases");
    }

    /**
     * Returns the type scheme of this primitive function.
     * @return the type scheme for pattern matching on optional values
     */
    @Override
    public TypeScheme type() {
        return scheme("a", "b", function(optional("a"), function("b", function(function("a", "b"), "b"))));
    }

    /**
     * Returns the implementation of this primitive function.
     * @return a function that performs pattern matching on optional values
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.maybeTerm(cx, t -> Either.right(t), graph, args.get(0)), opt ->
            opt.isJust()
                ? Either.right(Terms.apply(args.get(2), opt.fromJust()))
                : Either.right(args.get(1)));
    }

    /**
     * Handles Just and Nothing cases. Curried version.
     * @param <X> the optional value type
     * @param <Y> the result type
     * @param opt the optional value to match on
     * @return a function that takes a Nothing case handler and returns a function that takes a Just case handler
     */
    public static <X, Y> Function<Y, Function<Function<X, Y>, Y>> apply(Maybe<X> opt) {
        return (nothingCase) -> (justCase) -> apply(opt, nothingCase, justCase);
    }

    /**
     * Handles Just and Nothing cases with explicit handlers.
     * @param <X> the optional value type
     * @param <Y> the result type
     * @param opt the optional value to match on
     * @param nothingCase the value to return if the optional is empty
     * @param justCase the function to apply to the value if the optional is present
     * @return the result of applying the appropriate case handler
     */
    public static <X, Y> Y apply(Maybe<X> opt, Y nothingCase, Function<X, Y> justCase) {
        return opt.isJust() ? justCase.apply(opt.fromJust()) : nothingCase;
    }
}

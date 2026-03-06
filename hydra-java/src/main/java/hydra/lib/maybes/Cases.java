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
import java.util.function.Supplier;

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
     * @deprecated Use {@link #applyLazy(Maybe, Supplier, Function)} instead. Eager evaluation of the nothing case wastes memory.
     */
    @Deprecated
    public static <X, Y> Function<Y, Function<Function<X, Y>, Y>> apply(Maybe<X> opt) {
        return (nothingCase) -> (justCase) -> apply(opt, nothingCase, justCase);
    }

    /**
     * @deprecated Use {@link #applyLazy(Maybe, Supplier, Function)} instead. Eager evaluation of the nothing case wastes memory.
     */
    @Deprecated
    public static <X, Y> Y apply(Maybe<X> opt, Y nothingCase, Function<X, Y> justCase) {
        return opt.isJust() ? justCase.apply(opt.fromJust()) : nothingCase;
    }

    /**
     * Lazily pattern matches on an optional value.
     * The nothing case is only evaluated if the optional is empty.
     */
    public static <X, Y> Function<Supplier<Y>, Function<Function<X, Y>, Y>> applyLazy(Maybe<X> opt) {
        return (nothingCase) -> (justCase) -> applyLazy(opt, nothingCase, justCase);
    }

    /**
     * Lazily pattern matches on an optional value.
     * The nothing case is only evaluated if the optional is empty.
     */
    public static <X, Y> Y applyLazy(Maybe<X> opt, Supplier<Y> nothingCase, Function<X, Y> justCase) {
        return opt.isJust() ? justCase.apply(opt.fromJust()) : nothingCase.get();
    }
}

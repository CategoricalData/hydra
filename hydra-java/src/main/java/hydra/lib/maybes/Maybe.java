package hydra.lib.maybes;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Pattern matches with a default.
 */
public class Maybe extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.maybes.maybe"
     */
    public Name name() {
        return new Name("hydra.lib.maybes.maybe");
    }

    /**
     * Returns the type scheme of this primitive function.
     * @return the type scheme for pattern matching on optional values with a default
     */
    @Override
    public TypeScheme type() {
        // Variables are listed in order of first appearance in type: b -> (a -> b) -> optional<a> -> b
        // b appears first, then a
        return scheme("b", "a", function("b", function(function("a", "b"), function(optional("a"), "b"))));
    }

    /**
     * Returns the implementation of this primitive function.
     * @return a function that performs pattern matching on optional values with a default
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.Core.maybeTerm(t -> Either.right(t), graph, args.get(2)), opt ->
            opt.isJust()
                ? Either.right(Terms.apply(args.get(1), opt.fromJust()))
                : Either.right(args.get(0)));
    }

    /**
     * @deprecated Use {@link #applyLazy(Supplier, Function, hydra.util.Maybe)} instead. Eager evaluation of the nothing case wastes memory.
     */
    @Deprecated
    public static <X, Y> Function<Function<X, Y>, Function<hydra.util.Maybe<X>, Y>> apply(Y nothingCase) {
        return (justCase) -> (opt) -> apply(nothingCase, justCase, opt);
    }

    /**
     * @deprecated Use {@link #applyLazy(Supplier, Function, hydra.util.Maybe)} instead. Eager evaluation of the nothing case wastes memory.
     */
    @Deprecated
    public static <X, Y> Y apply(Y nothingCase, Function<X, Y> justCase, hydra.util.Maybe<X> opt) {
        return opt.isJust() ? justCase.apply(opt.fromJust()) : nothingCase;
    }

    /**
     * Lazily pattern matches on an optional value with a default.
     * The nothing case is only evaluated if the optional is empty,
     * avoiding construction of expensive error messages on the success path.
     */
    public static <X, Y> Function<Function<X, Y>, Function<hydra.util.Maybe<X>, Y>> applyLazy(Supplier<Y> nothingCase) {
        return (justCase) -> (opt) -> applyLazy(nothingCase, justCase, opt);
    }

    /**
     * Lazily pattern matches on an optional value with a default.
     * The nothing case is only evaluated if the optional is empty,
     * avoiding construction of expensive error messages on the success path.
     */
    public static <X, Y> Y applyLazy(Supplier<Y> nothingCase, Function<X, Y> justCase, hydra.util.Maybe<X> opt) {
        return opt.isJust() ? justCase.apply(opt.fromJust()) : nothingCase.get();
    }
}

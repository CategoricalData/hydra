package hydra.lib.optionals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Optional;

import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import hydra.typing.InferenceContext;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Pattern matches on Optional.
 */
public class Cases extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.optionals.cases"
     */
    public Name name() {
        return new Name("hydra.lib.optionals.cases");
    }

    @Override
    protected List<Integer> lazyParams() {
        return List.of(1);
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
    protected Function<List<Term>, Function<InferenceContext, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.Core.optionalTerm(t -> Either.right(t), graph, args.get(0)), opt ->
            opt.isGiven()
                ? Either.right(Terms.apply(args.get(2), opt.fromGiven()))
                : Either.right(args.get(1)));
    }

    /**
     * @deprecated Use {@link #applyLazy(Optional, Supplier, Function)} instead. Eager evaluation of the nothing case wastes memory.
     */
    @Deprecated
    public static <X, Y> Function<Y, Function<Function<X, Y>, Y>> apply(Optional<X> opt) {
        return (nothingCase) -> (justCase) -> apply(opt, nothingCase, justCase);
    }

    /**
     * @deprecated Use {@link #applyLazy(Optional, Supplier, Function)} instead. Eager evaluation of the nothing case wastes memory.
     */
    @Deprecated
    public static <X, Y> Y apply(Optional<X> opt, Y nothingCase, Function<X, Y> justCase) {
        return opt.isGiven() ? justCase.apply(opt.fromGiven()) : nothingCase;
    }

    /**
     * Lazily pattern matches on an optional value.
     * The nothing case is only evaluated if the optional is empty.
     */
    public static <X, Y> Function<Supplier<Y>, Function<Function<X, Y>, Y>> applyLazy(Optional<X> opt) {
        return (nothingCase) -> (justCase) -> applyLazy(opt, nothingCase, justCase);
    }

    /**
     * Lazily pattern matches on an optional value.
     * The nothing case is only evaluated if the optional is empty.
     */
    public static <X, Y> Y applyLazy(Optional<X> opt, Supplier<Y> nothingCase, Function<X, Y> justCase) {
        return opt.isGiven() ? justCase.apply(opt.fromGiven()) : nothingCase.get();
    }
}

package hydra.core.overlay.java.lib.optionals;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;
import hydra.core.overlay.java.util.Optional;

import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.optional;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Extracts the value from an optional, or returns a default if Nothing.
 */
public class WithDefault extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.core.lib.optionals.withDefault"
     */
    public Name name() {
        return hydra.lib.Optionals.withDefault().name;
    }

    @Override
    protected List<Integer> lazyParams() {
        return List.of(0);
    }

    /**
     * Returns the type scheme of this primitive function.
     * @return the type scheme for extracting a value from an optional with a default
     */
    @Override
    public TypeScheme type() {
        return scheme("a", function("a", function(optional("a"), "a")));
    }

    /**
     * Returns the implementation of this primitive function.
     * @return a function that extracts the value from an optional or returns a default
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Bind.apply(Either.right(args.get(0)), defaultTerm ->
            hydra.core.overlay.java.lib.eithers.Bind.apply(hydra.core.extract.Core.optionalTerm(t -> Either.right(t), graph, args.get(1)), opt ->
                Either.right(opt.isGiven() ? opt.fromGiven() : defaultTerm)));
    }

    /**
     * @deprecated Use {@link #applyLazy(Supplier, Optional)} instead. Eager evaluation of the default wastes memory.
     */
    @Deprecated
    public static <X> Function<Optional<X>, X> apply(X defaultValue) {
        return (opt) -> apply(defaultValue, opt);
    }

    /**
     * @deprecated Use {@link #applyLazy(Supplier, Optional)} instead. Eager evaluation of the default wastes memory.
     */
    @Deprecated
    public static <X> X apply(X defaultValue, Optional<X> opt) {
        return opt.orElse(defaultValue);
    }

    /**
     * Lazily returns the value from an optional or a default value.
     * The default is only evaluated if the optional is empty.
     */
    public static <X> Function<Optional<X>, X> applyLazy(Supplier<X> defaultValue) {
        return (opt) -> applyLazy(defaultValue, opt);
    }

    /**
     * Lazily returns the value from an optional or a default value.
     * The default is only evaluated if the optional is empty.
     */
    public static <X> X applyLazy(Supplier<X> defaultValue, Optional<X> opt) {
        return opt.isGiven() ? opt.fromGiven() : defaultValue.get();
    }
}

package hydra.lib.maybes;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
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
 * Provides a default for Nothing.
 */
public class FromMaybe extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.maybes.fromMaybe"
     */
    public Name name() {
        return new Name("hydra.lib.maybes.fromMaybe");
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
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind(pure(args.get(0)), defaultTerm ->
            bind(Expect.optional(Flows::pure, args.get(1)), opt ->
                pure(opt.isPresent() ? opt.get() : defaultTerm)));
    }

    /**
     * Returns the value from an optional or a default value. Curried version.
     * @param <X> the value type
     * @param defaultValue the default value to return if the optional is empty
     * @return a function that takes an optional and returns the value or default
     */
    public static <X> Function<Opt<X>, X> apply(X defaultValue) {
        return (opt) -> apply(defaultValue, opt);
    }

    /**
     * Returns the value from an optional or a default value.
     * @param <X> the value type
     * @param defaultValue the default value to return if the optional is empty
     * @param opt the optional value
     * @return the contained value if present, otherwise the default value
     */
    public static <X> X apply(X defaultValue, Opt<X> opt) {
        return opt.orElse(defaultValue);
    }
}

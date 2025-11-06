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
    public Name name() {
        return new Name("hydra.lib.maybes.fromMaybe");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function("a", function(optional("a"), "a")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind(pure(args.get(0)), defaultTerm ->
            bind(Expect.optional(Flows::pure, args.get(1)), opt ->
                pure(opt.isPresent() ? opt.get() : defaultTerm)));
    }

    /**
     * Returns value or default.
     * @param defaultValue the default
     * @return the value or default
     */
        public static <X> Function<Opt<X>, X> apply(X defaultValue) {
        return (opt) -> apply(defaultValue, opt);
    }

    /**
     * Returns value or default.
     * @param defaultValue the default
     * @param opt the maybeValue
     * @return the value or default
     */
        public static <X> X apply(X defaultValue, Opt<X> opt) {
        return opt.orElse(defaultValue);
    }
}

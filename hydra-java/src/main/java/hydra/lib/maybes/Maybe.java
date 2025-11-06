package hydra.lib.maybes;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Flows;
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
 * Pattern matches with a default.
 */
public class Maybe extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.maybes.maybe");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b", function("b", function(function("a", "b"), function(optional("a"), "b"))));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind(Expect.optional(Flows::pure, args.get(2)), opt ->
            opt.isPresent()
                ? pure(Terms.apply(args.get(1), opt.get()))
                : pure(args.get(0)));
    }

    /**
     * Handles Nothing with default.
     * @param nothingCase the default
     * @return the result
     */
        public static <X, Y> Function<Function<X, Y>, Function<Opt<X>, Y>> apply(Y nothingCase) {
        return (justCase) -> (opt) -> apply(nothingCase, justCase, opt);
    }

    /**
     * Handles Nothing with default.
     * @param nothingCase the default
     * @param justCase the function
     * @param opt the maybeValue
     * @return the result
     */
        public static <X, Y> Y apply(Y nothingCase, Function<X, Y> justCase, Opt<X> opt) {
        return opt.isPresent() ? justCase.apply(opt.get()) : nothingCase;
    }
}

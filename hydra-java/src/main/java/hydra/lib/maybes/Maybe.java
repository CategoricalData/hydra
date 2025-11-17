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
        return scheme("a", "b", function("b", function(function("a", "b"), function(optional("a"), "b"))));
    }

    /**
     * Returns the implementation of this primitive function.
     * @return a function that performs pattern matching on optional values with a default
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind(Expect.optional(Flows::pure, args.get(2)), opt ->
            opt.isPresent()
                ? pure(Terms.apply(args.get(1), opt.get()))
                : pure(args.get(0)));
    }

    /**
     * Pattern matches on an optional value with a default. Curried version.
     * @param <X> the optional value type
     * @param <Y> the result type
     * @param nothingCase the default value to return if the optional is empty
     * @return a function that takes a Just case handler and returns a function that takes an optional
     */
    public static <X, Y> Function<Function<X, Y>, Function<Opt<X>, Y>> apply(Y nothingCase) {
        return (justCase) -> (opt) -> apply(nothingCase, justCase, opt);
    }

    /**
     * Pattern matches on an optional value with a default.
     * @param <X> the optional value type
     * @param <Y> the result type
     * @param nothingCase the default value to return if the optional is empty
     * @param justCase the function to apply to the value if the optional is present
     * @param opt the optional value to match on
     * @return the result of applying the appropriate handler
     */
    public static <X, Y> Y apply(Y nothingCase, Function<X, Y> justCase, Opt<X> opt) {
        return opt.isPresent() ? justCase.apply(opt.get()) : nothingCase;
    }
}

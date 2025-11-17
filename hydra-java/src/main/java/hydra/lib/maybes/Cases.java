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
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> bind(Expect.optional(Flows::pure, args.get(0)), opt ->
            opt.isPresent()
                ? pure(Terms.apply(args.get(2), opt.get()))
                : pure(args.get(1)));
    }

    /**
     * Handles Just and Nothing cases. Curried version.
     * @param <X> the optional value type
     * @param <Y> the result type
     * @param opt the optional value to match on
     * @return a function that takes a Nothing case handler and returns a function that takes a Just case handler
     */
    public static <X, Y> Function<Y, Function<Function<X, Y>, Y>> apply(Opt<X> opt) {
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
    public static <X, Y> Y apply(Opt<X> opt, Y nothingCase, Function<X, Y> justCase) {
        return opt.isPresent() ? justCase.apply(opt.get()) : nothingCase;
    }
}

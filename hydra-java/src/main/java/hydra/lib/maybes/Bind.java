package hydra.lib.maybes;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Opt;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;


/**
 * Monadic bind for flows.
 */
public class Bind extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.maybes.bind"
     */
    public Name name() {
        return new Name("hydra.lib.maybes.bind");
    }

    /**
     * Returns the type scheme of this primitive function.
     * @return the type scheme for monadic bind on optional values
     */
    @Override
    public TypeScheme type() {
        return scheme("a","b",
                function(optional("a"), function("a", optional("b")), optional("b")));
    }

    /**
     * Returns the implementation of this primitive function.
     * @return a function that performs monadic bind on optional values
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.optional(Flows::pure, args.get(0)),
            arg -> arg.map(term -> Terms.optional(Opt.of(Terms.apply(args.get(1), term))))
                .orElseGet(() -> Terms.optional(Opt.empty())));
    }

    /**
     * Chains flow computations. Curried version.
     * @param <X> the input type
     * @param <Y> the output type
     * @param optionalArg the optional value to bind
     * @return a function that takes a binding function and returns an optional result
     */
    public static <X, Y> Function<Function<X, Opt<Y>>, Opt<Y>> apply(Opt<X> optionalArg) {
        return (f) -> apply(optionalArg, f);
    }

    /**
     * Applies monadic bind to an optional value with a binding function.
     * @param <X> the input type
     * @param <Y> the output type
     * @param optionalArg the optional value to bind
     * @param f the binding function
     * @return the optional result of applying the binding function, or empty if the input is empty
     */
    public static <X, Y> Opt<Y> apply(Opt<X> optionalArg, Function<X, Opt<Y>> f) {
        return optionalArg.isPresent()
            ? f.apply(optionalArg.get())
            : Opt.empty();
    }
}

package hydra.lib.logic;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Performs logical OR on two boolean values.
 */
public class Or extends PrimitiveFunction {
    /**
     * Returns the fully qualified name of this primitive function.
     * @return the name "hydra.lib.logic.or"
     */
    public Name name() {
        return new Name("hydra.lib.logic.or");
    }

    /**
     * Returns the type scheme for this function.
     * @return a type scheme representing a function that takes two booleans and returns a boolean
     */
    @Override
    public TypeScheme type() {
        return scheme(function(boolean_(), boolean_(), boolean_()));
    }

    /**
     * Returns the implementation of this primitive function as an Either computation.
     * @return a function that takes a list of terms and returns an Either producing the OR result
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.Core.boolean_(cx, graph, args.get(0)), b1 -> hydra.lib.eithers.Map.apply(b2 -> Terms.boolean_(Or.apply(b1, b2)), hydra.extract.Core.boolean_(cx, graph, args.get(1))));
    }

    /**
     * Returns a function that performs logical OR with the given boolean value.
     * @param b1 the first boolean value
     * @return a function that takes a second boolean and returns the OR result
     */
    public static Function<Boolean, Boolean> apply(boolean b1) {
        return b2 -> apply(b1, b2);
    }

    /**
     * Performs logical OR on two boolean values.
     * @param b1 the first boolean value
     * @param b2 the second boolean value
     * @return true if at least one value is true, false otherwise
     */
    public static boolean apply(boolean b1, boolean b2) {
        return b1 || b2;
    }
}

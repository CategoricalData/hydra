package hydra.overlay.java.lib.logic;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.boolean_;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Performs logical negation on a boolean value.
 */
public class Not extends PrimitiveFunction {
    /**
     * Returns the fully qualified name of this primitive function.
     * @return the name "hydra.lib.logic.not"
     */
    public Name name() {
        return hydra.lib.Logic.not().name;
    }

    /**
     * Returns the type scheme for this function.
     * @return a type scheme representing a function that takes a boolean and returns a boolean
     */
    @Override
    public TypeScheme type() {
        return scheme(function(boolean_(), boolean_()));
    }

    /**
     * Returns the implementation of this primitive function as an Either computation.
     * @return a function that takes a list of terms and returns an Either producing the negated boolean
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply(b1 -> Terms.boolean_(Not.apply(b1)), hydra.extract.Core.boolean_(graph, args.get(0)));
    }

    /**
     * Negates the given boolean value.
     * @param b1 the boolean value to negate
     * @return the negated value
     */
    public static boolean apply(boolean b1) {
        return !b1;
    }
}

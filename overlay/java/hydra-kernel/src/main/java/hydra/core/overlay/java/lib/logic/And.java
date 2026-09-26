package hydra.core.overlay.java.lib.logic;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.boolean_;
import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Performs logical AND on two boolean values.
 */
public class And extends PrimitiveFunction {
    /**
     * Returns the fully qualified name of this primitive function.
     * @return the name "hydra.core.lib.logic.and"
     */
    public Name name() {
        return hydra.core.lib.Logic.and().name;
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
     * @return a function that takes a list of terms and returns an Either producing the AND result
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Bind.apply(hydra.core.extract.Model.boolean_(graph, args.get(0)), b1 -> hydra.core.overlay.java.lib.eithers.Map.apply(b2 -> Terms.boolean_(And.apply(b1, b2)), hydra.core.extract.Model.boolean_(graph, args.get(1))));
    }

    /**
     * Returns a function that performs logical AND with the given boolean value.
     * @param b1 the first boolean value
     * @return a function that takes a second boolean and returns the AND result
     */
    public static Function<Boolean, Boolean> apply(boolean b1) {
        return b2 -> apply(b1, b2);
    }

    /**
     * Performs logical AND on two boolean values.
     * @param b1 the first boolean value
     * @param b2 the second boolean value
     * @return true if both values are true, false otherwise
     */
    public static boolean apply(boolean b1, boolean b2) {
        return b1 && b2;
    }
}

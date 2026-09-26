package hydra.core.overlay.java.lib.optionals;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;
import hydra.core.overlay.java.util.Optional;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.boolean_;
import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.optional;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Checks if value is Nothing.
 */
public class IsNone extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.core.lib.optionals.isNone"
     */
    public Name name() {
        return hydra.core.lib.Optionals.isNone().name;
    }

    /**
     * Returns the type scheme of this primitive function.
     * @return the type scheme for checking if an optional is empty
     */
    @Override
    public TypeScheme type() {
        return scheme("a", function(optional("a"), boolean_()));
    }

    /**
     * Returns the implementation of this primitive function.
     * @return a function that checks if an optional value is empty
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply(x -> Terms.boolean_(IsNone.apply(x)), hydra.core.extract.Model.optionalTerm(t -> Either.right(t), graph, args.get(0)));
    }

    /**
     * Checks if an optional value is empty (Nothing).
     * @param <X> the value type
     * @param opt the optional value to check
     * @return true if the optional is empty, false otherwise
     */
    public static <X> boolean apply(Optional<X> opt) {
        return !opt.isGiven();
    }
}

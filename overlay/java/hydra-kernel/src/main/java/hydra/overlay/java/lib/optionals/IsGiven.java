package hydra.overlay.java.lib.optionals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;
import hydra.overlay.java.util.Optional;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.boolean_;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.optional;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


/**
 * Checks if value is Just.
 */
public class IsGiven extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.optionals.isGiven"
     */
    public Name name() {
        return hydra.lib.Optionals.isGiven().name;
    }

    /**
     * Returns the type scheme of this primitive function.
     * @return the type scheme for checking if an optional is present
     */
    @Override
    public TypeScheme type() {
        return scheme("a", function(optional("a"), boolean_()));
    }

    /**
     * Returns the implementation of this primitive function.
     * @return a function that checks if an optional value is present
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply(x -> Terms.boolean_(IsGiven.apply(x)), hydra.extract.Core.optionalTerm(t -> Either.right(t), graph, args.get(0)));
    }

    /**
     * Checks if an optional value is present (Just).
     * @param <X> the value type
     * @param opt the optional value to check
     * @return true if the optional contains a value, false otherwise
     */
    public static <X> boolean apply(Optional<X> opt) {
        return opt.isGiven();
    }
}

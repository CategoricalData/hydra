package hydra.lib.maybes;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Checks if value is Nothing.
 */
public class IsNothing extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.maybes.isNothing"
     */
    public Name name() {
        return new Name("hydra.lib.maybes.isNothing");
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(x -> Terms.boolean_(IsNothing.apply(x)), hydra.extract.Core.maybeTerm(t -> Either.right(t), graph, args.get(0)));
    }

    /**
     * Checks if an optional value is empty (Nothing).
     * @param <X> the value type
     * @param opt the optional value to check
     * @return true if the optional is empty, false otherwise
     */
    public static <X> boolean apply(Maybe<X> opt) {
        return !opt.isJust();
    }
}

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
import hydra.context.InContext;
import hydra.error.OtherError;
import hydra.util.Either;


/**
 * Checks if value is Just.
 */
public class IsJust extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.maybes.isJust"
     */
    public Name name() {
        return new Name("hydra.lib.maybes.isJust");
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(x -> Terms.boolean_(IsJust.apply(x)), hydra.extract.core.Core.maybeTerm(cx, t -> Either.right(t), graph, args.get(0)));
    }

    /**
     * Checks if an optional value is present (Just).
     * @param <X> the value type
     * @param opt the optional value to check
     * @return true if the optional contains a value, false otherwise
     */
    public static <X> boolean apply(Maybe<X> opt) {
        return opt.isJust();
    }
}

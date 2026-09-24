package hydra.core.overlay.java.lib.optionals;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;
import hydra.core.overlay.java.util.ConsList;
import hydra.core.overlay.java.util.Optional;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.optional;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Converts a Optional to a list: Just x becomes [x], Nothing becomes [].
 */
public class ToList extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.core.lib.optionals.toList"
     */
    public Name name() {
        return hydra.lib.Optionals.toList().name;
    }

    /**
     * Returns the type scheme of this primitive function.
     * @return the type scheme for converting an optional to a list
     */
    @Override
    public TypeScheme type() {
        return scheme("a", function(optional("a"), list("a")));
    }

    /**
     * Returns the implementation of this primitive function.
     * @return a function that converts an optional to a list
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply(
            (Function<Optional<Term>, Term>) opt -> Terms.list(apply(opt)),
            hydra.core.extract.Core.optionalTerm(t -> Either.right(t), graph, args.get(0)));
    }

    /**
     * Converts an optional value to a list.
     * @param <X> the element type
     * @param opt the optional value
     * @return a singleton list if Just, an empty list if Nothing
     */
    public static <X> List<X> apply(Optional<X> opt) {
        return opt.isGiven() ? ConsList.singleton(opt.fromGiven()) : ConsList.empty();
    }
}

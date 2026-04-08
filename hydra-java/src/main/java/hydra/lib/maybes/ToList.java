package hydra.lib.maybes;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Converts a Maybe to a list: Just x becomes [x], Nothing becomes [].
 */
public class ToList extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.maybes.toList"
     */
    public Name name() {
        return new Name("hydra.lib.maybes.toList");
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(
            (Function<Maybe<Term>, Term>) opt -> Terms.list(apply(opt)),
            hydra.extract.Core.maybeTerm(t -> Either.right(t), graph, args.get(0)));
    }

    /**
     * Converts an optional value to a list.
     * @param <X> the element type
     * @param opt the optional value
     * @return a singleton list if Just, an empty list if Nothing
     */
    public static <X> List<X> apply(Maybe<X> opt) {
        return opt.map(Collections::singletonList).orElse(new ArrayList<>());
    }
}

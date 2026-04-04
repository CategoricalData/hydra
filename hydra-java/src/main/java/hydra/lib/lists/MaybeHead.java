package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Get the first element of a list, returning Nothing if the list is empty.
 */
public class MaybeHead extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.maybeHead");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list("a"), optional("a")));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<List<Term>, Term>) l -> Terms.optional(MaybeHead.apply(l)), hydra.extract.Core.list(cx, graph, args.get(0)));
    }

    /**
     * Apply the function to its single argument.
     * @param <X> the element type
     * @param list the list to get the head from
     * @return a Maybe containing the first element, or empty if the list is empty
     */
    public static <X> Maybe<X> apply(List<X> list) {
        return list.isEmpty() ? Maybe.nothing() : Maybe.just(list.get(0));
    }
}

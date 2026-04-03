package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import hydra.util.ConsList;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Get the element at a given index in a list, returning Nothing if the index is out of bounds.
 */
public class MaybeAt extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.maybeAt");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(int32(), list("a"), optional("a")));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.Core.int32(cx, graph, args.get(0)), i -> hydra.lib.eithers.Map.apply((Function<ConsList<Term>, Term>) l -> Terms.optional(MaybeAt.apply(i, l)), hydra.extract.Core.list(cx, graph, args.get(1))));
    }

    /**
     * Apply the function to its arguments.
     * @param <X> the element type
     * @param index the zero-based index
     * @param list the list to get the element from
     * @return a Maybe containing the element at the index, or empty if out of bounds
     */
    public static <X> Maybe<X> apply(int index, ConsList<X> list) {
        if (index < 0 || index >= list.size()) {
            return Maybe.nothing();
        } else {
            return Maybe.just(list.get(index));
        }
    }
}

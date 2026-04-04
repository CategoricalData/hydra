package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.variable;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Gets the element at the specified index of a list.
 */
public class At extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.at");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(int32(), list("a"), variable("a")));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.Core.int32(cx, graph, args.get(0)), i -> hydra.lib.eithers.Map.apply((Function<List<Term>, Term>) list -> list.get(i), hydra.extract.Core.list(cx, graph, args.get(1))));
    }

    /**
     * Gets the element at the specified index.
     * @param <X> the element type
     * @param i the zero-based index
     * @return a function that gets the element at the given index from a list
     */
    public static <X> Function<List<X>, X> apply(int i) {
        return list -> apply(i, list);
    }

    /**
     * Gets the element at the specified index. Throws if the list is empty.
     * @param <X> the element type
     * @param i the zero-based index
     * @param list the list to get the element from
     * @return the element at the given index
     * @throws IllegalArgumentException if the list is empty
     */
    public static <X> X apply(int i, List<X> list) {
        if (list.isEmpty()) {
            throw new IllegalArgumentException("Cannot get head of empty list");
        } else {
            return list.get(i);
        }
    }
}

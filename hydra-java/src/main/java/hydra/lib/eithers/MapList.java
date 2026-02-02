package hydra.lib.eithers;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.either;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.var;

/**
 * Map a function that may fail over a list, collecting results or returning the first error.
 */
public class MapList extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.eithers.mapList");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b", "z",
            function(
                function(var("a"), either(var("z"), var("b"))),
                list(var("a")),
                either(var("z"), list(var("b")))));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> {
            throw new UnsupportedOperationException("MapList primitive not yet implemented for term evaluation");
        };
    }

    /**
     * Map a function over a list, returning Left on the first failure or Right with all results.
     */
    public static <A, B, Z> hydra.util.Either<Z, List<B>> apply(
            Function<A, hydra.util.Either<Z, B>> fn,
            List<A> items) {
        List<B> results = new ArrayList<>();
        for (A item : items) {
            hydra.util.Either<Z, B> result = fn.apply(item);
            if (result.isLeft()) {
                return new hydra.util.Either.Left<>(((hydra.util.Either.Left<Z, B>) result).value);
            }
            results.add(((hydra.util.Either.Right<Z, B>) result).value);
        }
        return new hydra.util.Either.Right<>(results);
    }

    /**
     * Curried version for method references.
     */
    public static <A, B, Z> Function<List<A>, hydra.util.Either<Z, List<B>>> apply(
            Function<A, hydra.util.Either<Z, B>> fn) {
        return items -> apply(fn, items);
    }
}

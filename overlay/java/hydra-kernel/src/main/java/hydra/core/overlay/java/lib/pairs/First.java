package hydra.core.overlay.java.lib.pairs;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;
import hydra.core.overlay.java.util.Pair;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.pair;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.variable;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Extracts the first element of a pair.
 */
public class First extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return hydra.core.lib.Pairs.first().name;
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme("a", "b", function(pair(variable("a"), variable("b")), "a"));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply((Function<Pair<Term, Term>, Term>) pair -> apply(pair), hydra.core.extract.Model.pair(t -> Either.right(t), t -> Either.right(t), graph, args.get(0)));
    }

    /**
     * Apply the function to extract the first element of a pair.
     * @param <A> the first element type
     * @param <B> the second element type
     * @param pair the pair
     * @return the first element
     */
    public static <A, B> A apply(Pair<A, B> pair) {
        return pair.first;
    }
}

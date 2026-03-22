package hydra.lib.pairs;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Pair;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.pair;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.variable;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Extracts the second element of a pair.
 */
public class Second extends PrimitiveFunction {
    /**
     * Get the name of this primitive function.
     * @return the name
     */
    public Name name() {
        return new Name("hydra.lib.pairs.second");
    }

    /**
     * Get the type scheme of this primitive function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme("a", "b", function(pair(variable("a"), variable("b")), "b"));
    }

    /**
     * Get the implementation of this primitive function.
     * @return the implementation function
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<Pair<Term, Term>, Term>) pair -> apply(pair), hydra.extract.Core.pair(cx, t -> Either.right(t), t -> Either.right(t), graph, args.get(0)));
    }

    /**
     * Apply the function to extract the second element of a pair.
     * @param <A> the first element type
     * @param <B> the second element type
     * @param pair the pair
     * @return the second element
     */
    public static <A, B> B apply(Pair<A, B> pair) {
        return pair.second;
    }
}

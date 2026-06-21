package hydra.lib.effects;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.core.TypeScheme;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.var;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Map an effect-returning function over a list.
 * In Java the effect type is transparent (effect&lt;t&gt; = t), so this applies the
 * function to each element from left to right and collects the results.
 */
public class MapList extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.effects.mapList"
     */
    public Name name() {
        return hydra.lib.Effects.mapList().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme (x -&gt; effect&lt;y&gt;) -&gt; [x] -&gt; effect&lt;[y]&gt;
     */
    @Override
    public TypeScheme type() {
        return scheme("x", "y",
            function(
                function(var("x"), new Type.Effect(var("y"))),
                list(var("x")),
                new Type.Effect(list(var("y")))));
    }

    /**
     * Provides the implementation of this primitive function.
     * Effectful primitives are evaluated through the native (host) path; the
     * term-level interpreter cannot reduce them, so this returns a function that
     * yields a deferred error when applied.
     * @return a function that yields a deferred error on reduction
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> Either.left(
            new hydra.errors.Error_.Other(new hydra.errors.OtherError(
                "effect primitive cannot be reduced by Hydra's pure reducer: " + name().value)));
    }

    /**
     * Map an effect-returning function over a list, collecting the results.
     * @param <A> the input element type
     * @param <B> the output element type
     * @param f the effect-returning function
     * @param list the list to map over
     * @return the list of results
     */
    public static <A, B> List<B> apply(Function<A, B> f, List<A> list) {
        List<B> result = new ArrayList<B>(list.size());
        for (A element : list) {
            result.add(f.apply(element));
        }
        return result;
    }
}

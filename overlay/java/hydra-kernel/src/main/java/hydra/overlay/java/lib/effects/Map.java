package hydra.overlay.java.lib.effects;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.core.TypeScheme;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.var;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Map a pure function over the result of an effect.
 * In Java the effect type is transparent (effect&lt;t&gt; = t), so this applies the
 * function to the (already-evaluated) effect result.
 */
public class Map extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.effects.map"
     */
    public Name name() {
        return hydra.lib.Effects.map().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme (x -&gt; y) -&gt; effect&lt;x&gt; -&gt; effect&lt;y&gt;
     */
    @Override
    public TypeScheme type() {
        return scheme("x", "y",
            function(
                function(var("x"), var("y")),
                new Type.Effect(var("x")),
                new Type.Effect(var("y"))));
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

    @Override
    protected boolean isPure() {
        return false;
    }

    /**
     * Map a pure function over the result of an effect.
     * @param <A> the effect result type
     * @param <B> the mapped result type
     * @param f the function to apply
     * @param value the effect result
     * @return the result of applying f to value
     */
    public static <A, B> B apply(Function<A, B> f, A value) {
        return f.apply(value);
    }
}

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
 * Sequence two effectful computations.
 * In Java the effect type is transparent (effect&lt;t&gt; = t), so this forces the
 * first effect (already a value) and passes it to the continuation.
 */
public class Bind extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.effects.bind"
     */
    public Name name() {
        return hydra.lib.Effects.bind().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme effect&lt;x&gt; -&gt; (x -&gt; effect&lt;y&gt;) -&gt; effect&lt;y&gt;
     */
    @Override
    public TypeScheme type() {
        return scheme("x", "y",
            function(
                new Type.Effect(var("x")),
                function(var("x"), new Type.Effect(var("y"))),
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
     * Sequence two effectful computations: pass the value of the first effect to the continuation.
     * @param <A> the result type of the first effect
     * @param <B> the result type of the second effect
     * @param value the value of the first effect
     * @param f the continuation producing the second effect
     * @return the result of applying f to value
     */
    public static <A, B> B apply(A value, Function<A, B> f) {
        return f.apply(value);
    }
}

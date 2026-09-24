package hydra.core.overlay.java.lib.effects;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.Type;
import hydra.core.model.TypeScheme;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.var;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Lift a pure value into an effect.
 * In Java the effect type is transparent (effect&lt;t&gt; = t), so this is the identity function.
 */
public class Pure extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.core.lib.effects.pure"
     */
    public Name name() {
        return hydra.lib.Effects.pure().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme x -&gt; effect&lt;x&gt;
     */
    @Override
    public TypeScheme type() {
        return scheme("x", function(var("x"), new Type.Effect(var("x"))));
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
            new hydra.core.errors.Error_.Other(new hydra.core.errors.OtherError(
                "effect primitive cannot be reduced by Hydra's pure reducer: " + name().value)));
    }

    @Override
    protected boolean isPure() {
        return false;
    }

    /**
     * Lift a pure value into an effect (identity, since effects are transparent in Java).
     * @param <A> the value type
     * @param value the value to lift
     * @return the value unchanged
     */
    public static <A> A apply(A value) {
        return value;
    }
}

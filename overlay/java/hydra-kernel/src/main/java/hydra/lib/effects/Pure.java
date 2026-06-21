package hydra.lib.effects;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.core.TypeScheme;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.var;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Lift a pure value into an effect.
 * In Java the effect type is transparent (effect&lt;t&gt; = t), so this is the identity function.
 */
public class Pure extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.effects.pure"
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
            new hydra.errors.Error_.Other(new hydra.errors.OtherError(
                "effect primitive cannot be reduced by Hydra's pure reducer: " + name().value)));
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

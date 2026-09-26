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
import static hydra.core.overlay.java.dsl.Types.optional;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.var;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;
import hydra.core.overlay.java.util.Optional;

/**
 * Map an effect-returning function over an optional.
 * In Java the effect type is transparent (effect&lt;t&gt; = t), so this returns none
 * when the optional is none, or applies the function to the contained value.
 */
public class MapOptional extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.core.lib.effects.mapOptional"
     */
    public Name name() {
        return hydra.core.lib.Effects.mapOptional().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme (x -&gt; effect&lt;y&gt;) -&gt; optional&lt;x&gt; -&gt; effect&lt;optional&lt;y&gt;&gt;
     */
    @Override
    public TypeScheme type() {
        return scheme("x", "y",
            function(
                function(var("x"), new Type.Effect(var("y"))),
                optional(var("x")),
                new Type.Effect(optional(var("y")))));
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
     * Map an effect-returning function over an optional.
     * @param <A> the input type
     * @param <B> the output type
     * @param f the effect-returning function
     * @param opt the optional to map over
     * @return none if opt is none, otherwise given the result of applying f
     */
    public static <A, B> Optional<B> apply(Function<A, B> f, Optional<A> opt) {
        return opt.map(f);
    }
}

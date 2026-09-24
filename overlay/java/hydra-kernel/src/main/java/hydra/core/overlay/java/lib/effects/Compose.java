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
 * Kleisli composition for effects.
 * In Java the effect type is transparent (effect&lt;t&gt; = t), so this is ordinary
 * function composition: compose(f, g, x) = g(f(x)).
 */
public class Compose extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.core.lib.effects.compose"
     */
    public Name name() {
        return hydra.lib.Effects.compose().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme (x -&gt; effect&lt;y&gt;) -&gt; (y -&gt; effect&lt;z&gt;) -&gt; x -&gt; effect&lt;z&gt;
     */
    @Override
    public TypeScheme type() {
        return scheme("x", "y", "z",
            function(
                function(var("x"), new Type.Effect(var("y"))),
                function(var("y"), new Type.Effect(var("z"))),
                var("x"),
                new Type.Effect(var("z"))));
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
     * Kleisli composition: compose(f, g, x) = g(f(x)).
     * @param <A> the input type
     * @param <B> the intermediate type
     * @param <C> the result type
     * @param f the first effect-returning function
     * @param g the second effect-returning function
     * @param value the input value
     * @return the result of applying g to the result of applying f to value
     */
    public static <A, B, C> C apply(Function<A, B> f, Function<B, C> g, A value) {
        return g.apply(f.apply(value));
    }
}

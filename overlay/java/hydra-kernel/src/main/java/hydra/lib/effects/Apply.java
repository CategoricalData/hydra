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
 * Applicative apply for effects.
 * In Java the effect type is transparent (effect&lt;t&gt; = t), so this applies the
 * (already-evaluated) effectful function to the (already-evaluated) effectful argument.
 */
public class Apply extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.effects.apply"
     */
    public Name name() {
        return hydra.lib.Effects.apply().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme effect&lt;x -&gt; y&gt; -&gt; effect&lt;x&gt; -&gt; effect&lt;y&gt;
     */
    @Override
    public TypeScheme type() {
        return scheme("x", "y",
            function(
                new Type.Effect(function(var("x"), var("y"))),
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

    /**
     * Applicative apply: apply an effectful function to an effectful argument.
     * @param <A> the argument type
     * @param <B> the result type
     * @param f the effectful function
     * @param value the effectful argument
     * @return the result of applying f to value
     */
    public static <A, B> B apply(Function<A, B> f, A value) {
        return f.apply(value);
    }
}

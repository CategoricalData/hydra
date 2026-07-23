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
import static hydra.overlay.java.dsl.Types.list;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.var;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Left-fold over a list with an effect-returning function.
 * In Java the effect type is transparent (effect&lt;t&gt; = t), so this is an ordinary
 * left fold which sequences applications of the curried function from left to right.
 */
public class FoldList extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.effects.foldList"
     */
    public Name name() {
        return hydra.lib.Effects.foldList().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme (x -&gt; y -&gt; effect&lt;x&gt;) -&gt; x -&gt; [y] -&gt; effect&lt;x&gt;
     */
    @Override
    public TypeScheme type() {
        return scheme("x", "y",
            function(
                function(var("x"), function(var("y"), new Type.Effect(var("x")))),
                var("x"),
                list(var("y")),
                new Type.Effect(var("x"))));
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
     * Left-fold over a list with a curried effect-returning function.
     * @param <A> the accumulator type
     * @param <B> the element type
     * @param f the curried folding function: acc -&gt; element -&gt; acc
     * @param acc the initial accumulator
     * @param list the list to fold over
     * @return the final accumulator
     */
    public static <A, B> A apply(Function<A, Function<B, A>> f, A acc, List<B> list) {
        A result = acc;
        for (B element : list) {
            result = f.apply(result).apply(element);
        }
        return result;
    }
}

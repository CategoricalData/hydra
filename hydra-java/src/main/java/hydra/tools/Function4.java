package hydra.tools;

import java.util.Objects;
import java.util.function.Function;

/**
 * A functional interface representing a function that accepts four arguments and produces a result.
 * @param <A> the type of the first argument
 * @param <B> the type of the second argument
 * @param <C> the type of the third argument
 * @param <D> the type of the fourth argument
 * @param <R> the type of the result
 */
@FunctionalInterface
public interface Function4<A, B, C, D, R> {

    /**
     * Apply this function to the given arguments.
     * @param a the first argument
     * @param b the second argument
     * @param c the third argument
     * @param d the fourth argument
     * @return the result
     */
    R apply(A a, B b, C c, D d);

    /**
     * Returns a composed function that first applies this function and then applies the after function to the result.
     * @param <K> the type of the output of the after function
     * @param after the function to apply after this function
     * @return the composed function
     */
    default <K> Function4<A, B, C, D, K> andThen(Function<? super R, ? extends K> after) {
        Objects.requireNonNull(after);
        return (A a, B b, C c, D d) -> after.apply(apply(a, b, c, d));
    }
}

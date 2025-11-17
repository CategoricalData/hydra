package hydra.tools;

import java.util.Objects;
import java.util.function.Function;

/**
 * A functional interface representing a function that accepts three arguments and produces a result.
 * Copied from https://www.baeldung.com/java-trifunction
 * @param <A> the type of the first argument
 * @param <B> the type of the second argument
 * @param <C> the type of the third argument
 * @param <R> the type of the result
 */
@FunctionalInterface
public interface Function3<A, B, C, R> {

    /**
     * Apply this function to the given arguments.
     * @param a the first argument
     * @param b the second argument
     * @param c the third argument
     * @return the result
     */
    R apply(A a, B b, C c);

    /**
     * Returns a composed function that first applies this function and then applies the after function to the result.
     * @param <K> the type of the output of the after function
     * @param after the function to apply after this function
     * @return the composed function
     */
    default <K> Function3<A, B, C, K> andThen(Function<? super R, ? extends K> after) {
        Objects.requireNonNull(after);
        return (A a, B b, C c) -> after.apply(apply(a, b, c));
    }
}

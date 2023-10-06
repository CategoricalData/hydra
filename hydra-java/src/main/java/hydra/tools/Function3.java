package hydra.tools;

import java.util.Objects;
import java.util.function.Function;

// Copied from https://www.baeldung.com/java-trifunction
@FunctionalInterface
public interface Function3<A, B, C, R> {

    R apply(A a, B b, C c);

    default <K> Function3<A, B, C, K> andThen(Function<? super R, ? extends K> after) {
        Objects.requireNonNull(after);
        return (A a, B b, C c) -> after.apply(apply(a, b, c));
    }
}

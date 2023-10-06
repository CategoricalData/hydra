package hydra.tools;

import java.util.Objects;
import java.util.function.Function;

@FunctionalInterface
public interface Function4<A, B, C, D, R> {

    R apply(A a, B b, C c, D d);

    default <K> Function4<A, B, C, D, K> andThen(Function<? super R, ? extends K> after) {
        Objects.requireNonNull(after);
        return (A a, B b, C c, D d) -> after.apply(apply(a, b, c, d));
    }
}

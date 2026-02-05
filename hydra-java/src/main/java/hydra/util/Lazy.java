package hydra.util;

import java.util.function.Supplier;

/**
 * A memoizing supplier that evaluates its value at most once.
 * Equivalent to Python's @lru_cache(1) on zero-arg functions,
 * matching Haskell's lazy evaluation with sharing for let bindings.
 */
public class Lazy<T> implements Supplier<T> {
    private Supplier<T> supplier;
    private T value;
    private boolean evaluated;

    public Lazy(Supplier<T> supplier) {
        this.supplier = supplier;
    }

    @Override
    public T get() {
        if (!evaluated) {
            value = supplier.get();
            supplier = null;
            evaluated = true;
        }
        return value;
    }
}

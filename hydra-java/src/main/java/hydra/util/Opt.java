package hydra.util;

import java.io.Serializable;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import static java.util.Objects.requireNonNull;

/**
 * A clone of java.util.Optional which implements Serializable and can therefore be used with frameworks
 * including Spark which require serializable objects.
 */
public class Opt<T> implements Serializable {
    private final T value;

    private Opt() {
        this.value = null;
    }

    private Opt(T value) {
        this.value = value;
    }

    public static <T> Opt<T> empty() {
        return new Opt<>();
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (!(obj instanceof Opt)) {
            return false;
        }

        Opt<?> other = (Opt<?>) obj;
        return Objects.equals(value, other.value);
    }
    
    public <U> Opt<U> flatMap(Function<? super T, ? extends Opt<? extends U>> mapper) {
        Objects.requireNonNull(mapper);
        if (!isPresent()) {
            return empty();
        } else {
            @SuppressWarnings("unchecked")
            Opt<U> r = (Opt<U>) mapper.apply(value);
            return Objects.requireNonNull(r);
        }
    }
    
    public T get() {
        if (value == null) {
            throw new NoSuchElementException("No value present");
        } else {
            return value;
        }
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(value);
    }

    public void ifPresent(Consumer<? super T> action) {
        if (value != null) {
            action.accept(value);
        }
    }

    public boolean isPresent() {
        return value != null;
    }

    public boolean isEmpty() {
        return value == null;
    }

    public <U> Opt<U> map(Function<? super T, ? extends U> mapper) {
        Objects.requireNonNull(mapper);
        if (!isPresent()) {
            return empty();
        } else {
            return Opt.ofNullable(mapper.apply(value));
        }
    }

    public static <T> Opt<T> of(T value) {
        requireNonNull(value);
        return new Opt<>(value);
    }

    public static <T> Opt<T> ofNullable(T value) {
        return value == null ? empty() : of(value);
    }

    public T orElse(T other) {
        return value != null ? value : other;
    }

    public T orElseGet(Supplier<? extends T> supplier) {
        return value != null ? value : supplier.get();
    }
}

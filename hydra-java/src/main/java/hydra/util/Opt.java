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
 *
 * @param <T> the type
 */
public class Opt<T> implements Serializable {
    private final T value;

    private Opt() {
        this.value = null;
    }

    private Opt(T value) {
        this.value = value;
    }

    /**
     * Creates an empty Opt instance.
     *
     * @param <T> the type
     * @return an empty Opt
     */
    public static <T> Opt<T> empty() {
        return new Opt<>();
    }

    /**
     * Checks if this Opt is equal to another object.
     *
     * @param obj the object to compare to
     * @return true if the objects are equal, false otherwise
     */
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
    
    /**
     * Applies a flatMap operation to this Opt.
     *
     * @param <U> the type
     * @param mapper the mapping function
     * @return the result of applying the mapper if a value is present, otherwise an empty Opt
     */
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
    
    /**
     * Gets the value if present, otherwise throws an exception.
     *
     * @return the value
     * @throws NoSuchElementException if no value is present
     */
    public T get() {
        if (value == null) {
            throw new NoSuchElementException("No value present");
        } else {
            return value;
        }
    }

    /**
     * Returns the hash code of this Opt.
     *
     * @return the hash code
     */
    @Override
    public int hashCode() {
        return Objects.hashCode(value);
    }

    /**
     * Executes the given action if a value is present.
     *
     * @param action the action to execute
     */
    public void ifPresent(Consumer<? super T> action) {
        if (value != null) {
            action.accept(value);
        }
    }

    /**
     * Checks if a value is present.
     *
     * @return true if a value is present, false otherwise
     */
    public boolean isPresent() {
        return value != null;
    }

    /**
     * Checks if this Opt is empty.
     *
     * @return true if no value is present, false otherwise
     */
    public boolean isEmpty() {
        return value == null;
    }

    /**
     * Applies a mapping function to this Opt.
     *
     * @param <U> the type
     * @param mapper the mapping function
     * @return an Opt containing the result of applying the mapper if a value is present, otherwise an empty Opt
     */
    public <U> Opt<U> map(Function<? super T, ? extends U> mapper) {
        Objects.requireNonNull(mapper);
        if (!isPresent()) {
            return empty();
        } else {
            return Opt.ofNullable(mapper.apply(value));
        }
    }

    /**
     * Creates an Opt with the given non-null value.
     *
     * @param <T> the type
     * @param value the value, must not be null
     * @return an Opt containing the value
     * @throws NullPointerException if value is null
     */
    public static <T> Opt<T> of(T value) {
        requireNonNull(value);
        return new Opt<>(value);
    }

    /**
     * Creates an Opt with the given value, or an empty Opt if the value is null.
     *
     * @param <T> the type
     * @param value the value, may be null
     * @return an Opt containing the value if non-null, otherwise an empty Opt
     */
    public static <T> Opt<T> ofNullable(T value) {
        return value == null ? empty() : of(value);
    }

    /**
     * Returns the value if present, otherwise returns the given default value.
     *
     * @param other the default value to return if no value is present
     * @return the value if present, otherwise the default value
     */
    public T orElse(T other) {
        return value != null ? value : other;
    }

    /**
     * Returns the value if present, otherwise returns the result of the supplier.
     *
     * @param supplier the supplier to invoke if no value is present
     * @return the value if present, otherwise the result of the supplier
     */
    public T orElseGet(Supplier<? extends T> supplier) {
        return value != null ? value : supplier.get();
    }
}

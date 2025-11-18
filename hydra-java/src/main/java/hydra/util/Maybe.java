package hydra.util;

import java.io.Serializable;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import static java.util.Objects.requireNonNull;

/**
 * A Hydra binary labeled union type, similar to java.util.Optional.
 * This class implements Serializable and can be used in place of java.util.Optional with frameworks
 * including Spark which require serializable objects.
 *
 * @param <T> the element type
 */
public class Maybe<T> implements Serializable {
    private final T value;

    private Maybe() {
        this.value = null;
    }

    private Maybe(T value) {
        this.value = value;
    }
    
    /**
     * Checks if this Maybe is equal to another object.
     *
     * @param obj the object to compare to
     * @return true if the objects are equal, false otherwise
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (!(obj instanceof Maybe)) {
            return false;
        }

        Maybe<?> other = (Maybe<?>) obj;
        return Objects.equals(value, other.value);
    }
    
    /**
     * Applies a flatMap operation to this Maybe.
     *
     * @param <U> the type
     * @param mapper the mapping function
     * @return the result of applying the mapper if a value is present, otherwise an empty Maybe
     */
    public <U> Maybe<U> flatMap(Function<? super T, ? extends Maybe<? extends U>> mapper) {
        Objects.requireNonNull(mapper);
        if (!isJust()) {
            return nothing();
        } else {
            @SuppressWarnings("unchecked")
            Maybe<U> r = (Maybe<U>) mapper.apply(value);
            return Objects.requireNonNull(r);
        }
    }
    
    /**
     * Gets the value if present, otherwise throws an exception. Analogous to Optional.get.
     *
     * @return the value
     * @throws NoSuchElementException if no value is present
     */
    public T fromJust() {
        if (value == null) {
            throw new NoSuchElementException("No value present");
        } else {
            return value;
        }
    }

    /**
     * Returns the hash code of this Maybe.
     *
     * @return the hash code
     */
    @Override
    public int hashCode() {
        return Objects.hashCode(value);
    }

    /**
     * Executes the given action if a value is present. Analogous to Optional.ifPresent.
     *
     * @param action the action to execute
     */
    public void ifJust(Consumer<? super T> action) {
        if (value != null) {
            action.accept(value);
        }
    }

    /**
     * Checks if a value is present. Analogous to Optional.isPresent.
     *
     * @return true if a value is present, false otherwise
     */
    public boolean isJust() {
        return value != null;
    }

    /**
     * Checks if this Maybe is empty. Analogous to Optional.isPresent.
     *
     * @return true if no value is present, false otherwise
     */
    public boolean isNothing() {
        return value == null;
    }

    /**
     * Creates a Maybe with the given non-null value. Analogous to Optional.of.
     *
     * @param <T> the type
     * @param value the value, must not be null
     * @return an Opt containing the value
     * @throws NullPointerException if value is null
     */
    public static <T> Maybe<T> just(T value) {
        requireNonNull(value);
        return new Maybe<>(value);
    }

    /**
     * Creates a Maybe with the given value, or an empty Maybe if the value is null. Analogous to Optional.ofNullable.
     *
     * @param <T> the type
     * @param value the value, may be null
     * @return an Opt containing the value if non-null, otherwise an empty Maybe
     */
    public static <T> Maybe<T> justNullable(T value) {
        return value == null ? nothing() : just(value);
    }

    /**
     * Applies a mapping function to this Maybe.
     *
     * @param <U> the type
     * @param mapper the mapping function
     * @return a Maybe containing the result of applying the mapper if a value is present, otherwise an empty Maybe
     */
    public <U> Maybe<U> map(Function<? super T, ? extends U> mapper) {
        Objects.requireNonNull(mapper);
        if (!isJust()) {
            return nothing();
        } else {
            return Maybe.justNullable(mapper.apply(value));
        }
    }
    
    /**
     * Creates an empty Maybe instance. Analogous to Optional.empty().
     *
     * @param <T> the type
     * @return an empty Maybe
     */
    public static <T> Maybe<T> nothing() {
        return new Maybe<>();
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

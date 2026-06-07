package hydra.util;

import java.io.Serializable;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * A Hydra binary labeled union type, similar to java.util.Optional.
 * This class implements Serializable and can be used in place of java.util.Optional with frameworks
 * including Spark which require serializable objects.
 *
 * Unlike java.util.Optional, this class can hold null values (e.g. for Void/unit types),
 * using a separate flag to distinguish Given(null) from None.
 *
 * @param <T> the element type
 */
@SuppressWarnings("rawtypes")
public class Optional<T> implements Serializable, Comparable<Optional> {
    private final T value;
    private final boolean present;

    private Optional() {
        this.value = null;
        this.present = false;
    }

    private Optional(T value) {
        this.value = value;
        this.present = true;
    }

    /**
     * Checks if this Optional is equal to another object.
     *
     * @param obj the object to compare to
     * @return true if the objects are equal, false otherwise
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (!(obj instanceof Optional)) {
            return false;
        }

        Optional<?> other = (Optional<?>) obj;
        if (present != other.present) {
            return false;
        }
        return Objects.equals(value, other.value);
    }

    /**
     * Applies a flatMap operation to this Optional.
     *
     * @param <U> the type
     * @param mapper the mapping function
     * @return the result of applying the mapper if a value is present, otherwise an empty Optional
     */
    public <U> Optional<U> flatMap(Function<? super T, ? extends Optional<? extends U>> mapper) {
        Objects.requireNonNull(mapper);
        if (!isGiven()) {
            return none();
        } else {
            @SuppressWarnings("unchecked")
            Optional<U> r = (Optional<U>) mapper.apply(value);
            return Objects.requireNonNull(r);
        }
    }

    /**
     * Gets the value if present, otherwise throws an exception. Analogous to java.util.Optional.get.
     *
     * @return the value
     * @throws NoSuchElementException if no value is present
     */
    public T fromGiven() {
        if (!present) {
            throw new NoSuchElementException("No value present");
        } else {
            return value;
        }
    }

    /**
     * Returns the hash code of this Optional.
     *
     * @return the hash code
     */
    @Override
    public int hashCode() {
        return present ? Objects.hashCode(value) + 1 : 0;
    }

    /**
     * Compares this Optional to another. None &lt; Given, and Given values are compared by their content.
     */
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Optional other) {
        if (!this.present && !other.present) return 0;
        if (!this.present) return -1;
        if (!other.present) return 1;
        return ((Comparable) this.value).compareTo(other.value);
    }

    /**
     * Executes the given action if a value is present. Analogous to java.util.Optional.ifPresent.
     *
     * @param action the action to execute
     */
    public void ifGiven(Consumer<? super T> action) {
        if (present) {
            action.accept(value);
        }
    }

    /**
     * Checks if a value is present. Analogous to java.util.Optional.isPresent.
     *
     * @return true if a value is present, false otherwise
     */
    public boolean isGiven() {
        return present;
    }

    /**
     * Checks if this Optional is empty. Analogous to java.util.Optional.isPresent.
     *
     * @return true if no value is present, false otherwise
     */
    public boolean isNone() {
        return !present;
    }

    /**
     * Creates an Optional with the given value. Analogous to java.util.Optional.of, but allows null
     * values (needed for Void/unit types).
     *
     * @param <T> the type
     * @param value the value
     * @return an Optional containing the value
     */
    public static <T> Optional<T> given(T value) {
        return new Optional<>(value);
    }

    /**
     * Creates an Optional with the given value, or an empty Optional if the value is null.
     * Analogous to java.util.Optional.ofNullable.
     *
     * @param <T> the type
     * @param value the value, may be null
     * @return an Optional containing the value if non-null, otherwise an empty Optional
     */
    public static <T> Optional<T> givenNullable(T value) {
        return value == null ? none() : given(value);
    }

    /**
     * Applies a mapping function to this Optional.
     *
     * @param <U> the type
     * @param mapper the mapping function
     * @return an Optional containing the result of applying the mapper if a value is present, otherwise an empty Optional
     */
    public <U> Optional<U> map(Function<? super T, ? extends U> mapper) {
        Objects.requireNonNull(mapper);
        if (!isGiven()) {
            return none();
        } else {
            return Optional.given(mapper.apply(value));
        }
    }

    /**
     * Creates an empty Optional instance. Analogous to java.util.Optional.empty().
     *
     * @param <T> the type
     * @return an empty Optional
     */
    public static <T> Optional<T> none() {
        return new Optional<>();
    }

    /**
     * Returns the value if present, otherwise returns the given default value.
     *
     * @param other the default value to return if no value is present
     * @return the value if present, otherwise the default value
     */
    public T orElse(T other) {
        return present ? value : other;
    }

    /**
     * Returns the value if present, otherwise returns the result of the supplier.
     *
     * @param supplier the supplier to invoke if no value is present
     * @return the value if present, otherwise the result of the supplier
     */
    public T orElseGet(Supplier<? extends T> supplier) {
        return present ? value : supplier.get();
    }

    /**
     * Converts this Optional to a java.util.Optional. Note: Given(null) maps to java.util.Optional.empty(),
     * since java.util.Optional does not support null values.
     *
     * @return a java.util.Optional containing the value if present and non-null, otherwise empty
     */
    public java.util.Optional<T> toJavaOptional() {
        return present ? java.util.Optional.ofNullable(value) : java.util.Optional.empty();
    }

    /**
     * Creates a hydra.util.Optional from a java.util.Optional.
     *
     * @param <T> the type
     * @param optional the java.util.Optional to convert
     * @return an Optional containing the value if present, otherwise None
     */
    public static <T> Optional<T> fromJavaOptional(java.util.Optional<T> optional) {
        return optional.isPresent() ? given(optional.get()) : none();
    }
}

package hydra.tools;

import java.util.ArrayList;
import java.util.List;
import hydra.util.Maybe;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * A base class for simple functional mappers with exceptions (specific to Java).
 */
public abstract class MapperBase {

    /**
     * Apply a function to a single element or list of elements, depending on the list size.
     *
     * @param <T> the type of elements in the list
     * @param <C> the return type
     * @param list the list to process
     * @param one the function to apply if the list has exactly one element
     * @param many the function to apply if the list has multiple elements
     * @return the result of applying the appropriate function
     * @throws MapperException if the list is empty
     */
    protected static <T, C> C oneOrMany(List<T> list, Function<T, C> one, Function<List<T>, C> many) {
        if (list.isEmpty()) {
            return invalidEmptyList();
        } else if (list.size() == 1) {
            return one.apply(list.get(0));
        } else {
            return many.apply(list);
        }
    }

    /**
     * Throw a MapperException with the given message.
     *
     * @param <T> the expected return type (never actually returned)
     * @param message the error message
     * @return never returns; always throws an exception
     * @throws MapperException always
     */
    protected static <T> T invalid(String message) {
        throw new MapperException(message);
    }

    /**
     * Throw a MapperException indicating an empty list was encountered.
     *
     * @param <T> the expected return type (never actually returned)
     * @return never returns; always throws an exception
     * @throws MapperException always
     */
    protected static <T> T invalidEmptyList() {
        return invalid("empty list");
    }

    /**
     * Throw a MapperException indicating a required field is missing.
     *
     * @param <T> the expected return type (never actually returned)
     * @return never returns; always throws an exception
     * @throws MapperException always
     */
    protected static <T> T invalidMissingField() {
        return invalid("missing required field");
    }

    /**
     * Throw a MapperException indicating an unexpected null value was encountered.
     *
     * @param <T> the expected return type (never actually returned)
     * @return never returns; always throws an exception
     * @throws MapperException always
     */
    protected static <T> T invalidUnexpectedNull() {
        return invalid("unexpected null");
    }

    /**
     * Optionally extract and transform a value from an object.
     *
     * @param <C1> the type of the source object
     * @param <C2> the intermediate type returned by the accessor
     * @param <T> the final result type
     * @param c1 the source object
     * @param accessor the function to extract the intermediate value
     * @param constructor the function to transform the intermediate value to the final type
     * @return an Optional containing the result, or empty if the intermediate value is null
     */
    protected static <C1, C2, T> Maybe<T> optional(C1 c1, Function<C1, C2> accessor, Function<C2, T> constructor) {
        C2 c2 = accessor.apply(c1);
        return c2 == null ? Maybe.nothing() : Maybe.just(constructor.apply(c2));
    }

    /**
     * Optionally extract and transform a value from a list at a specific index.
     *
     * @param <C1> the type of the source object
     * @param <C2> the type of elements in the list
     * @param <T> the final result type
     * @param c1 the source object
     * @param index the index of the element to extract
     * @param accessor the function to extract the list
     * @param constructor the function to transform the element to the final type
     * @return an Optional containing the result, or empty if the list is null or the index is out of bounds
     */
    protected static <C1, C2, T> Maybe<T> optional(
            C1 c1, int index, Function<C1, List<C2>> accessor, Function<C2, T> constructor) {
        List<C2> c2 = accessor.apply(c1);
        return null == c2 || index >= c2.size() ? Maybe.nothing() : Maybe.just(constructor.apply(c2.get(index)));
    }

    /**
     * Extract and transform a required value from an object, throwing an exception if missing.
     *
     * @param <C1> the type of the source object
     * @param <C2> the intermediate type returned by the accessor
     * @param <T> the final result type
     * @param c1 the source object
     * @param accessor the function to extract the intermediate value
     * @param constructor the function to transform the intermediate value to the final type
     * @return the transformed result
     * @throws MapperException if the intermediate value is null
     */
    protected static <C1, C2, T> T required(C1 c1, Function<C1, C2> accessor, Function<C2, T> constructor) {
        Maybe<T> t = optional(c1, accessor, constructor);
        return t.orElseGet(() -> invalid("missing required field"));
    }

    /**
     * Extract and transform a required value from a list at a specific index, throwing an exception if missing.
     *
     * @param <C1> the type of the source object
     * @param <C2> the type of elements in the list
     * @param <T> the final result type
     * @param c1 the source object
     * @param index the index of the element to extract
     * @param accessor the function to extract the list
     * @param constructor the function to transform the element to the final type
     * @return the transformed result
     * @throws MapperException if the list is null or the index is out of bounds
     */
    protected static <C1, C2, T> T required(
            C1 c1, int index, Function<C1, List<C2>> accessor, Function<C2, T> constructor) {
        Maybe<T> t = optional(c1, index, accessor, constructor);
        return t.orElseGet(() -> invalid("missing required field"));
    }

    /**
     * Extract and transform a list of values from an object, returning an empty list if not present.
     *
     * @param <C1> the type of the source object
     * @param <C2> the type of elements in the source list
     * @param <T> the type of elements in the result list
     * @param c1 the source object
     * @param accessor the function to extract the list
     * @param constructor the function to transform each element
     * @return the transformed list, or an empty list if the source list is null
     * @throws MapperException if any transformed element is null
     */
    protected static <C1, C2, T> List<T> list(C1 c1, Function<C1, List<C2>> accessor, Function<C2, T> constructor) {
        Maybe<List<T>> result = optional(c1, accessor, c2s -> {
            List<T> ts = new ArrayList<>();
            for (C2 c2 : c2s) {
                T t = constructor.apply(c2);
                if (null == t) {
                    return invalidUnexpectedNull();
                }
                ts.add(t);
            }
            return ts;
        });

        // Always return a list, even if empty
        return result.orElseGet(ArrayList::new);
    }

    /**
     * Apply a function to each element in a list, producing a new list.
     *
     * @param <A> the type of elements in the input list
     * @param <B> the type of elements in the output list
     * @param list the input list
     * @param f the function to apply to each element
     * @return a new list containing the transformed elements
     */
    protected static <A, B> List<B> map(List<A> list, Function<A, B> f) {
        return list.stream().map(f).collect(Collectors.toList());
    }

    /**
     * Extract and transform a non-empty list of values from an object, throwing an exception if empty.
     *
     * @param <C1> the type of the source object
     * @param <C2> the type of elements in the source list
     * @param <T> the type of elements in the result list
     * @param c1 the source object
     * @param accessor the function to extract the list
     * @param constructor the function to transform each element
     * @return the transformed non-empty list
     * @throws MapperException if the list is empty or any transformed element is null
     */
    protected static <C1, C2, T> List<T> nonemptyList(
            C1 c1, Function<C1, List<C2>> accessor, Function<C2, T> constructor) {
        List<T> ts = list(c1, accessor, constructor);
        if (ts.isEmpty()) {
            return invalid("empty list");
        }
        return ts;
    }

    /**
     * Create a function that matches a case by optionally extracting and transforming a value.
     *
     * @param <C0> the type extracted from the context
     * @param <P0> the type of the context
     * @param <C> the intermediate child type
     * @param <P> the parent result type
     * @param getter the function to extract from the context
     * @param childConstructor the function to construct the child type
     * @param parentConstructor the function to construct the parent type
     * @return a function that returns an Optional parent object
     */
    protected static <C0, P0, C, P> Function<P0, Maybe<P>> matchCase(
            Function<P0, C0> getter,
            Function<C0, C> childConstructor,
            Function<C, P> parentConstructor) {
        return ctx -> optional(ctx, getter, childConstructor).map(parentConstructor);
    }

    /**
     * Create a function that matches a case by checking if a value is present and returning a constant.
     *
     * @param <C0> the type extracted from the context
     * @param <P0> the type of the context
     * @param <P> the parent result type
     * @param getter the function to extract from the context
     * @param parent the constant parent object to return if the extracted value is non-null
     * @return a function that returns an Optional parent object
     */
    protected static <C0, P0, P> Function<P0, Maybe<P>> matchCase(
            Function<P0, C0> getter,
            P parent) {
        return ctx -> null == getter.apply(ctx) ? Maybe.nothing() : Maybe.just(parent);
    }

    /**
     * Throw an UnsupportedOperationException with a default message.
     *
     * @param <T> the expected return type (never actually returned)
     * @return never returns; always throws an exception
     * @throws UnsupportedOperationException always
     */
    protected static <T> T unsupported() {
        return unsupported("not yet implemented");
    }

    /**
     * Throw an UnsupportedOperationException with the given message.
     *
     * @param <T> the expected return type (never actually returned)
     * @param message the error message
     * @return never returns; always throws an exception
     * @throws UnsupportedOperationException always
     */
    protected static <T> T unsupported(String message) {
        throw new UnsupportedOperationException(message);
    }

    /**
     * Exception thrown by mapper operations when invalid data is encountered.
     */
    public static class MapperException extends RuntimeException {
        /**
         * Construct a MapperException with the given message.
         *
         * @param message the error message
         */
        public MapperException(String message) {
            super(message);
        }

        /**
         * Construct a MapperException with the given cause.
         *
         * @param cause the underlying cause of this exception
         */
        public MapperException(Throwable cause) {
            super(cause);
        }
    }
}

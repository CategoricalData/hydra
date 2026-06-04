package hydra.util;

import java.util.function.Function;

/**
 * A simple class for disjoint unions (Either types) in Java.
 * Represents a value that can be either a Left or a Right.
 *
 * @param <L> the left type
 * @param <R> the right type
 */
public abstract class Either<L, R> {
    private Either() {
    }

    /**
     * Create a Left value.
     *
     * @param <L> the left type
     * @param <R> the right type
     * @param value the value to wrap
     * @return an Either containing the left value
     */
    public static <L, R> Either<L, R> left(L value) {
        return new Left<>(value);
    }

    /**
     * Create a Right value.
     *
     * @param <L> the left type
     * @param <R> the right type
     * @param value the value to wrap
     * @return an Either containing the right value
     */
    public static <L, R> Either<L, R> right(R value) {
        return new Right<>(value);
    }

    /**
     * Check if this is a Left value.
     *
     * @return true if this is a Left value, false otherwise
     */
    public abstract boolean isLeft();

    /**
     * Check if this is a Right value.
     *
     * @return true if this is a Right value, false otherwise
     */
    public abstract boolean isRight();

    /**
     * Accept a visitor for pattern matching on Either values.
     *
     * @param <T> the return type of the visitor
     * @param visitor the visitor to accept
     * @return the result of applying the visitor
     */
    public abstract <T> T accept(Visitor<L, R, T> visitor);

    /**
     * Returns the Right value, or throws an exception mapped from the Left value.
     *
     * @param <X> the exception type
     * @param exceptionMapper a function that converts a Left value to a RuntimeException
     * @return the Right value
     * @throws X if this is a Left value
     */
    public <X extends RuntimeException> R getOrThrow(Function<L, X> exceptionMapper) {
        return accept(new Visitor<L, R, R>() {
            @Override
            public R visit(Left<L, R> instance) {
                throw exceptionMapper.apply(instance.value);
            }

            @Override
            public R visit(Right<L, R> instance) {
                return instance.value;
            }
        });
    }

    /**
     * Applies a function to the Right value if present.
     *
     * @param <R2> the new Right type
     * @param mapper the function to apply
     * @return an Either with the mapped Right value, or the original Left
     */
    @SuppressWarnings("unchecked")
    public <R2> Either<L, R2> map(Function<R, R2> mapper) {
        return accept(new Visitor<L, R, Either<L, R2>>() {
            @Override
            public Either<L, R2> visit(Left<L, R> instance) {
                return (Either<L, R2>) instance;
            }

            @Override
            public Either<L, R2> visit(Right<L, R> instance) {
                return right(mapper.apply(instance.value));
            }
        });
    }

    /**
     * Visitor interface for pattern matching on Either values.
     *
     * @param <T> the return type
     */
    public interface Visitor<L, R, T> {
        /**
         * Visit a Left value.
         *
         * @param instance the Left instance
         * @return the result
         */
        T visit(Left<L, R> instance);

        /**
         * Visit a Right value.
         *
         * @param instance the Right instance
         * @return the result
         */
        T visit(Right<L, R> instance);
    }

    /**
     * Partial visitor interface with a default case for non-exhaustive matching.
     *
     * @param <T> the return type
     */
    public interface PartialVisitor<L, R, T> extends Visitor<L, R, T> {
        /**
         * Default case for non-exhaustive patterns.
         *
         * @param instance the Either instance
         * @return the result
         */
        default T otherwise(Either<?, ?> instance) {
            throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
        }

        @Override
        default T visit(Left<L, R> instance) {
            return otherwise(instance);
        }

        @Override
        default T visit(Right<L, R> instance) {
            return otherwise(instance);
        }
    }

    /**
     * A Left value in an Either.
     *
     * @param <L> the left type
     * @param <R> the right type
     */
    public static final class Left<L, R> extends Either<L, R> {
        public final L value;

        /**
         * Constructs a Left value.
         *
         * @param value the value to wrap
         */
        public Left(L value) {
            this.value = value;
        }

        @Override
        public boolean isLeft() {
            return true;
        }

        @Override
        public boolean isRight() {
            return false;
        }

        @Override
        public <T> T accept(Visitor<L, R, T> visitor) {
            return visitor.visit(this);
        }

        /**
         * Checks if this Left value is equal to another object.
         *
         * @param other the object to compare to
         * @return true if the objects are equal, false otherwise
         */
        @Override
        public boolean equals(Object other) {
            if (!(other instanceof Left)) {
                return false;
            }
            Left<?, ?> o = (Left<?, ?>) other;
            return value.equals(o.value);
        }

        /**
         * Returns the hash code of this Left value.
         *
         * @return the hash code
         */
        @Override
        public int hashCode() {
            return value.hashCode();
        }
    }

    /**
     * A Right value in an Either.
     *
     * @param <L> the left type
     * @param <R> the right type
     */
    public static final class Right<L, R> extends Either<L, R> {
        public final R value;

        /**
         * Constructs a Right value.
         *
         * @param value the value to wrap
         */
        public Right(R value) {
            this.value = value;
        }

        @Override
        public boolean isLeft() {
            return false;
        }

        @Override
        public boolean isRight() {
            return true;
        }

        @Override
        public <T> T accept(Visitor<L, R, T> visitor) {
            return visitor.visit(this);
        }

        /**
         * Checks if this Right value is equal to another object.
         *
         * @param other the object to compare to
         * @return true if the objects are equal, false otherwise
         */
        @Override
        public boolean equals(Object other) {
            if (!(other instanceof Right)) {
                return false;
            }
            Right<?, ?> o = (Right<?, ?>) other;
            return value.equals(o.value);
        }

        /**
         * Returns the hash code of this Right value.
         *
         * @return the hash code
         */
        @Override
        public int hashCode() {
            return value.hashCode();
        }
    }
}

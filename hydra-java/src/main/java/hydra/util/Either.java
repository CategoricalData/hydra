package hydra.util;

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

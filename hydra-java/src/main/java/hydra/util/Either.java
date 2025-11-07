package hydra.util;

/**
 * A simple class for disjoint unions (Either types) in Java.
 * Represents a value that can be either a Left or a Right.
 */
public abstract class Either<L, R> {
    private Either() {
    }

    /**
     * Create a Left value
     */
    public static <L, R> Either<L, R> left(L value) {
        return new Left<>(value);
    }

    /**
     * Create a Right value
     */
    public static <L, R> Either<L, R> right(R value) {
        return new Right<>(value);
    }

    /**
     * Check if this is a Left value
     */
    public abstract boolean isLeft();

    /**
     * Check if this is a Right value
     */
    public abstract boolean isRight();

    /**
     * A Left value in an Either
     */
    public static final class Left<L, R> extends Either<L, R> {
        public final L value;

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
        public boolean equals(Object other) {
            if (!(other instanceof Left)) {
                return false;
            }
            Left<?, ?> o = (Left<?, ?>) other;
            return value.equals(o.value);
        }

        @Override
        public int hashCode() {
            return value.hashCode();
        }
    }

    /**
     * A Right value in an Either
     */
    public static final class Right<L, R> extends Either<L, R> {
        public final R value;

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
        public boolean equals(Object other) {
            if (!(other instanceof Right)) {
                return false;
            }
            Right<?, ?> o = (Right<?, ?>) other;
            return value.equals(o.value);
        }

        @Override
        public int hashCode() {
            return value.hashCode();
        }
    }
}

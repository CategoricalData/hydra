package hydra.util;

/**
 * A simple class for pairs in Java.
 * Unlike Tuple2, this is specifically for the Pair type variant.
 *
 * @param <A> the type of the first element
 * @param <B> the type of the second element
 */
public class Pair<A, B> {
    public final A first;
    public final B second;

    /**
     * Constructs a new Pair.
     *
     * @param first the first element
     * @param second the second element
     */
    public Pair(A first, B second) {
        this.first = first;
        this.second = second;
    }

    /**
     * Checks if this Pair is equal to another object.
     *
     * @param other the object to compare to
     * @return true if the objects are equal, false otherwise
     */
    @Override
    public boolean equals(Object other) {
        if (!(other instanceof Pair)) {
            return false;
        }
        Pair<?, ?> o = (Pair<?, ?>) other;
        return first.equals(o.first) && second.equals(o.second);
    }

    /**
     * Returns the hash code of this Pair.
     *
     * @return the hash code
     */
    @Override
    public int hashCode() {
        return 31 * first.hashCode() + second.hashCode();
    }
}

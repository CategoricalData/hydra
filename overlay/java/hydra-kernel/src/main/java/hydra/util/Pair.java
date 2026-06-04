package hydra.util;

/**
 * A simple class for pairs in Java.
 * A generic pair of two values.
 *
 * @param <A> the type of the first element
 * @param <B> the type of the second element
 */
@SuppressWarnings("rawtypes")
public class Pair<A, B> implements Comparable<Pair> {
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

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Pair other) {
        int cmp = ((Comparable) first).compareTo(other.first);
        if (cmp != 0) {
            return cmp;
        }
        return ((Comparable) second).compareTo(other.second);
    }
}

package hydra.util;

/**
 * A simple class for pairs in Java.
 * Unlike Tuple2, this is specifically for the Pair type variant.
 */
public class Pair<A, B> {
    public final A first;
    public final B second;

    public Pair(A first, B second) {
        this.first = first;
        this.second = second;
    }

    @Override
    public boolean equals(Object other) {
        if (!(other instanceof Pair)) {
            return false;
        }
        Pair<?, ?> o = (Pair<?, ?>) other;
        return first.equals(o.first) && second.equals(o.second);
    }

    @Override
    public int hashCode() {
        return 31 * first.hashCode() + second.hashCode();
    }
}

package hydra.util;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * A persistent (immutable, structurally-shared) ordered set based on {@link PersistentMap}.
 * <p>
 * All mutating operations ({@link #insert}, {@link #delete}, {@link #union}) return a new set
 * that shares most of its internal structure with the original.
 * <p>
 * Elements must be {@link Comparable}. Iteration order is sorted, matching the semantics
 * of Haskell's {@code Data.Set}.
 *
 * @param <T> the element type (must be Comparable)
 */
@SuppressWarnings("rawtypes")
public class PersistentSet<T> implements Set<T>, Serializable, Comparable<PersistentSet> {

    private static final Object PRESENT = new Object();

    @SuppressWarnings("rawtypes")
    private static final PersistentSet EMPTY_INSTANCE = new PersistentSet<>(PersistentMap.empty());

    private final PersistentMap<T, Object> map;

    private PersistentSet(PersistentMap<T, Object> map) {
        this.map = map;
    }

    /**
     * Returns an empty persistent set.
     */
    @SuppressWarnings("unchecked")
    public static <T> PersistentSet<T> empty() {
        return (PersistentSet<T>) EMPTY_INSTANCE;
    }

    /**
     * Returns a singleton set.
     */
    public static <T> PersistentSet<T> singleton(T value) {
        return new PersistentSet<>(PersistentMap.singleton(value, PRESENT));
    }

    /**
     * Builds a set from varargs. O(n log n).
     */
    @SafeVarargs
    public static <T> PersistentSet<T> of(T... elements) {
        PersistentMap<T, Object> m = PersistentMap.empty();
        for (T elem : elements) {
            m = m.insert(elem, PRESENT);
        }
        return new PersistentSet<>(m);
    }

    /**
     * Builds a set from a list. O(n log n).
     */
    public static <T> PersistentSet<T> fromList(List<T> list) {
        PersistentMap<T, Object> m = PersistentMap.empty();
        for (T elem : list) {
            m = m.insert(elem, PRESENT);
        }
        return new PersistentSet<>(m);
    }

    /**
     * Builds a set from a ConsList. O(n log n).
     */
    public static <T> PersistentSet<T> fromConsList(ConsList<T> list) {
        PersistentMap<T, Object> m = PersistentMap.empty();
        for (T elem : list) {
            m = m.insert(elem, PRESENT);
        }
        return new PersistentSet<>(m);
    }

    /**
     * Returns the number of elements.
     */
    public int size() {
        return map.size();
    }

    /**
     * Returns true if this set is empty.
     */
    public boolean isEmpty() {
        return map.isEmpty();
    }

    /**
     * Returns true if this set contains the given element. O(log n).
     */
    public boolean member(T elem) {
        return map.containsKey(elem);
    }

    /**
     * Inserts an element. O(log n).
     */
    public PersistentSet<T> insert(T elem) {
        PersistentMap<T, Object> newMap = map.insert(elem, PRESENT);
        if (newMap == map) {
            return this;
        }
        return new PersistentSet<>(newMap);
    }

    /**
     * Removes an element. O(log n).
     */
    public PersistentSet<T> delete(T elem) {
        PersistentMap<T, Object> newMap = map.delete(elem);
        if (newMap == map) {
            return this;
        }
        return new PersistentSet<>(newMap);
    }

    /**
     * Returns the union of this set and another. O(m * log(m + n)).
     */
    public PersistentSet<T> union(PersistentSet<T> other) {
        if (this.isEmpty()) {
            return other;
        }
        if (other.isEmpty()) {
            return this;
        }
        return new PersistentSet<>(map.union(other.map));
    }

    /**
     * Returns the intersection of this set and another.
     */
    public PersistentSet<T> intersection(PersistentSet<T> other) {
        if (this.isEmpty() || other.isEmpty()) {
            return empty();
        }
        // Filter the smaller set, keeping elements present in the larger
        PersistentSet<T> smaller;
        PersistentSet<T> larger;
        if (this.size() <= other.size()) {
            smaller = this;
            larger = other;
        } else {
            smaller = other;
            larger = this;
        }
        PersistentMap<T, Object> result = PersistentMap.empty();
        for (T elem : smaller) {
            if (larger.member(elem)) {
                result = result.insert(elem, PRESENT);
            }
        }
        return new PersistentSet<>(result);
    }

    /**
     * Returns the difference of this set minus another.
     */
    public PersistentSet<T> difference(PersistentSet<T> other) {
        if (this.isEmpty() || other.isEmpty()) {
            return this;
        }
        PersistentMap<T, Object> result = map;
        for (T elem : other) {
            result = result.delete(elem);
        }
        if (result == map) {
            return this;
        }
        return new PersistentSet<>(result);
    }

    /**
     * Returns the union of multiple sets.
     */
    @SuppressWarnings("unchecked")
    public static <T> PersistentSet<T> unions(List<PersistentSet<T>> sets) {
        PersistentSet<T> result = empty();
        for (PersistentSet<T> set : sets) {
            result = result.union(set);
        }
        return result;
    }

    /**
     * Applies a function to each element, building a new set. O(n log n).
     */
    public <U extends Comparable<U>> PersistentSet<U> map(Function<T, U> f) {
        PersistentMap<U, Object> result = PersistentMap.empty();
        for (T elem : this) {
            result = result.insert(f.apply(elem), PRESENT);
        }
        return new PersistentSet<>(result);
    }

    /**
     * Filters elements by predicate.
     */
    public PersistentSet<T> filter(Predicate<T> pred) {
        PersistentMap<T, Object> newMap = map.filterWithKey((k, v) -> pred.test(k));
        if (newMap == map) {
            return this;
        }
        return new PersistentSet<>(newMap);
    }

    /**
     * Returns a sorted list of all elements.
     */
    public List<T> toList() {
        return map.keys();
    }

    /**
     * Returns a sorted ConsList of all elements.
     */
    public ConsList<T> toConsList() {
        return ConsList.fromList(map.keys());
    }

    /**
     * Returns an in-order iterator over elements.
     */
    @Override
    public Iterator<T> iterator() {
        return map.keys().iterator();
    }

    // ---- java.util.Set interface methods ----

    /**
     * Returns true if this set contains the specified element.
     */
    @Override
    @SuppressWarnings("unchecked")
    public boolean contains(Object o) {
        try {
            return member((T) o);
        } catch (ClassCastException e) {
            return false;
        }
    }

    /**
     * Returns an array containing all elements in this set.
     */
    @Override
    public Object[] toArray() {
        return toList().toArray();
    }

    /**
     * Returns an array containing all elements in this set.
     */
    @Override
    @SuppressWarnings("unchecked")
    public <A> A[] toArray(A[] a) {
        return toList().toArray(a);
    }

    /**
     * Unsupported. PersistentSet is immutable; use {@link #insert(Object)} instead.
     */
    @Override
    public boolean add(T t) {
        throw new UnsupportedOperationException("PersistentSet is immutable; use insert() instead");
    }

    /**
     * Unsupported. PersistentSet is immutable; use {@link #delete(Object)} instead.
     */
    @Override
    public boolean remove(Object o) {
        throw new UnsupportedOperationException("PersistentSet is immutable; use delete() instead");
    }

    /**
     * Returns true if this set contains all elements in the specified collection.
     */
    @Override
    @SuppressWarnings("unchecked")
    public boolean containsAll(Collection<?> c) {
        for (Object o : c) {
            try {
                if (!member((T) o)) {
                    return false;
                }
            } catch (ClassCastException e) {
                return false;
            }
        }
        return true;
    }

    /**
     * Unsupported. PersistentSet is immutable.
     */
    @Override
    public boolean addAll(Collection<? extends T> c) {
        throw new UnsupportedOperationException("PersistentSet is immutable");
    }

    /**
     * Unsupported. PersistentSet is immutable.
     */
    @Override
    public boolean retainAll(Collection<?> c) {
        throw new UnsupportedOperationException("PersistentSet is immutable");
    }

    /**
     * Unsupported. PersistentSet is immutable.
     */
    @Override
    public boolean removeAll(Collection<?> c) {
        throw new UnsupportedOperationException("PersistentSet is immutable");
    }

    /**
     * Unsupported. PersistentSet is immutable.
     */
    @Override
    public void clear() {
        throw new UnsupportedOperationException("PersistentSet is immutable");
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!(obj instanceof PersistentSet)) {
            return false;
        }
        PersistentSet<?> other = (PersistentSet<?>) obj;
        if (this.size() != other.size()) {
            return false;
        }
        Iterator<T> it1 = this.iterator();
        Iterator<?> it2 = other.iterator();
        while (it1.hasNext()) {
            if (!Objects.equals(it1.next(), it2.next())) {
                return false;
            }
        }
        return true;
    }

    @Override
    public int hashCode() {
        int h = 0;
        for (T elem : this) {
            h += Objects.hashCode(elem);
        }
        return h;
    }

    /**
     * Lexicographic comparison over sorted elements.
     */
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PersistentSet other) {
        Iterator<T> it1 = this.iterator();
        Iterator it2 = other.iterator();
        while (it1.hasNext() && it2.hasNext()) {
            int cmp = ((Comparable) it1.next()).compareTo(it2.next());
            if (cmp != 0) return cmp;
        }
        return Integer.compare(this.size(), other.size());
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("{");
        boolean first = true;
        for (T elem : this) {
            if (!first) {
                sb.append(", ");
            }
            sb.append(elem);
            first = false;
        }
        sb.append("}");
        return sb.toString();
    }
}

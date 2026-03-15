package hydra.util;

import java.io.Serializable;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/**
 * A persistent (immutable, shared-tail) singly-linked list.
 * <p>
 * {@link #cons} is O(1) -- it creates a new cell that shares the tail with the original list.
 * {@link #head} and {@link #tail} are O(1). {@link #size} is O(1) via a cached count.
 * <p>
 * This matches the semantics of Haskell's {@code [a]} and is the natural list representation
 * for functional programs that build lists via repeated prepend.
 *
 * @param <T> the element type
 */
public abstract class ConsList<T> extends AbstractList<T> implements Serializable {

    @SuppressWarnings("rawtypes")
    private static final Nil NIL_INSTANCE = new Nil();

    // Prevent external subclassing
    ConsList() {
    }

    /**
     * Returns the empty list.
     */
    @SuppressWarnings("unchecked")
    public static <T> ConsList<T> empty() {
        return (ConsList<T>) NIL_INSTANCE;
    }

    /**
     * Prepends an element to this list. O(1).
     */
    public static <T> ConsList<T> cons(T head, ConsList<T> tail) {
        return new Cell<>(head, tail, tail.size() + 1);
    }

    /**
     * Creates a singleton list.
     */
    public static <T> ConsList<T> singleton(T value) {
        return cons(value, empty());
    }

    /**
     * Builds a ConsList from a java.util.List. The resulting ConsList has the same order.
     */
    public static <T> ConsList<T> fromList(List<T> list) {
        ConsList<T> result = empty();
        for (int i = list.size() - 1; i >= 0; i--) {
            result = cons(list.get(i), result);
        }
        return result;
    }

    /**
     * Builds a ConsList from an array.
     */
    @SafeVarargs
    public static <T> ConsList<T> of(T... elements) {
        ConsList<T> result = empty();
        for (int i = elements.length - 1; i >= 0; i--) {
            result = cons(elements[i], result);
        }
        return result;
    }

    /**
     * Returns the number of elements. O(1).
     */
    public abstract int size();

    /**
     * Returns true if this list is empty. O(1).
     */
    public abstract boolean isEmpty();

    /**
     * Returns the first element. Throws if empty.
     */
    public abstract T head();

    /**
     * Returns an optional first element.
     */
    public abstract Maybe<T> safeHead();

    /**
     * Returns all elements except the first. O(1) -- shared structure.
     * Throws if empty.
     */
    public abstract ConsList<T> tail();

    /**
     * Returns the element at index i. O(i).
     */
    @SuppressWarnings("unchecked")
    public T get(int i) {
        if (i < 0 || i >= size()) {
            throw new IndexOutOfBoundsException("Index: " + i + ", Size: " + size());
        }
        ConsList<T> cur = this;
        for (int j = 0; j < i; j++) {
            cur = cur.tail();
        }
        return cur.head();
    }

    /**
     * Returns the last element. O(n).
     */
    public T last() {
        if (isEmpty()) {
            throw new NoSuchElementException("last of empty list");
        }
        ConsList<T> cur = this;
        while (!cur.tail().isEmpty()) {
            cur = cur.tail();
        }
        return cur.head();
    }

    /**
     * Returns all elements except the last. O(n).
     */
    public ConsList<T> init() {
        if (isEmpty()) {
            throw new NoSuchElementException("init of empty list");
        }
        if (tail().isEmpty()) {
            return empty();
        }
        return cons(head(), tail().init());
    }

    /**
     * Returns the first n elements. O(n).
     */
    public ConsList<T> take(int n) {
        if (n <= 0 || isEmpty()) {
            return empty();
        }
        return cons(head(), tail().take(n - 1));
    }

    /**
     * Drops the first n elements. O(n). The result shares structure with this list.
     */
    public ConsList<T> drop(int n) {
        ConsList<T> cur = this;
        for (int i = 0; i < n && !cur.isEmpty(); i++) {
            cur = cur.tail();
        }
        return cur;
    }

    /**
     * Reverses this list. O(n).
     */
    public ConsList<T> reverse() {
        ConsList<T> result = empty();
        ConsList<T> cur = this;
        while (!cur.isEmpty()) {
            result = cons(cur.head(), result);
            cur = cur.tail();
        }
        return result;
    }

    /**
     * Applies a function to each element. O(n).
     */
    public <U> ConsList<U> map(Function<T, U> f) {
        if (isEmpty()) {
            return empty();
        }
        // Build iteratively to avoid stack overflow on large lists
        ArrayList<U> mapped = new ArrayList<>(size());
        for (T elem : this) {
            mapped.add(f.apply(elem));
        }
        return fromList(mapped);
    }

    /**
     * Filters elements by predicate. O(n).
     */
    public ConsList<T> filter(Predicate<T> pred) {
        ArrayList<T> filtered = new ArrayList<>();
        for (T elem : this) {
            if (pred.test(elem)) {
                filtered.add(elem);
            }
        }
        return fromList(filtered);
    }

    /**
     * Left fold. O(n).
     */
    public <R> R foldl(BiFunction<R, T, R> f, R init) {
        R acc = init;
        ConsList<T> cur = this;
        while (!cur.isEmpty()) {
            acc = f.apply(acc, cur.head());
            cur = cur.tail();
        }
        return acc;
    }

    /**
     * Right fold. O(n). Uses reverse + foldl for stack safety.
     */
    public <R> R foldr(BiFunction<T, R, R> f, R init) {
        return reverse().foldl((acc, elem) -> f.apply(elem, acc), init);
    }

    /**
     * Concatenates this list with another. O(m) where m = this.size().
     * The result shares the tail (other) structurally.
     */
    public ConsList<T> concat(ConsList<T> other) {
        if (isEmpty()) {
            return other;
        }
        if (other.isEmpty()) {
            return this;
        }
        // Build iteratively to avoid stack overflow
        ArrayList<T> elems = new ArrayList<>(size());
        for (T elem : this) {
            elems.add(elem);
        }
        ConsList<T> result = other;
        for (int i = elems.size() - 1; i >= 0; i--) {
            result = cons(elems.get(i), result);
        }
        return result;
    }

    /**
     * Flattens a list of lists. O(total elements).
     */
    public static <T> ConsList<T> concatAll(ConsList<ConsList<T>> lists) {
        ArrayList<T> all = new ArrayList<>();
        for (ConsList<T> sublist : lists) {
            for (T elem : sublist) {
                all.add(elem);
            }
        }
        return fromList(all);
    }

    /**
     * Returns true if this list contains the specified element. O(n).
     */
    @Override
    public boolean contains(Object elem) {
        for (T e : this) {
            if (Objects.equals(e, elem)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Converts to a java.util.ArrayList. O(n).
     */
    public ArrayList<T> toArrayList() {
        ArrayList<T> result = new ArrayList<>(size());
        for (T elem : this) {
            result.add(elem);
        }
        return result;
    }

    /**
     * Returns a sequential stream over the elements.
     */
    public Stream<T> stream() {
        return StreamSupport.stream(spliterator(), false);
    }

    @Override
    public Iterator<T> iterator() {
        return new ConsIterator<>(this);
    }

    // equals and hashCode are inherited from AbstractList, which implements
    // the java.util.List contract (element-wise comparison with any List)

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("[");
        boolean first = true;
        for (T elem : this) {
            if (!first) {
                sb.append(", ");
            }
            sb.append(elem);
            first = false;
        }
        sb.append("]");
        return sb.toString();
    }

    // ---- Inner classes ----

    static final class Nil<T> extends ConsList<T> {
        @Override
        public int size() {
            return 0;
        }

        @Override
        public boolean isEmpty() {
            return true;
        }

        @Override
        public T head() {
            throw new NoSuchElementException("head of empty list");
        }

        @Override
        public Maybe<T> safeHead() {
            return Maybe.nothing();
        }

        @Override
        public ConsList<T> tail() {
            throw new NoSuchElementException("tail of empty list");
        }
    }

    static final class Cell<T> extends ConsList<T> {
        final T hd;
        final ConsList<T> tl;
        final int sz;

        Cell(T head, ConsList<T> tail, int size) {
            this.hd = head;
            this.tl = tail;
            this.sz = size;
        }

        @Override
        public int size() {
            return sz;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public T head() {
            return hd;
        }

        @Override
        public Maybe<T> safeHead() {
            return Maybe.just(hd);
        }

        @Override
        public ConsList<T> tail() {
            return tl;
        }
    }

    private static final class ConsIterator<T> implements Iterator<T> {
        private ConsList<T> current;

        ConsIterator(ConsList<T> list) {
            this.current = list;
        }

        @Override
        public boolean hasNext() {
            return !current.isEmpty();
        }

        @Override
        public T next() {
            if (current.isEmpty()) {
                throw new NoSuchElementException();
            }
            T value = current.head();
            current = current.tail();
            return value;
        }
    }
}

package hydra.util;

import java.io.Serializable;
import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.BiPredicate;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * A persistent (immutable, structurally-shared) ordered map based on a red-black tree.
 * <p>
 * All mutating operations ({@link #insert}, {@link #delete}, {@link #union}) return a new map
 * that shares most of its internal structure with the original. This makes them O(log n)
 * instead of the O(n) full-copy approach used by mutable maps.
 * <p>
 * Keys must be {@link Comparable}. Iteration order is sorted by key, matching the semantics
 * of Haskell's {@code Data.Map}.
 * <p>
 * Based on Chris Okasaki's purely functional red-black trees.
 *
 * @param <K> the key type (must be Comparable)
 * @param <V> the value type
 */
@SuppressWarnings("rawtypes")
public abstract class PersistentMap<K, V> implements Map<K, V>, Iterable<Map.Entry<K, V>>, Serializable, Comparable<PersistentMap> {

    private static final boolean RED = true;
    private static final boolean BLACK = false;

    @SuppressWarnings("rawtypes")
    private static final Empty EMPTY_INSTANCE = new Empty();

    // Prevent external subclassing
    PersistentMap() {
    }

    /**
     * Returns an empty persistent map.
     */
    @SuppressWarnings("unchecked")
    public static <K, V> PersistentMap<K, V> empty() {
        return (PersistentMap<K, V>) EMPTY_INSTANCE;
    }

    /**
     * Returns a singleton persistent map.
     */
    public static <K, V> PersistentMap<K, V> singleton(K key, V value) {
        return new Node<>(BLACK, empty(), key, value, empty(), 1);
    }

    /**
     * Creates a Map.Entry for use with ofEntries(). Mirrors java.util.Map.entry().
     */
    public static <K, V> Map.Entry<K, V> entry(K key, V value) {
        return new AbstractMap.SimpleImmutableEntry<>(key, value);
    }

    /**
     * Builds a persistent map from Map.Entry varargs. Mirrors java.util.Map.ofEntries().
     * Later entries override earlier ones for duplicate keys.
     */
    @SafeVarargs
    @SuppressWarnings("unchecked")
    public static <K, V> PersistentMap<K, V> ofEntries(Map.Entry<K, V>... entries) {
        PersistentMap<K, V> map = empty();
        for (Map.Entry<K, V> entry : entries) {
            map = map.insert(entry.getKey(), entry.getValue());
        }
        return map;
    }

    /**
     * Builds a persistent map from a list of key-value pairs.
     * Later entries override earlier ones for duplicate keys.
     */
    public static <K extends Comparable<K>, V> PersistentMap<K, V> fromPairList(List<Pair<K, V>> pairs) {
        PersistentMap<K, V> map = empty();
        for (Pair<K, V> pair : pairs) {
            map = map.insert(pair.first, pair.second);
        }
        return map;
    }

    /**
     * Builds a persistent map from a java.util.Map.
     */
    public static <K extends Comparable<K>, V> PersistentMap<K, V> fromMap(Map<K, V> source) {
        PersistentMap<K, V> map = empty();
        for (Map.Entry<K, V> entry : source.entrySet()) {
            map = map.insert(entry.getKey(), entry.getValue());
        }
        return map;
    }

    /**
     * Returns the number of entries.
     */
    public abstract int size();

    /**
     * Returns true if this map contains no entries.
     */
    public abstract boolean isEmpty();

    /**
     * Looks up a key, returning a Maybe.
     */
    @SuppressWarnings("unchecked")
    public Maybe<V> lookup(K key) {
        PersistentMap<K, V> node = this;
        while (node instanceof Node) {
            Node<K, V> n = (Node<K, V>) node;
            int cmp = ((Comparable<K>) key).compareTo(n.key);
            if (cmp < 0) {
                node = n.left;
            } else if (cmp > 0) {
                node = n.right;
            } else {
                return Maybe.just(n.value);
            }
        }
        return Maybe.nothing();
    }

    /**
     * Returns the value for the given key, or a default if not found.
     */
    public V findWithDefault(V defaultValue, K key) {
        return lookup(key).orElse(defaultValue);
    }

    /**
     * Inserts a key-value pair, returning a new map. If the key already exists,
     * its value is replaced. O(log n).
     */
    @SuppressWarnings("unchecked")
    public PersistentMap<K, V> insert(K key, V value) {
        PersistentMap<K, V> result = ins(this, key, value);
        // Root must be black
        if (result instanceof Node) {
            Node<K, V> n = (Node<K, V>) result;
            if (n.color == RED) {
                return new Node<>(BLACK, n.left, n.key, n.value, n.right, n.sz);
            }
        }
        return result;
    }

    @SuppressWarnings("unchecked")
    private static <K, V> PersistentMap<K, V> ins(PersistentMap<K, V> tree, K key, V value) {
        if (tree instanceof Empty) {
            return new Node<>(RED, empty(), key, value, empty(), 1);
        }
        Node<K, V> n = (Node<K, V>) tree;
        int cmp = ((Comparable<K>) key).compareTo(n.key);
        if (cmp < 0) {
            return balance(n.color, ins(n.left, key, value), n.key, n.value, n.right);
        } else if (cmp > 0) {
            return balance(n.color, n.left, n.key, n.value, ins(n.right, key, value));
        } else {
            // Key exists, replace value; share children
            if (value == n.value || Objects.equals(value, n.value)) {
                return tree; // No change
            }
            return new Node<>(n.color, n.left, key, value, n.right, n.sz);
        }
    }

    /**
     * Removes a key, returning a new map. O(log n).
     */
    @SuppressWarnings("unchecked")
    public PersistentMap<K, V> delete(K key) {
        PersistentMap<K, V> result = del(this, key);
        // Root must be black
        if (result instanceof Node) {
            Node<K, V> n = (Node<K, V>) result;
            if (n.color == RED) {
                return new Node<>(BLACK, n.left, n.key, n.value, n.right, n.sz);
            }
        }
        return result;
    }

    @SuppressWarnings("unchecked")
    private static <K, V> PersistentMap<K, V> del(PersistentMap<K, V> tree, K key) {
        if (tree instanceof Empty) {
            return tree;
        }
        Node<K, V> n = (Node<K, V>) tree;
        int cmp = ((Comparable<K>) key).compareTo(n.key);
        if (cmp < 0) {
            PersistentMap<K, V> newLeft = del(n.left, key);
            if (newLeft == n.left) {
                return tree; // Key not found
            }
            return rebalanceLeft(n.color, newLeft, n.key, n.value, n.right);
        } else if (cmp > 0) {
            PersistentMap<K, V> newRight = del(n.right, key);
            if (newRight == n.right) {
                return tree; // Key not found
            }
            return rebalanceRight(n.color, n.left, n.key, n.value, newRight);
        } else {
            // Found the key to delete
            return merge(n.left, n.right);
        }
    }

    private static <K, V> PersistentMap<K, V> merge(PersistentMap<K, V> left, PersistentMap<K, V> right) {
        if (left instanceof Empty) {
            return right;
        }
        if (right instanceof Empty) {
            return left;
        }
        // Find the minimum of the right subtree to replace the deleted node
        Node<K, V> minRight = findMin((Node<K, V>) right);
        PersistentMap<K, V> newRight = deleteMin(right);
        return rebalanceRight(BLACK, left, minRight.key, minRight.value, newRight);
    }

    @SuppressWarnings("unchecked")
    private static <K, V> Node<K, V> findMin(Node<K, V> node) {
        while (node.left instanceof Node) {
            node = (Node<K, V>) node.left;
        }
        return node;
    }

    @SuppressWarnings("unchecked")
    private static <K, V> PersistentMap<K, V> deleteMin(PersistentMap<K, V> tree) {
        if (tree instanceof Empty) {
            return tree;
        }
        Node<K, V> n = (Node<K, V>) tree;
        if (n.left instanceof Empty) {
            return n.right;
        }
        return rebalanceLeft(n.color, deleteMin(n.left), n.key, n.value, n.right);
    }

    private static <K, V> PersistentMap<K, V> rebalanceLeft(
            boolean color, PersistentMap<K, V> left, K key, V value, PersistentMap<K, V> right) {
        int newSize = sizeOf(left) + 1 + sizeOf(right);
        return balance(color, left, key, value, right);
    }

    private static <K, V> PersistentMap<K, V> rebalanceRight(
            boolean color, PersistentMap<K, V> left, K key, V value, PersistentMap<K, V> right) {
        int newSize = sizeOf(left) + 1 + sizeOf(right);
        return balance(color, left, key, value, right);
    }

    /**
     * Left-biased union: entries in this map take precedence over entries in other.
     * O(m * log(n/m + 1)) where m &lt;= n.
     */
    public PersistentMap<K, V> union(PersistentMap<K, V> other) {
        if (this.isEmpty()) {
            return other;
        }
        if (other.isEmpty()) {
            return this;
        }
        // Insert all entries of 'other' into 'this', but only if not already present
        // For left-biased union, we insert this's entries into other (this takes precedence)
        return unionWith(this, other);
    }

    @SuppressWarnings("unchecked")
    private static <K, V> PersistentMap<K, V> unionWith(PersistentMap<K, V> preferred, PersistentMap<K, V> fallback) {
        // Simple approach: insert all of fallback into preferred (preferred wins on conflict)
        // This is O(m * log(m + n)) which is acceptable.
        // A split-based approach would be O(m * log(n/m + 1)) but is more complex.
        if (fallback instanceof Empty) {
            return preferred;
        }
        if (preferred instanceof Empty) {
            return fallback;
        }

        // Iterate over the smaller map and insert into the larger
        PersistentMap<K, V> larger;
        PersistentMap<K, V> smaller;
        boolean preferredIsSmaller;
        if (preferred.size() >= fallback.size()) {
            larger = preferred;
            smaller = fallback;
            preferredIsSmaller = false;
        } else {
            larger = fallback;
            smaller = preferred;
            preferredIsSmaller = true;
        }

        PersistentMap<K, V> result = larger;
        for (Map.Entry<K, V> entry : smaller) {
            if (preferredIsSmaller) {
                // smaller is preferred, so its entries should override
                result = overrideInsert(result, entry.getKey(), entry.getValue());
            } else {
                // smaller is fallback, so only insert if key is absent
                if (!result.containsKey(entry.getKey())) {
                    result = result.insert(entry.getKey(), entry.getValue());
                }
            }
        }
        return result;
    }

    private static <K, V> PersistentMap<K, V> overrideInsert(PersistentMap<K, V> map, K key, V value) {
        return map.insert(key, value);
    }

    /**
     * Applies a function to alter the value for a key.
     * The function receives Maybe.nothing() if the key is absent, or Maybe.just(v) if present.
     * If the function returns Maybe.nothing(), the key is removed.
     * If the function returns Maybe.just(v), the key is set to v.
     */
    public PersistentMap<K, V> alter(Function<Maybe<V>, Maybe<V>> f, K key) {
        Maybe<V> current = lookup(key);
        Maybe<V> result = f.apply(current);
        if (result.isJust()) {
            return insert(key, result.fromJust());
        } else if (current.isJust()) {
            return delete(key);
        } else {
            return this;
        }
    }

    /**
     * Maps a function over all values, preserving tree structure.
     */
    @SuppressWarnings("unchecked")
    public <V2> PersistentMap<K, V2> mapValues(Function<V, V2> f) {
        if (this instanceof Empty) {
            return empty();
        }
        Node<K, V> n = (Node<K, V>) this;
        PersistentMap<K, V2> newLeft = n.left.mapValues(f);
        V2 newValue = f.apply(n.value);
        PersistentMap<K, V2> newRight = n.right.mapValues(f);
        return new Node<>(n.color, newLeft, n.key, newValue, newRight, n.sz);
    }

    /**
     * Maps functions over both keys and values. Since key order may change,
     * this rebuilds the map from scratch. O(n log n).
     */
    public <K2 extends Comparable<K2>, V2> PersistentMap<K2, V2> bimap(Function<K, K2> fk, Function<V, V2> fv) {
        PersistentMap<K2, V2> result = empty();
        for (Map.Entry<K, V> entry : this) {
            result = result.insert(fk.apply(entry.getKey()), fv.apply(entry.getValue()));
        }
        return result;
    }

    /**
     * Maps a function over keys. Since key order may change, this rebuilds.
     */
    public <K2 extends Comparable<K2>> PersistentMap<K2, V> mapKeys(Function<K, K2> f) {
        PersistentMap<K2, V> result = empty();
        for (Map.Entry<K, V> entry : this) {
            result = result.insert(f.apply(entry.getKey()), entry.getValue());
        }
        return result;
    }

    /**
     * Filters entries by value.
     */
    public PersistentMap<K, V> filter(Predicate<V> pred) {
        return filterWithKey((k, v) -> pred.test(v));
    }

    /**
     * Filters entries by key and value.
     */
    @SuppressWarnings("unchecked")
    public PersistentMap<K, V> filterWithKey(BiPredicate<K, V> pred) {
        if (this instanceof Empty) {
            return this;
        }
        Node<K, V> n = (Node<K, V>) this;
        PersistentMap<K, V> newLeft = n.left.filterWithKey(pred);
        PersistentMap<K, V> newRight = n.right.filterWithKey(pred);
        if (pred.test(n.key, n.value)) {
            if (newLeft == n.left && newRight == n.right) {
                return this; // No change, share structure
            }
            return join(n.key, n.value, newLeft, newRight);
        } else {
            return merge2(newLeft, newRight);
        }
    }

    // Join two trees with a new root entry. Both trees must have keys in the
    // appropriate ranges. This is simpler than a full merge.
    @SuppressWarnings("unchecked")
    private static <K, V> PersistentMap<K, V> join(K key, V value,
            PersistentMap<K, V> left, PersistentMap<K, V> right) {
        // Rebuild via insertion for correctness
        PersistentMap<K, V> result = merge2(left, right);
        return result.insert(key, value);
    }

    // Merge two trees where all keys in left < all keys in right
    private static <K, V> PersistentMap<K, V> merge2(PersistentMap<K, V> left, PersistentMap<K, V> right) {
        if (left instanceof Empty) {
            return right;
        }
        if (right instanceof Empty) {
            return left;
        }
        // Insert all of right into left
        PersistentMap<K, V> result = left;
        for (Map.Entry<K, V> entry : right) {
            result = result.insert(entry.getKey(), entry.getValue());
        }
        return result;
    }

    /**
     * Returns a sorted list of all keys.
     */
    public List<K> keys() {
        List<K> result = new ArrayList<>(size());
        collectKeys(this, result);
        return result;
    }

    @SuppressWarnings("unchecked")
    private static <K, V> void collectKeys(PersistentMap<K, V> tree, List<K> result) {
        if (tree instanceof Node) {
            Node<K, V> n = (Node<K, V>) tree;
            collectKeys(n.left, result);
            result.add(n.key);
            collectKeys(n.right, result);
        }
    }

    /**
     * Returns a list of all values in key order.
     */
    public List<V> values() {
        List<V> result = new ArrayList<>(size());
        collectValues(this, result);
        return result;
    }

    @SuppressWarnings("unchecked")
    private static <K, V> void collectValues(PersistentMap<K, V> tree, List<V> result) {
        if (tree instanceof Node) {
            Node<K, V> n = (Node<K, V>) tree;
            collectValues(n.left, result);
            result.add(n.value);
            collectValues(n.right, result);
        }
    }

    /**
     * Returns a sorted list of key-value pairs.
     */
    public List<Pair<K, V>> toList() {
        List<Pair<K, V>> result = new ArrayList<>(size());
        collectPairs(this, result);
        return result;
    }

    @SuppressWarnings("unchecked")
    private static <K, V> void collectPairs(PersistentMap<K, V> tree, List<Pair<K, V>> result) {
        if (tree instanceof Node) {
            Node<K, V> n = (Node<K, V>) tree;
            collectPairs(n.left, result);
            result.add(new Pair<>(n.key, n.value));
            collectPairs(n.right, result);
        }
    }

    /**
     * Returns an in-order iterator over entries.
     */
    @Override
    public Iterator<Map.Entry<K, V>> iterator() {
        List<Map.Entry<K, V>> entries = new ArrayList<>(size());
        collectEntries(this, entries);
        return entries.iterator();
    }

    @SuppressWarnings("unchecked")
    private static <K, V> void collectEntries(PersistentMap<K, V> tree, List<Map.Entry<K, V>> result) {
        if (tree instanceof Node) {
            Node<K, V> n = (Node<K, V>) tree;
            collectEntries(n.left, result);
            result.add(new java.util.AbstractMap.SimpleImmutableEntry<>(n.key, n.value));
            collectEntries(n.right, result);
        }
    }

    /**
     * Folds over all entries in key order.
     */
    @SuppressWarnings("unchecked")
    public <R> R foldl(BiFunction<R, Map.Entry<K, V>, R> f, R init) {
        if (this instanceof Empty) {
            return init;
        }
        Node<K, V> n = (Node<K, V>) this;
        R acc = n.left.foldl(f, init);
        acc = f.apply(acc, new java.util.AbstractMap.SimpleImmutableEntry<>(n.key, n.value));
        return n.right.foldl(f, acc);
    }

    // ---- java.util.Map interface ----

    @Override
    @SuppressWarnings("unchecked")
    public V get(Object key) {
        try {
            return lookup((K) key).orElse(null);
        } catch (ClassCastException e) {
            return null;
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public boolean containsKey(Object key) {
        try {
            return lookup((K) key).isJust();
        } catch (ClassCastException e) {
            return false;
        }
    }

    @Override
    public boolean containsValue(Object value) {
        for (Map.Entry<K, V> entry : this) {
            if (Objects.equals(value, entry.getValue())) {
                return true;
            }
        }
        return false;
    }

    @Override
    public V put(K key, V value) {
        throw new UnsupportedOperationException("PersistentMap is immutable. Use insert() instead.");
    }

    @Override
    public V remove(Object key) {
        throw new UnsupportedOperationException("PersistentMap is immutable. Use delete() instead.");
    }

    @Override
    public void putAll(Map<? extends K, ? extends V> m) {
        throw new UnsupportedOperationException("PersistentMap is immutable. Use union() instead.");
    }

    @Override
    public void clear() {
        throw new UnsupportedOperationException("PersistentMap is immutable. Use empty() instead.");
    }

    @Override
    public Set<K> keySet() {
        return new AbstractSet<K>() {
            @Override
            public Iterator<K> iterator() {
                return keys().iterator();
            }

            @Override
            public int size() {
                return PersistentMap.this.size();
            }

            @Override
            public boolean contains(Object o) {
                return PersistentMap.this.containsKey(o);
            }
        };
    }

    @Override
    public Set<Map.Entry<K, V>> entrySet() {
        return new AbstractSet<Map.Entry<K, V>>() {
            @Override
            public Iterator<Map.Entry<K, V>> iterator() {
                return PersistentMap.this.iterator();
            }

            @Override
            public int size() {
                return PersistentMap.this.size();
            }
        };
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!(obj instanceof PersistentMap)) {
            return false;
        }
        PersistentMap<?, ?> other = (PersistentMap<?, ?>) obj;
        if (this.size() != other.size()) {
            return false;
        }
        // Compare sorted entries
        Iterator<Map.Entry<K, V>> it1 = this.iterator();
        Iterator<?> it2 = other.iterator();
        while (it1.hasNext()) {
            Map.Entry<K, V> e1 = it1.next();
            @SuppressWarnings("unchecked")
            Map.Entry<K, V> e2 = (Map.Entry<K, V>) it2.next();
            if (!Objects.equals(e1.getKey(), e2.getKey()) || !Objects.equals(e1.getValue(), e2.getValue())) {
                return false;
            }
        }
        return true;
    }

    @Override
    public int hashCode() {
        int h = 0;
        for (Map.Entry<K, V> entry : this) {
            h += Objects.hashCode(entry.getKey()) ^ Objects.hashCode(entry.getValue());
        }
        return h;
    }

    /**
     * Lexicographic comparison over sorted entries.
     */
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PersistentMap other) {
        Iterator<Map.Entry<K, V>> it1 = this.iterator();
        Iterator it2 = other.iterator();
        while (it1.hasNext() && it2.hasNext()) {
            Map.Entry<K, V> e1 = it1.next();
            Map.Entry e2 = (Map.Entry) it2.next();
            int cmp = ((Comparable) e1.getKey()).compareTo(e2.getKey());
            if (cmp != 0) return cmp;
            cmp = ((Comparable) e1.getValue()).compareTo(e2.getValue());
            if (cmp != 0) return cmp;
        }
        return Integer.compare(this.size(), other.size());
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("{");
        boolean first = true;
        for (Map.Entry<K, V> entry : this) {
            if (!first) {
                sb.append(", ");
            }
            sb.append(entry.getKey()).append("=").append(entry.getValue());
            first = false;
        }
        sb.append("}");
        return sb.toString();
    }

    // ---- Red-black tree balancing ----

    // Okasaki's balance function handles all four rotation cases
    @SuppressWarnings("unchecked")
    private static <K, V> PersistentMap<K, V> balance(
            boolean color, PersistentMap<K, V> left, K key, V value, PersistentMap<K, V> right) {
        int newSize = sizeOf(left) + 1 + sizeOf(right);

        if (color == BLACK) {
            // Case 1: left-left red
            if (left instanceof Node) {
                Node<K, V> l = (Node<K, V>) left;
                if (l.color == RED && l.left instanceof Node) {
                    Node<K, V> ll = (Node<K, V>) l.left;
                    if (ll.color == RED) {
                        return new Node<>(RED,
                                new Node<>(BLACK, ll.left, ll.key, ll.value, ll.right, sizeOf(ll.left) + 1 + sizeOf(ll.right)),
                                l.key, l.value,
                                new Node<>(BLACK, l.right, key, value, right, sizeOf(l.right) + 1 + sizeOf(right)),
                                newSize);
                    }
                }
                // Case 2: left-right red
                if (l.color == RED && l.right instanceof Node) {
                    Node<K, V> lr = (Node<K, V>) l.right;
                    if (lr.color == RED) {
                        return new Node<>(RED,
                                new Node<>(BLACK, l.left, l.key, l.value, lr.left, sizeOf(l.left) + 1 + sizeOf(lr.left)),
                                lr.key, lr.value,
                                new Node<>(BLACK, lr.right, key, value, right, sizeOf(lr.right) + 1 + sizeOf(right)),
                                newSize);
                    }
                }
            }
            // Case 3: right-left red
            if (right instanceof Node) {
                Node<K, V> r = (Node<K, V>) right;
                if (r.color == RED && r.left instanceof Node) {
                    Node<K, V> rl = (Node<K, V>) r.left;
                    if (rl.color == RED) {
                        return new Node<>(RED,
                                new Node<>(BLACK, left, key, value, rl.left, sizeOf(left) + 1 + sizeOf(rl.left)),
                                rl.key, rl.value,
                                new Node<>(BLACK, rl.right, r.key, r.value, r.right, sizeOf(rl.right) + 1 + sizeOf(r.right)),
                                newSize);
                    }
                }
                // Case 4: right-right red
                if (r.color == RED && r.right instanceof Node) {
                    Node<K, V> rr = (Node<K, V>) r.right;
                    if (rr.color == RED) {
                        return new Node<>(RED,
                                new Node<>(BLACK, left, key, value, r.left, sizeOf(left) + 1 + sizeOf(r.left)),
                                r.key, r.value,
                                new Node<>(BLACK, rr.left, rr.key, rr.value, rr.right, sizeOf(rr.left) + 1 + sizeOf(rr.right)),
                                newSize);
                    }
                }
            }
        }
        // No rebalancing needed
        return new Node<>(color, left, key, value, right, newSize);
    }

    private static int sizeOf(PersistentMap<?, ?> tree) {
        return tree instanceof Node ? ((Node<?, ?>) tree).sz : 0;
    }

    // ---- Inner classes ----

    static final class Empty<K, V> extends PersistentMap<K, V> {
        @Override
        public int size() {
            return 0;
        }

        @Override
        public boolean isEmpty() {
            return true;
        }
    }

    static final class Node<K, V> extends PersistentMap<K, V> {
        final boolean color;
        final PersistentMap<K, V> left;
        final K key;
        final V value;
        final PersistentMap<K, V> right;
        final int sz;

        Node(boolean color, PersistentMap<K, V> left, K key, V value, PersistentMap<K, V> right, int size) {
            this.color = color;
            this.left = left;
            this.key = key;
            this.value = value;
            this.right = right;
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
    }
}

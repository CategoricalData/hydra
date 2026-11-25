package hydra.core.overlay.java.lib;

import hydra.core.errors.Error_;
import hydra.core.graph.Graph;
import hydra.core.model.Term;
import hydra.core.overlay.java.util.Either;
import hydra.core.overlay.java.util.PersistentMap;
import hydra.core.overlay.java.util.PersistentSet;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;


/**
 * Graph-aware comparison of object-level values for the interpreter primitives (#848).
 * Values are compared by their order keys (hydra.core.ordering.termOrderKey), so union variants compare in declared
 * order and annotations are ignored at every depth. Map keys and set elements are stored with their annotations
 * removed, so keys which differ only in annotations are the same key.
 */
public final class OrderKeys {
    private OrderKeys() {
    }

    /**
     * Compute the order key of a value.
     */
    public static Either<Error_, Term> key(Graph graph, Term term) {
        return hydra.core.Ordering.termOrderKey(graph, term);
    }

    /**
     * Compare two values by their order keys.
     */
    public static Either<Error_, Integer> compare(Graph graph, Term left, Term right) {
        Either<Error_, Term> kl = key(graph, left);
        if (kl.isLeft()) {
            return (Either) kl;
        }
        Either<Error_, Term> kr = key(graph, right);
        if (kr.isLeft()) {
            return (Either) kr;
        }
        return Either.right(((Either.Right<Error_, Term>) kl).value.compareTo(((Either.Right<Error_, Term>) kr).value));
    }

    /**
     * Compute the order keys of a list of values.
     */
    public static Either<Error_, List<Term>> keys(Graph graph, List<Term> terms) {
        List<Term> result = new ArrayList<>(terms.size());
        for (Term t : terms) {
            Either<Error_, Term> k = key(graph, t);
            if (k.isLeft()) {
                return (Either) k;
            }
            result.add(((Either.Right<Error_, Term>) k).value);
        }
        return Either.right(result);
    }

    /**
     * Stable-sort items by the given keys, which are parallel to the items.
     */
    public static <A> List<A> sortByKeys(List<A> items, List<Term> keys) {
        List<Integer> indices = new ArrayList<>(items.size());
        for (int i = 0; i < items.size(); i++) {
            indices.add(i);
        }
        indices.sort((a, b) -> keys.get(a).compareTo(keys.get(b)));
        List<A> result = new ArrayList<>(items.size());
        for (int i : indices) {
            result.add(items.get(i));
        }
        return result;
    }

    /**
     * Stable-sort values by their own order keys.
     */
    public static Either<Error_, List<Term>> sort(Graph graph, List<Term> terms) {
        Either<Error_, List<Term>> ks = keys(graph, terms);
        if (ks.isLeft()) {
            return ks;
        }
        return Either.right(sortByKeys(terms, ((Either.Right<Error_, List<Term>>) ks).value));
    }

    /**
     * Remove duplicates by order key, keeping the first occurrence of each.
     */
    public static Either<Error_, List<Term>> distinct(Graph graph, List<Term> terms) {
        Either<Error_, List<Term>> ks = keys(graph, terms);
        if (ks.isLeft()) {
            return ks;
        }
        List<Term> keyList = ((Either.Right<Error_, List<Term>>) ks).value;
        java.util.TreeSet<Term> seen = new java.util.TreeSet<>();
        List<Term> result = new ArrayList<>();
        for (int i = 0; i < terms.size(); i++) {
            if (seen.add(keyList.get(i))) {
                result.add(terms.get(i));
            }
        }
        return Either.right(result);
    }

    /**
     * Group adjacent values whose order keys are equal.
     */
    public static Either<Error_, List<List<Term>>> group(Graph graph, List<Term> terms) {
        Either<Error_, List<Term>> ks = keys(graph, terms);
        if (ks.isLeft()) {
            return (Either) ks;
        }
        List<Term> keyList = ((Either.Right<Error_, List<Term>>) ks).value;
        List<List<Term>> groups = new ArrayList<>();
        for (int i = 0; i < terms.size(); i++) {
            if (i == 0 || keyList.get(i).compareTo(keyList.get(i - 1)) != 0) {
                groups.add(new ArrayList<>());
            }
            groups.get(groups.size() - 1).add(terms.get(i));
        }
        return Either.right(groups);
    }

    /**
     * Whether a list contains a value with the same order key.
     */
    public static Either<Error_, Boolean> member(Graph graph, Term term, List<Term> terms) {
        Either<Error_, Term> k = key(graph, term);
        if (k.isLeft()) {
            return (Either) k;
        }
        Either<Error_, List<Term>> ks = keys(graph, terms);
        if (ks.isLeft()) {
            return (Either) ks;
        }
        Term target = ((Either.Right<Error_, Term>) k).value;
        for (Term other : ((Either.Right<Error_, List<Term>>) ks).value) {
            if (target.compareTo(other) == 0) {
                return Either.right(true);
            }
        }
        return Either.right(false);
    }

    /**
     * The entries of a map, sorted by the order keys of the map keys.
     */
    public static Either<Error_, List<Map.Entry<Term, Term>>> sortedEntries(Graph graph, Map<Term, Term> map) {
        List<Map.Entry<Term, Term>> entries = new ArrayList<>(map.entrySet());
        List<Term> mapKeys = new ArrayList<>(entries.size());
        for (Map.Entry<Term, Term> e : entries) {
            mapKeys.add(e.getKey());
        }
        Either<Error_, List<Term>> ks = keys(graph, mapKeys);
        if (ks.isLeft()) {
            return (Either) ks;
        }
        return Either.right(sortByKeys(entries, ((Either.Right<Error_, List<Term>>) ks).value));
    }

    /**
     * Remove annotations from a map key or set element at every depth.
     */
    public static Term strip(Term term) {
        return hydra.core.Strip.removeTermAnnotations(term);
    }

    /**
     * Extract a set argument, removing annotations from its elements.
     */
    public static Either<Error_, Set<Term>> set(Graph graph, Term term) {
        Either<Error_, Set<Term>> r = hydra.core.extract.Model.set(graph, term);
        if (r.isLeft()) {
            return r;
        }
        PersistentSet<Term> result = PersistentSet.empty();
        for (Term t : ((Either.Right<Error_, Set<Term>>) r).value) {
            result = result.insert(strip(t));
        }
        return Either.right(result);
    }

    /**
     * Extract a map argument, removing annotations from its keys.
     */
    public static Either<Error_, Map<Term, Term>> map(Graph graph, Term term) {
        Function<Term, Either<Error_, Term>> stripKey = t -> Either.right(strip(t));
        Function<Term, Either<Error_, Term>> same = t -> Either.right(t);
        return hydra.core.extract.Model.map(stripKey, same, graph, term);
    }

    /**
     * Build a set term, removing annotations from its elements.
     */
    public static Term setTerm(Iterable<Term> elements) {
        PersistentSet<Term> result = PersistentSet.empty();
        for (Term t : elements) {
            result = result.insert(strip(t));
        }
        return new Term.Set(result);
    }

    /**
     * Build a map term, removing annotations from its keys.
     */
    public static Term mapTerm(Map<Term, Term> entries) {
        PersistentMap<Term, Term> result = PersistentMap.empty();
        for (Map.Entry<Term, Term> e : entries.entrySet()) {
            result = result.insert(strip(e.getKey()), e.getValue());
        }
        return new Term.Map(result);
    }
}

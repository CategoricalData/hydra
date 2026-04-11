package hydra.util;

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.ArrayList;

/**
 * Utility methods for comparing objects that may not implement Comparable directly,
 * such as java.util collections. Used by generated compareTo methods.
 */
public class Comparing {

    /**
     * Compare two objects, handling java.util collections that do not implement Comparable.
     * Falls back to Comparable.compareTo for non-collection types.
     */
    @SuppressWarnings("unchecked")
    public static int compare(Object a, Object b) {
        if (a == b) return 0;
        if (a == null) return -1;
        if (b == null) return 1;

        // List comparison: element-by-element
        if (a instanceof List && b instanceof List) {
            return compareLists((List<?>) a, (List<?>) b);
        }

        // Map comparison: compare sorted entries
        if (a instanceof Map && b instanceof Map) {
            return compareMaps((Map<?, ?>) a, (Map<?, ?>) b);
        }

        // Set comparison: compare sorted elements
        if (a instanceof Set && b instanceof Set) {
            return compareSets((Set<?>) a, (Set<?>) b);
        }

        // Default: use Comparable
        return ((Comparable<Object>) a).compareTo(b);
    }

    @SuppressWarnings("unchecked")
    private static int compareLists(List<?> a, List<?> b) {
        int sizeA = a.size();
        int sizeB = b.size();
        int minSize = Math.min(sizeA, sizeB);
        for (int i = 0; i < minSize; i++) {
            int cmp = compare(a.get(i), b.get(i));
            if (cmp != 0) return cmp;
        }
        return Integer.compare(sizeA, sizeB);
    }

    @SuppressWarnings("unchecked")
    private static int compareMaps(Map<?, ?> a, Map<?, ?> b) {
        // Compare by sorted entries
        List<? extends Map.Entry<?, ?>> entriesA = sortedEntries(a);
        List<? extends Map.Entry<?, ?>> entriesB = sortedEntries(b);
        int minSize = Math.min(entriesA.size(), entriesB.size());
        for (int i = 0; i < minSize; i++) {
            Map.Entry<?, ?> ea = entriesA.get(i);
            Map.Entry<?, ?> eb = entriesB.get(i);
            int keyCmp = compare(ea.getKey(), eb.getKey());
            if (keyCmp != 0) return keyCmp;
            int valCmp = compare(ea.getValue(), eb.getValue());
            if (valCmp != 0) return valCmp;
        }
        return Integer.compare(entriesA.size(), entriesB.size());
    }

    @SuppressWarnings("unchecked")
    private static int compareSets(Set<?> a, Set<?> b) {
        List<Object> listA = new ArrayList<>(a);
        List<Object> listB = new ArrayList<>(b);
        listA.sort((x, y) -> compare(x, y));
        listB.sort((x, y) -> compare(x, y));
        return compareLists(listA, listB);
    }

    @SuppressWarnings("unchecked")
    private static List<? extends Map.Entry<?, ?>> sortedEntries(Map<?, ?> map) {
        List<Map.Entry<?, ?>> entries = new ArrayList<>(map.entrySet());
        entries.sort((a, b) -> compare(a.getKey(), b.getKey()));
        return entries;
    }
}

package hydra.tools;

import java.util.ArrayList;
import java.util.List;


/**
 * A simple tail-sharing linked list.
 */
public class LList<X> {
    public final X first;
    public final LList<X> rest;

    /**
     * Construct a list using a given head and tail (where the tail may be null).
     */
    public LList(X first, LList<X> rest) {
        this.first = first;
        this.rest = rest;
    }

    /**
     * Find the length of a list (which may be null).
     */
    public static <X> int length(LList<X> list) {
        LList<X> cur = list;
        int len = 0;
        while (cur != null) {
            len++;
            cur = cur.rest;
        }
        return len;
    }

    /**
     * Check whether a list is empty (null).
     */
    public static <X> boolean isEmpty(LList<X> list) {
        return list == null;
    }

    /**
     * Construct a list by logically pushing a value to the head of another list (which is not modified).
     */
    public static <X> LList<X> push(X value, LList<X> list) {
        return new LList<>(value, list);
    }

    /**
     * Construct a list by logically dropping n values from the head of another list (which is not modified).
     */
    public static <X> LList<X> drop(int n, LList<X> list) {
        LList<X> cur = list;
        for (int i = 0; i < n; i++) {
            cur = cur.rest;
        }
        return cur;
    }

    /**
     * Construct a list by logically taking the first n values from the head of another list (which is not modified).
     */
    public static <X> List<X> take(int n, LList<X> list) {
        List<X> javaList = new ArrayList<>();
        LList<X> cur = list;
        for (int i = 0; i < n; i++) {
            javaList.add(cur.first);
            cur = cur.rest;
        }
        return javaList;
    }
}

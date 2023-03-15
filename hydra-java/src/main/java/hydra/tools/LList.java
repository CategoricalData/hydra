package hydra.tools;

import java.util.ArrayList;
import java.util.List;


/**
 * A simple tail-sharing linked list
 */
public class LList<X> {
    public final X first;
    public final LList<X> rest;

    public LList(X first, LList<X> rest) {
        this.first = first;
        this.rest = rest;
    }

    public static <X> int length(LList<X> list) {
        LList<X> cur = list;
        int len = 0;
        while (cur != null) {
            len++;
            cur = cur.rest;
        }
        return len;
    }

    public static <X> boolean isEmpty(LList<X> list) {
        return list == null;
    }

    public static <X> LList<X> push(X value, LList<X> list) {
        return new LList<>(value, list);
    }

    public static <X> LList<X> drop(int n, LList<X> list) {
        LList<X> cur = list;
        for (int i = 0; i < n; i++) {
            cur = cur.rest;
        }
        return cur;
    }

    public static <X> List<X> take(int n, LList<X> list) {
        List<X> javaList = new ArrayList<>();
        LList<X> cur = list;
        for (int i = 0; i < n; i++) {
            javaList.add(cur.first);
            cur = cur.rest;
        }
        return javaList;
    }

    public List<X> takeAll() {
        List<X> javaList = new ArrayList<>();
        LList<X> cur = this;
        while (cur != null) {
            javaList.add(cur.first);
            cur = cur.rest;
        }
        return javaList;
    }
}

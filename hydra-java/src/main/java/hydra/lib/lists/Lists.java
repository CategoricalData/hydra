package hydra.lib.lists;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

public interface Lists {
    static <A, B> List<B> apply(List<Function<A, B>> functions, List<A> args) {
        return functions.stream().flatMap(f -> args.stream().map(f)).collect(Collectors.toList());
    }

    static <A, B> List<B> bind(List<A> args, Function<A, List<B>> mapping) {
        return args.stream().flatMap(x -> mapping.apply(x).stream()).collect(Collectors.toList());
    }

    static <A> List<A> concat(List<List<A>> sublists) {
        return sublists.stream().flatMap(Collection::stream).collect(Collectors.toList());
    }

    static <A> A head(List<A> list) {
        return list.get(0);
    }

    static <A> List<A> intercalate(List<A> delim, List<List<A>> sublists) {
        List<A> result = new ArrayList<>();
        boolean first = true;
        for (List<A> sublist : sublists) {
            if (first) {
                first = false;
            } else {
                result.addAll(delim);
            }
            result.addAll(sublist);
        }
        return result;
    }

    static <A> List<A> intersperse(A delim, List<A> list) {
        List<A> result = new ArrayList<>();
        boolean first = true;
        for (A a : list) {
            if (first) {
                first = false;
            } else {
                result.add(delim);
            }
            result.add(a);
        }
        return result;
    }

    static <A> A last(List<A> list) {
        return list.get(list.size() - 1);
    }

    static <A> int length(List<A> list) {
        return list.size();
    }

    static <A, B> List<B> map(Function<A, B> mapping, List<A> arg) {
        return arg.stream().map(mapping).collect(Collectors.toList());
    }

    static <A> List<A> pure(A single) {
        return Collections.singletonList(single);
    }
}

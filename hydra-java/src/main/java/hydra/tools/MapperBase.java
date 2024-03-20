package hydra.tools;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * A base class for simple functional mappers with exceptions (specific to Java)
 */
public abstract class MapperBase {

    protected static <T, C> C oneOrMany(List<T> list, Function<T, C> one, Function<List<T>, C> many) {
        if (list.isEmpty()) {
            return invalidEmptyList();
        } else if (list.size() == 1) {
            return one.apply(list.get(0));
        } else {
            return many.apply(list);
        }
    }

    protected static <T> T invalid(String message) {
        throw new MapperException(message);
    }

    protected static <T> T invalidEmptyList() {
        return invalid("empty list");
    }

    protected static <T> T invalidMissingField() {
        return invalid("missing required field");
    }

    protected static <T> T invalidUnexpectedNull() {
        return invalid("unexpected null");
    }

    protected static <C1, C2, T> Optional<T> optional(C1 c1, Function<C1, C2> accessor, Function<C2, T> constructor) {
        C2 c2 = accessor.apply(c1);
        return c2 == null ? Optional.empty() : Optional.of(constructor.apply(c2));
    }

    protected static <C1, C2, T> Optional<T> optional(C1 c1, int index, Function<C1, List<C2>> accessor, Function<C2, T> constructor) {
        List<C2> c2 = accessor.apply(c1);
        return null == c2 || index >= c2.size() ? Optional.empty() : Optional.of(constructor.apply(c2.get(index)));
    }

    protected static <C1, C2, T> T required(C1 c1, Function<C1, C2> accessor, Function<C2, T> constructor) {
        Optional<T> t = optional(c1, accessor, constructor);
        return t.orElseGet(() -> invalid("missing required field"));
    }

    protected static <C1, C2, T> T required(C1 c1, int index, Function<C1, List<C2>> accessor, Function<C2, T> constructor) {
        Optional<T> t = optional(c1, index, accessor, constructor);
        return t.orElseGet(() -> invalid("missing required field"));
    }

    protected static <C1, C2, T> List<T> list(C1 c1, Function<C1, List<C2>> accessor, Function<C2, T> constructor) {
        Optional<List<T>> result = optional(c1, accessor, c2s -> {
            List<T> ts = new ArrayList<>();
            for (C2 c2 : c2s) {
                T t = constructor.apply(c2);
                if (null == t) return invalidUnexpectedNull();
                ts.add(t);
            }
            return ts;
        });

        // Always return a list, even if empty
        return result.orElseGet(ArrayList::new);
    }

    protected static <A, B> List<B> map(List<A> list, Function<A, B> f) {
        return list.stream().map(f).collect(Collectors.toList());
    }

    protected static <C1, C2, T> List<T> nonemptyList(C1 c1, Function<C1, List<C2>> accessor, Function<C2, T> constructor) {
        List<T> ts = list(c1, accessor, constructor);
        if (ts.isEmpty()) {
            return invalid("empty list");
        }
        return ts;
    }

    protected static <C0, P0, C, P> Function<P0, Optional<P>> matchCase(
            Function<P0, C0> getter,
            Function<C0, C> childConstructor,
            Function<C, P> parentConstructor) {
        return ctx -> optional(ctx, getter, childConstructor).map(parentConstructor);
    }

    protected static <C0, P0, P> Function<P0, Optional<P>> matchCase(
            Function<P0, C0> getter,
            P parent) {
        return ctx -> null == getter.apply(ctx) ? Optional.empty() : Optional.of(parent);
    }

    protected static <T> T unsupported() {
        return unsupported("not yet implemented");
    }

    protected static <T> T unsupported(String message) {
        throw new UnsupportedOperationException(message);
    }

    public static class MapperException extends RuntimeException {
        public MapperException(String message) {
            super(message);
        }

        public MapperException(Throwable cause) {
            super(cause);
        }
    }
}

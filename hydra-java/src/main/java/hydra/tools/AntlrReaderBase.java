package hydra.tools;

import org.antlr.v4.runtime.ParserRuleContext;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

/**
 * A base class for utilities which convert ANTLR parse trees into domain-specific objects
 */
public class AntlrReaderBase {
    public static class AntlrReaderException extends RuntimeException {
        public AntlrReaderException(String message) {
            super(message);
        }

        public AntlrReaderException(Throwable cause) {
            super(cause);
        }
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

    protected static <C1, C2, T> List<T> nonemptyList(C1 c1, Function<C1, List<C2>> accessor, Function<C2, T> constructor) {
        List<T> ts = list(c1, accessor, constructor);
        if (ts.isEmpty()) {
            return invalid("empty list");
        }
        return ts;
    }

    protected static <T> T invalid(String message) {
        throw new AntlrReaderException(message);
    }

    protected static <T> T invalidMissingField() {
        return invalid("missing required field");
    }

    protected static <T> T invalidUnexpectedNull() {
        return invalid("unexpected null");
    }

    protected static <P0 extends ParserRuleContext, P> P match(P0 ctx,
                                                               Function<P0, Optional<P>>... funs) {
        if (null != ctx.exception) {
            String text = ctx.getText();
            throw new AntlrReaderException(ctx.exception);
        }

        for (Function<P0, Optional<P>> f : funs) {
            Optional<P> res = f.apply(ctx);
            if (res.isPresent()) {
                return res.get();
            }
        }
        return invalid("union failed to match");
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
        throw new UnsupportedOperationException("not yet implemented");
    }
}

package hydra.lib.strings;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Get the character at a given index in a string, returning Nothing if the index is out of bounds.
 */
public class MaybeCharAt extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.strings.maybeCharAt");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(int32(), string(), optional(int32())));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.Core.int32(cx, graph, args.get(0)), i -> hydra.lib.eithers.Map.apply(s -> Terms.optional(MaybeCharAt.apply(i, s).map(Terms::int32)), hydra.extract.Core.string(cx, graph, args.get(1))));
    }

    /**
     * Returns a function that retrieves the character at the given index.
     * @param index the index of the character to retrieve
     * @return a function that takes a string and returns a Maybe containing the code point at the index
     */
    public static Function<String, Maybe<Integer>> apply(Integer index) {
        return (s) -> apply(index, s);
    }

    /**
     * Get the character at the given index in the string.
     * @param index the index of the character to retrieve
     * @param s the string to query
     * @return a Maybe containing the code point at the index, or empty if out of bounds
     */
    public static Maybe<Integer> apply(Integer index, String s) {
        int len = s.codePointCount(0, s.length());
        if (index < 0 || index >= len) {
            return Maybe.nothing();
        } else {
            return Maybe.just(s.codePointAt(s.offsetByCodePoints(0, index)));
        }
    }
}

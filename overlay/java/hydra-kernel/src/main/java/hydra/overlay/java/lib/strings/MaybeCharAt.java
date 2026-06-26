package hydra.overlay.java.lib.strings;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;
import hydra.overlay.java.util.Optional;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.int32;
import static hydra.overlay.java.dsl.Types.optional;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.string;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


/**
 * Get the character at a given index in a string, returning Nothing if the index is out of bounds.
 */
public class MaybeCharAt extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Strings.maybeCharAt().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(int32(), string(), optional(int32())));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Bind.apply(hydra.extract.Core.int32(graph, args.get(0)), i -> hydra.overlay.java.lib.eithers.Map.apply(s -> Terms.optional(MaybeCharAt.apply(i, s).map(Terms::int32)), hydra.extract.Core.string(graph, args.get(1))));
    }

    /**
     * Returns a function that retrieves the character at the given index.
     * @param index the index of the character to retrieve
     * @return a function that takes a string and returns a Optional containing the code point at the index
     */
    public static Function<String, Optional<Integer>> apply(Integer index) {
        return (s) -> apply(index, s);
    }

    /**
     * Get the character at the given index in the string.
     * @param index the index of the character to retrieve
     * @param s the string to query
     * @return a Optional containing the code point at the index, or empty if out of bounds
     */
    public static Optional<Integer> apply(Integer index, String s) {
        int len = s.codePointCount(0, s.length());
        if (index < 0 || index >= len) {
            return Optional.none();
        } else {
            return Optional.given(s.codePointAt(s.offsetByCodePoints(0, index)));
        }
    }
}

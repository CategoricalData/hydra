package hydra.core.overlay.java.lib.strings;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;
import hydra.core.overlay.java.util.Optional;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.int32;
import static hydra.core.overlay.java.dsl.Types.optional;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.string;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Get the character at a given index in a string, returning Nothing if the index is out of bounds.
 */
public class CharAt extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Strings.charAt().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(int32(), string(), optional(int32())));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Bind.apply(hydra.core.extract.Core.int32(graph, args.get(0)), i -> hydra.core.overlay.java.lib.eithers.Map.apply(s -> Terms.optional(CharAt.apply(i, s).map(Terms::int32)), hydra.core.extract.Core.string(graph, args.get(1))));
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

package hydra.core.overlay.java.lib.literals;

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
import static hydra.core.overlay.java.dsl.Types.optional;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.string;
import static hydra.core.overlay.java.dsl.Types.uint32;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Primitive function which parses a string into a uint32 (32-bit unsigned integer).
 * Returns an optional value that is empty if the string cannot be parsed or is out of range (0-4294967295).
 */
public class ParseUint32 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.core.lib.literals.parseUint32"
     */
    public Name name() {
        return hydra.core.lib.Literals.parseUint32().name;
    }

    /**
     * Returns the type scheme for this function: string -&gt; optional uint32.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(string(), optional(uint32())));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that parses string terms into optional uint32 terms
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply((Function<String, Term>) s -> Terms.optional(apply(s).map(Terms::uint32)), hydra.core.extract.Model.string(graph, args.get(0)));
    }

    /**
     * Attempts to parse a string into a Long representing uint32 (0-4294967295).
     * @param str the string to parse
     * @return a Optional containing the parsed Long, or empty if parsing fails or value is out of range
     */
    public static Optional<Long> apply(String str) {
        try {
            long n = Long.parseLong(str);
            if (n >= 0 && n <= 4294967295L) {
                return Optional.given(n);
            } else {
                return Optional.none();
            }
        } catch (NumberFormatException e) {
            return Optional.none();
        }
    }
}

package hydra.overlay.java.lib.literals;

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
import static hydra.overlay.java.dsl.Types.int16;
import static hydra.overlay.java.dsl.Types.optional;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.string;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


/**
 * Primitive function which parses a string into an int16 (16-bit signed integer).
 * Returns an optional value that is empty if the string cannot be parsed or is out of range.
 */
public class ReadInt16 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.readInt16"
     */
    public Name name() {
        return hydra.lib.Literals.readInt16().name;
    }

    /**
     * Returns the type scheme for this function: string -&gt; optional int16.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(string(), optional(int16())));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that parses string terms into optional int16 terms
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply((Function<String, Term>) s -> Terms.optional(apply(s).map(Terms::int16)), hydra.extract.Core.string(graph, args.get(0)));
    }

    /**
     * Attempts to parse a string into a Short (16-bit signed).
     * @param str the string to parse
     * @return a Optional containing the parsed Short, or empty if parsing fails or value is out of range
     */
    public static Optional<Short> apply(String str) {
        try {
            long n = Long.parseLong(str);
            if (n >= -32768 && n <= 32767) {
                return Optional.given((short) n);
            } else {
                return Optional.none();
            }
        } catch (NumberFormatException e) {
            return Optional.none();
        }
    }
}

package hydra.lib.literals;

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
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import static hydra.dsl.Types.uint16;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;


/**
 * Primitive function which parses a string into a uint16 (16-bit unsigned integer).
 * Returns an optional value that is empty if the string cannot be parsed or is out of range (0-65535).
 */
public class ReadUint16 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.readUint16"
     */
    public Name name() {
        return new Name("hydra.lib.literals.readUint16");
    }

    /**
     * Returns the type scheme for this function: string -&gt; optional uint16.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(string(), optional(uint16())));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that parses string terms into optional uint16 terms
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<String, Term>) s -> Terms.optional(apply(s).map(Terms::uint16)), hydra.extract.core.Core.string(cx, graph, args.get(0)));
    }

    /**
     * Attempts to parse a string into a Character representing uint16 (0-65535).
     * @param str the string to parse
     * @return a Maybe containing the parsed Character, or empty if parsing fails or value is out of range
     */
    public static Maybe<Character> apply(String str) {
        try {
            long n = Long.parseLong(str);
            if (n >= 0 && n <= 65535) {
                return Maybe.just((char) n);
            } else {
                return Maybe.nothing();
            }
        } catch (NumberFormatException e) {
            return Maybe.nothing();
        }
    }
}

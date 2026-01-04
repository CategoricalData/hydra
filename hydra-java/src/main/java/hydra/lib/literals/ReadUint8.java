package hydra.lib.literals;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Flows;
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
import static hydra.dsl.Types.uint8;


/**
 * Primitive function which parses a string into a uint8 (8-bit unsigned integer).
 * Returns an optional value that is empty if the string cannot be parsed or is out of range (0-255).
 */
public class ReadUint8 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.readUint8"
     */
    public Name name() {
        return new Name("hydra.lib.literals.readUint8");
    }

    /**
     * Returns the type scheme for this function: string -&gt; optional uint8.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(string(), optional(uint8())));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that parses string terms into optional uint8 terms
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.string(args.get(0)),
            (Function<String, Term>) s -> Terms.optional(apply(s).map(Terms::uint8)));
    }

    /**
     * Attempts to parse a string into a Short representing uint8 (0-255).
     * @param str the string to parse
     * @return a Maybe containing the parsed Short, or empty if parsing fails or value is out of range
     */
    public static Maybe<Short> apply(String str) {
        try {
            long n = Long.parseLong(str);
            if (n >= 0 && n <= 255) {
                return Maybe.just((short) n);
            } else {
                return Maybe.nothing();
            }
        } catch (NumberFormatException e) {
            return Maybe.nothing();
        }
    }
}

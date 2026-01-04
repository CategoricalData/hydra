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
import static hydra.dsl.Types.int8;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;


/**
 * Primitive function which parses a string into an int8 (8-bit signed integer).
 * Returns an optional value that is empty if the string cannot be parsed or is out of range.
 */
public class ReadInt8 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.readInt8"
     */
    public Name name() {
        return new Name("hydra.lib.literals.readInt8");
    }

    /**
     * Returns the type scheme for this function: string -&gt; optional int8.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(string(), optional(int8())));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that parses string terms into optional int8 terms
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.string(args.get(0)),
            (Function<String, Term>) s -> Terms.optional(apply(s).map(Terms::int8)));
    }

    /**
     * Attempts to parse a string into a Byte (8-bit signed).
     * @param str the string to parse
     * @return a Maybe containing the parsed Byte, or empty if parsing fails or value is out of range
     */
    public static Maybe<Byte> apply(String str) {
        try {
            long n = Long.parseLong(str);
            if (n >= -128 && n <= 127) {
                return Maybe.just((byte) n);
            } else {
                return Maybe.nothing();
            }
        } catch (NumberFormatException e) {
            return Maybe.nothing();
        }
    }
}

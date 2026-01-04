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
import static hydra.dsl.Types.uint32;


/**
 * Primitive function which parses a string into a uint32 (32-bit unsigned integer).
 * Returns an optional value that is empty if the string cannot be parsed or is out of range (0-4294967295).
 */
public class ReadUint32 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.readUint32"
     */
    public Name name() {
        return new Name("hydra.lib.literals.readUint32");
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
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.string(args.get(0)),
            (Function<String, Term>) s -> Terms.optional(apply(s).map(Terms::uint32)));
    }

    /**
     * Attempts to parse a string into a Long representing uint32 (0-4294967295).
     * @param str the string to parse
     * @return a Maybe containing the parsed Long, or empty if parsing fails or value is out of range
     */
    public static Maybe<Long> apply(String str) {
        try {
            long n = Long.parseLong(str);
            if (n >= 0 && n <= 4294967295L) {
                return Maybe.just(n);
            } else {
                return Maybe.nothing();
            }
        } catch (NumberFormatException e) {
            return Maybe.nothing();
        }
    }
}

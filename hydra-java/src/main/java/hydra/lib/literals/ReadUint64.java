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

import java.math.BigInteger;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import static hydra.dsl.Types.uint64;


/**
 * Primitive function which parses a string into a uint64 (64-bit unsigned integer).
 * Returns an optional value that is empty if the string cannot be parsed or is out of range (0-18446744073709551615).
 */
public class ReadUint64 extends PrimitiveFunction {
    private static final BigInteger MAX_UINT64 = new BigInteger("18446744073709551615");

    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.readUint64"
     */
    public Name name() {
        return new Name("hydra.lib.literals.readUint64");
    }

    /**
     * Returns the type scheme for this function: string -&gt; optional uint64.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(string(), optional(uint64())));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that parses string terms into optional uint64 terms
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.string(args.get(0)),
            (Function<String, Term>) s -> Terms.optional(apply(s).map(Terms::uint64)));
    }

    /**
     * Attempts to parse a string into a BigInteger representing uint64 (0-18446744073709551615).
     * @param str the string to parse
     * @return a Maybe containing the parsed BigInteger, or empty if parsing fails or value is out of range
     */
    public static Maybe<BigInteger> apply(String str) {
        try {
            BigInteger n = new BigInteger(str);
            if (n.compareTo(BigInteger.ZERO) >= 0 && n.compareTo(MAX_UINT64) <= 0) {
                return Maybe.just(n);
            } else {
                return Maybe.nothing();
            }
        } catch (NumberFormatException e) {
            return Maybe.nothing();
        }
    }
}

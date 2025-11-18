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

import java.math.BigDecimal;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.bigfloat;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;


/**
 * Primitive function which parses a string into a bigfloat (arbitrary-precision decimal).
 * Returns an optional value that is empty if the string cannot be parsed.
 */
public class ReadBigfloat extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.readBigfloat"
     */
    public Name name() {
        return new Name("hydra.lib.literals.readBigfloat");
    }

    /**
     * Returns the type scheme for this function: string -&gt; optional bigfloat.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(string(), optional(bigfloat())));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that parses string terms into optional bigfloat terms
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.string(args.get(0)),
            (Function<String, Term>) s -> Terms.optional(apply(s).map(Terms::bigfloat)));
    }

    /**
     * Attempts to parse a string into a BigDecimal.
     * @param str the string to parse
     * @return an Opt containing the parsed BigDecimal, or empty if parsing fails
     */
    public static Maybe<BigDecimal> apply(String str) {
        try {
            return Maybe.just(new BigDecimal(str));
        } catch (NumberFormatException e) {
            return Maybe.nothing();
        }
    }
}

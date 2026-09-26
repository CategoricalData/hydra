package hydra.core.overlay.java.lib.literals;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;
import hydra.core.overlay.java.util.Optional;

import java.math.BigInteger;
import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.bigint;
import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.optional;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.string;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Primitive function which parses a string into a bigint (arbitrary precision integer).
 * Returns an optional value that is empty if the string cannot be parsed.
 */
public class ParseBigint extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.core.lib.literals.parseBigint"
     */
    public Name name() {
        return hydra.core.lib.Literals.parseBigint().name;
    }

    /**
     * Returns the type scheme for this function: string -&gt; optional bigint.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(string(), optional(bigint())));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that parses string terms into optional bigint terms
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply((Function<String, Term>) s -> Terms.optional(apply(s).map(Terms::bigint)), hydra.core.extract.Model.string(graph, args.get(0)));
    }

    /**
     * Attempts to parse a string into a BigInteger.
     * @param str the string to parse
     * @return a Optional containing the parsed BigInteger, or empty if parsing fails
     */
    public static Optional<BigInteger> apply(String str) {
        try {
            return Optional.given(new BigInteger(str));
        } catch (NumberFormatException e) {
            return Optional.none();
        }
    }
}

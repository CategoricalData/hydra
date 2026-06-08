package hydra.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Optional;

import java.math.BigInteger;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.bigint;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import hydra.typing.InferenceContext;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Primitive function which parses a string into a bigint (arbitrary precision integer).
 * Returns an optional value that is empty if the string cannot be parsed.
 */
public class ReadBigint extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.readBigint"
     */
    public Name name() {
        return new Name("hydra.lib.literals.readBigint");
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
    protected Function<List<Term>, Function<InferenceContext, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<String, Term>) s -> Terms.optional(apply(s).map(Terms::bigint)), hydra.extract.Core.string(graph, args.get(0)));
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

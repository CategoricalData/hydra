package hydra.overlay.java.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;
import hydra.overlay.java.util.Optional;

import java.math.BigDecimal;
import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.decimal;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.optional;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.string;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


/**
 * Primitive function which parses a string into a decimal (arbitrary-precision exact decimal).
 * Returns an optional value that is empty if the string cannot be parsed.
 */
public class ReadDecimal extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.readDecimal"
     */
    public Name name() {
        return hydra.lib.Literals.readDecimal().name;
    }

    /**
     * Returns the type scheme for this function: string -&gt; optional decimal.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(string(), optional(decimal())));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that parses string terms into optional decimal terms
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply((Function<String, Term>) s -> Terms.optional(apply(s).map(Terms::decimal)), hydra.extract.Core.string(graph, args.get(0)));
    }

    /**
     * Attempts to parse a string into a BigDecimal.
     * @param str the string to parse
     * @return a Optional containing the parsed BigDecimal, or empty if parsing fails
     */
    public static Optional<BigDecimal> apply(String str) {
        try {
            return Optional.given(new BigDecimal(str));
        } catch (NumberFormatException e) {
            return Optional.none();
        }
    }
}

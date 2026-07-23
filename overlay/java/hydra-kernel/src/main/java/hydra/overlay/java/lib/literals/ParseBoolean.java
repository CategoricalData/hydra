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

import static hydra.overlay.java.dsl.Types.boolean_;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.optional;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.string;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


/**
 * Primitive function which parses a string into a boolean.
 * Accepts "true" or "false" strings, returns empty optional for any other input.
 */
public class ParseBoolean extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.parseBoolean"
     */
    public Name name() {
        return hydra.lib.Literals.parseBoolean().name;
    }

    /**
     * Returns the type scheme for this function: string -&gt; optional boolean.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(string(), optional(boolean_())));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that parses string terms into optional boolean terms
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply((Function<String, Term>) s -> Terms.optional(apply(s).map(Terms::boolean_)), hydra.extract.Core.string(graph, args.get(0)));
    }

    /**
     * Attempts to parse a string into a Boolean.
     * @param str the string to parse (must be "true" or "false")
     * @return an Opt containing the parsed Boolean, or empty if the string is neither "true" nor "false"
     */
    public static Optional<Boolean> apply(String str) {
        if ("true".equals(str)) {
            return Optional.given(true);
        } else if ("false".equals(str)) {
            return Optional.given(false);
        } else {
            return Optional.none();
        }
    }
}

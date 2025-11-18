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


/**
 * Primitive function which parses a string literal representation into a string value.
 * Expects the input to be surrounded by double quotes.
 */
public class ReadString extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.readString"
     */
    public Name name() {
        return new Name("hydra.lib.literals.readString");
    }

    /**
     * Returns the type scheme for this function: string -&gt; optional string.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(string(), optional(string())));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that parses string literal terms into optional string terms
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.string(args.get(0)),
            (Function<String, Term>) s -> Terms.optional(apply(s).map(Terms::string)));
    }

    /**
     * Attempts to parse a quoted string literal into a string value.
     * @param str the string to parse (must be surrounded by double quotes)
     * @return an Opt containing the unquoted string, or empty if the input is not properly quoted
     */
    public static Maybe<String> apply(String str) {
        // In Haskell, readMaybe parses a string literal representation
        // For simple string reading, we just return the string wrapped in Some
        // A more sophisticated implementation might parse escaped strings
        if (str.startsWith("\"") && str.endsWith("\"") && str.length() >= 2) {
            return Maybe.just(str.substring(1, str.length() - 1));
        }
        return Maybe.nothing();
    }
}

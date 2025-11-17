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
import hydra.util.Opt;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;


/**
 * Primitive function which parses a string into a boolean.
 * Accepts "true" or "false" strings, returns empty optional for any other input.
 */
public class ReadBoolean extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.readBoolean"
     */
    public Name name() {
        return new Name("hydra.lib.literals.readBoolean");
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
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.string(args.get(0)),
            (Function<String, Term>) s -> Terms.optional(apply(s).map(Terms::boolean_)));
    }

    /**
     * Attempts to parse a string into a Boolean.
     * @param str the string to parse (must be "true" or "false")
     * @return an Opt containing the parsed Boolean, or empty if the string is neither "true" nor "false"
     */
    public static Opt<Boolean> apply(String str) {
        if ("true".equals(str)) {
            return Opt.of(true);
        } else if ("false".equals(str)) {
            return Opt.of(false);
        } else {
            return Opt.empty();
        }
    }
}

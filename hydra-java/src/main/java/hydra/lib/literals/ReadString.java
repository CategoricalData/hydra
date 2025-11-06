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

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;


/**
 * Primitive function: ReadString.
 */
public class ReadString extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.literals.readString");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(string(), optional(string())));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.string(args.get(0)),
            (Function<String, Term>) s -> Terms.optional(apply(s).map(Terms::string)));
    }

    /**
     * Applies the ReadString operation.
     * @param str the str
     * @return the result
     */
        public static Opt<String> apply(String str) {
        // In Haskell, readMaybe parses a string literal representation
        // For simple string reading, we just return the string wrapped in Some
        // A more sophisticated implementation might parse escaped strings
        if (str.startsWith("\"") && str.endsWith("\"") && str.length() >= 2) {
            return Opt.of(str.substring(1, str.length() - 1));
        }
        return Opt.empty();
    }
}

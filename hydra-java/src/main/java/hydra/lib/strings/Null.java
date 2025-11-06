package hydra.lib.strings;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Flows.map;
import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;

/**
 * Determines whether a string is empty (null check).
 */
public class Null extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.strings.null");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(string(), boolean_()));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> map(Expect.string(args.get(0)), s -> Terms.boolean_(apply(s)));
    }

    /**
     * Checks whether the given string is empty.
     * @param s the string to test
     * @return true if the string is empty, false otherwise
     */
    public static Boolean apply(String s) {
        return s.isEmpty();
    }
}

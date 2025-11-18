package hydra.lib.maybes;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;


/**
 * Filters and extracts Just values.
 */
public class Cat extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.maybes.cat"
     */
    public Name name() {
        return new Name("hydra.lib.maybes.cat");
    }

    /**
     * Returns the type scheme of this primitive function.
     * @return the type scheme for extracting present values from a list of optionals
     */
    @Override
    public TypeScheme type() {
        return scheme("a", function(list(optional("a")), list("a")));
    }

    /**
     * Returns the implementation of this primitive function.
     * @return a function that filters and extracts Just values from a list
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.list(x -> Expect.optional(Flows::pure, x), args.get(0)),
                (Function<List<Maybe<Term>>, Term>) optionals -> Terms.list(apply(optionals)));
    }

    /**
     * Filters and extracts Just values from a list of optionals.
     * @param <X> the element type
     * @param opt the list of optional values
     * @return a list containing only the present values
     */
    public static <X> List<X> apply(List<Maybe<X>> opt) {
        List<X> result = new ArrayList<>();
        for (Maybe<X> x : opt) {
            x.ifJust(result::add);
        }
        return result;
    }
}

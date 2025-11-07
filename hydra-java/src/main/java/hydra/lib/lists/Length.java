package hydra.lib.lists;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;

/**
 * Returns the length of a list.
 */
public class Length extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.length");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list("a"), int32()));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.list(Flows::pure, args.get(0)), l -> Terms.int32(apply(l)));
    }

    /**
     * Returns the number of elements.
     * @param list the list
     * @return the length
     */
        public static <X> int apply(List<X> list) {
        return list.size();
    }
}

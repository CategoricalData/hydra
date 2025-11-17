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

import java.util.Collection;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;

/**
 * Concatenates a list of lists.
 */
public class Concat extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.lists.concat");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list(list("a")), list("a")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.list(t -> Expect.list(Flows::pure, t), args.get(0)), l -> Terms.list(apply(l)));
    }

    /**
     * Flattens a list of lists.
     * @param <X> the element type
     * @param sublists the list of lists to concatenate
     * @return the concatenated list
     */
    public static <X> List<X> apply(List<List<X>> sublists) {
        return sublists.stream().flatMap(Collection::stream).collect(Collectors.toList());
    }
}

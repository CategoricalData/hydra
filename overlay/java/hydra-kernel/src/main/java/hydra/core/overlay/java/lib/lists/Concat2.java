package hydra.core.overlay.java.lib.lists;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.ConsList;
import hydra.core.overlay.java.util.Either;

/**
 * Concatenates two lists.
 */
public class Concat2 extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.core.lib.lists.concat2");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list("a"), list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Bind.apply(hydra.core.extract.Model.list(graph, args.get(0)), l1 -> hydra.core.overlay.java.lib.eithers.Map.apply(l2 -> Terms.list(Concat2.apply(l1, l2)), hydra.core.extract.Model.list(graph, args.get(1))));
    }

    /**
     * Apply the function to both arguments.
     * @param <X> the element type
     * @param l1 the first list
     * @param l2 the second list
     * @return the concatenated list
     */
    public static <X> List<X> apply(List<X> l1, List<X> l2) {
        return ConsList.fromList(l1).concat(ConsList.fromList(l2));
    }
}

package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import hydra.util.ConsList;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Concatenates two lists.
 */
public class Concat2 extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.lists.concat2");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list("a"), list("a"), list("a")));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.list(cx, graph, args.get(0)), l1 -> hydra.lib.eithers.Map.apply(l2 -> Terms.list(Concat2.apply(l1, l2)), hydra.extract.core.Core.list(cx, graph, args.get(1))));
    }

    /**
     * Apply the function to both arguments.
     * @param <X> the element type
     * @param l1 the first list
     * @param l2 the second list
     * @return the concatenated list
     */
    public static <X> ConsList<X> apply(ConsList<X> l1, ConsList<X> l2) {
        return l1.concat(l2);
    }
}

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
import java.util.stream.Collectors;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;


/**
 * Monadic bind for flows.
 */
public class Bind extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.bind");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b",
            function(list("a"), function(Types.var("a"), list("b")), list("b")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.list(Flows::pure, args.get(0)), argsArg -> {
            Term mapping = args.get(1);
            return Terms.apply(
                Terms.primitive(Concat.NAME),
                Terms.list(argsArg.stream().map(a -> Terms.apply(mapping, a)).collect(Collectors.toList())));
        });
    }

    /**
     * Chains flow computations.
     * @param args the flowValue
     * @return the result flow
     */
        public static <X, Y> Function<Function<X, List<Y>>, List<Y>> apply(List<X> args) {
        return (mapping) -> apply(args, mapping);
    }

    /**
     * Chains flow computations.
     * @param args the flowValue
     * @param mapping the function
     * @return the result flow
     */
        public static <X, Y> List<Y> apply(List<X> args, Function<X, List<Y>> mapping) {
        return args.stream().flatMap(x -> mapping.apply(x).stream()).collect(Collectors.toList());
    }
}

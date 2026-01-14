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

import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;


/**
 * Applies a function to each element of a list, returning a new list of results.
 */
public class Map extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.map");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b",
                function(function("a", "b"), list("a"), list("b")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.list(instance -> pure(Terms.apply(args.get(0), instance)), args.get(1)),
            Terms::list);
    }

    /**
     * Applies a function to each element of a list.
     * @param <X> the input element type
     * @param <Y> the output element type
     * @param mapping the function to apply to each element
     * @return a function that maps the function over a list
     */
    public static <X, Y> Function<List<X>, List<Y>> apply(Function<X, Y> mapping) {
        return (arg) -> apply(mapping, arg);
    }

    /**
     * Applies a function to each element of a list, returning a new list of results.
     * @param <X> the input element type
     * @param <Y> the output element type
     * @param mapping the function to apply to each element
     * @param arg the list to map over
     * @return a new list containing the results of applying the function to each element
     */
    public static <X, Y> List<Y> apply(Function<X, Y> mapping, List<X> arg) {
        return arg.stream().map(mapping).collect(Collectors.toList());
    }
}

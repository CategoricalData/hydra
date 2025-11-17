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

import java.util.LinkedList;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

import static hydra.dsl.Flows.map2;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;


/**
 * Applies a function in a flow context.
 */
public class Apply extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.apply");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b",
                function(list(function("a", "b")), list("a"), list("b")));
    }

    // Note: this implementation does not use apply(),
    //       as actually applying the mapping function would require beta reduction,
    //       which would make the mapping function monadic.
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> map2(Expect.list(Flows::pure, args.get(0)), Expect.list(Flows::pure, args.get(1)),
                (BiFunction<List<Term>, List<Term>, Term>) (functions, arguments) -> {
                    List<Term> apps = new LinkedList<>();
                    for (Term f : functions) {
                        for (Term a : arguments) {
                            apps.add(Terms.apply(f, a));
                        }
                    }
                    return Terms.list(apps);
                });
    }

    /**
     * Applies a function within a flow.
     * @param <X> the input type
     * @param <Y> the output type
     * @param functions the list of functions to apply
     * @return a function that applies all functions to all arguments
     */
    public static <X, Y> Function<List<X>, List<Y>> apply(List<Function<X, Y>> functions) {
        return (args) -> apply(functions, args);
    }

    /**
     * Apply the function to both arguments.
     * @param <X> the input type
     * @param <Y> the output type
     * @param functions the list of functions to apply
     * @param args the list of arguments
     * @return the list of results from applying each function to each argument
     */
    public static <X, Y> List<Y> apply(List<Function<X, Y>> functions, List<X> args) {
        List<Y> results = new LinkedList<>();
        for (Function<X, Y> f : functions) {
            for (X a : args) {
                results.add(f.apply(a));
            }
        }
        return results;
    }
}

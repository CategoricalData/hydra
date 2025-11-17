package hydra.lib.lists;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Flows.map2;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.variable;


/**
 * Retrieves an element at an index.
 */
public class At extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.at");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(int32(), list("a"), variable("a")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> map2(Expect.int32(args.get(0)), Expect.list(Flows::pure, args.get(1)),
                (i, list) -> list.get(i));
    }

    /**
     * Retrieves the element at the given index.
     * @param <X> the element type
     * @param i the index to retrieve
     * @return a function that retrieves the element at the given index
     */
    public static <X> Function<List<X>, X> apply(int i) {
        return list -> apply(i, list);
    }

    /**
     * Retrieves the element at the given index.
     * @param <X> the element type
     * @param i the index to retrieve
     * @param list the list to retrieve from
     * @return the element at the given index
     */
    public static <X> X apply(int i, List<X> list) {
        if (list.isEmpty()) {
            throw new IllegalArgumentException("Cannot get head of empty list");
        } else {
            return list.get(i);
        }
    }
}

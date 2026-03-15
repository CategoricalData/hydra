package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;
import hydra.util.Pair;

import hydra.util.ConsList;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;


/**
 * Zips two lists into pairs.
 */
public class Zip extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.zip");
    }

    @Override
    public TypeScheme type() {
        return new hydra.core.TypeScheme(
                ConsList.of(new hydra.core.Name("a"), new hydra.core.Name("b")),
                function(list("a"), list("b"), list(Types.pair(Types.variable("a"), Types.variable("b")))),
                Maybe.nothing());
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.list(cx, graph, args.get(0)), lst1 ->
            hydra.lib.eithers.Map.apply((Function<ConsList<Term>, Term>) lst2 -> {
                    ArrayList<Term> items1 = lst1.toArrayList();
                    ArrayList<Term> items2 = lst2.toArrayList();
                    List<Term> result = new ArrayList<>();
                    int minSize = Math.min(items1.size(), items2.size());
                    for (int i = 0; i < minSize; i++) {
                        result.add(Terms.pair(items1.get(i), items2.get(i)));
                    }
                    return Terms.list(result);
                }, hydra.extract.core.Core.list(cx, graph, args.get(1))));
    }

    /**
     * Combines two lists into pairs.
     * @param <X> the first list element type
     * @param <Y> the second list element type
     * @param lst1 the first list
     * @return a function that zips the first list with a second list
     */
    public static <X, Y> Function<ConsList<Y>, ConsList<Pair<X, Y>>> apply(ConsList<X> lst1) {
        return lst2 -> apply(lst1, lst2);
    }

    /**
     * Combines two lists into pairs.
     * @param <X> the first list element type
     * @param <Y> the second list element type
     * @param lst1 the first list
     * @param lst2 the second list
     * @return a list of pairs containing elements from both lists
     */
    public static <X, Y> ConsList<Pair<X, Y>> apply(ConsList<X> lst1, ConsList<Y> lst2) {
        ArrayList<X> items1 = lst1.toArrayList();
        ArrayList<Y> items2 = lst2.toArrayList();
        ArrayList<Pair<X, Y>> result = new ArrayList<>();
        int minSize = Math.min(items1.size(), items2.size());
        for (int i = 0; i < minSize; i++) {
            result.add(new Pair<>(items1.get(i), items2.get(i)));
        }
        return ConsList.fromList(result);
    }
}

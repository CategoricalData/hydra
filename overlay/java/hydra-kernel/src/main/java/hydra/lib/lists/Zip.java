package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.ConsList;
import hydra.util.Maybe;
import hydra.util.Pair;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import hydra.typing.InferenceContext;
import hydra.errors.Error_;
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
                Arrays.asList(new hydra.core.Name("a"), new hydra.core.Name("b")),
                function(list("a"), list("b"), list(Types.pair(Types.variable("a"), Types.variable("b")))),
                Maybe.nothing());
    }

    @Override
    protected Function<List<Term>, Function<InferenceContext, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.Core.list(graph, args.get(0)), lst1 ->
            hydra.lib.eithers.Map.apply((Function<List<Term>, Term>) lst2 -> {
                    ConsList<Term> reversed = ConsList.empty();
                    Iterator<Term> it1 = lst1.iterator();
                    Iterator<Term> it2 = lst2.iterator();
                    while (it1.hasNext() && it2.hasNext()) {
                        reversed = ConsList.cons(Terms.pair(it1.next(), it2.next()), reversed);
                    }
                    return Terms.list(reversed.reverse());
                }, hydra.extract.Core.list(graph, args.get(1))));
    }

    /**
     * Combines two lists into pairs.
     * @param <X> the first list element type
     * @param <Y> the second list element type
     * @param lst1 the first list
     * @return a function that zips the first list with a second list
     */
    public static <X, Y> Function<List<Y>, List<Pair<X, Y>>> apply(List<X> lst1) {
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
    public static <X, Y> List<Pair<X, Y>> apply(List<X> lst1, List<Y> lst2) {
        ConsList<Pair<X, Y>> reversed = ConsList.empty();
        Iterator<X> it1 = lst1.iterator();
        Iterator<Y> it2 = lst2.iterator();
        while (it1.hasNext() && it2.hasNext()) {
            reversed = ConsList.cons(new Pair<>(it1.next(), it2.next()), reversed);
        }
        return reversed.reverse();
    }
}

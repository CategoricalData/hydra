package hydra.core.overlay.java.lib.lists;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;
import hydra.core.overlay.java.util.ConsList;
import hydra.core.overlay.java.util.Optional;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Zips two lists with a function.
 */
public class ZipWith extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Lists.zipWith().name;
    }

    @Override
    public TypeScheme type() {
        return new hydra.core.model.TypeScheme(
                Arrays.asList(new hydra.core.model.Name("a"), new hydra.core.model.Name("b"), new hydra.core.model.Name("c")),
                function(function(Types.var("a"), Types.var("b"), Types.var("c")), list("a"), list("b"), list("c")),
                hydra.core.overlay.java.util.PersistentMap.<hydra.core.model.Name, hydra.core.model.TypeVariableConstraints>empty());
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Bind.apply(hydra.core.extract.Core.list(graph, args.get(1)), lst1 ->
                hydra.core.overlay.java.lib.eithers.Bind.apply(hydra.core.extract.Core.list(graph, args.get(2)), lst2 -> {
                    Term f = args.get(0);
                    ConsList<Term> reversed = ConsList.empty();
                    Iterator<Term> it1 = lst1.iterator();
                    Iterator<Term> it2 = lst2.iterator();
                    while (it1.hasNext() && it2.hasNext()) {
                        Either<Error_, Term> r = hydra.Reduction.reduceTerm(
                            hydra.Lexical.emptyInferenceContext(), graph, true, Terms.apply(Terms.apply(f, it1.next()), it2.next()));
                        if (r.isLeft()) return (Either) r;
                        reversed = ConsList.cons(((Either.Right<Error_, Term>) r).value, reversed);
                    }
                    return Either.right(Terms.list(reversed.reverse()));
                }));
    }

    /**
     * Combines two lists using a function.
     * @param <X> the first list element type
     * @param <Y> the second list element type
     * @param <Z> the result element type
     * @param f the combining function
     * @return a curried function that takes two lists and combines them
     */
    public static <X, Y, Z> Function<List<X>, Function<List<Y>, List<Z>>> apply(BiFunction<X, Y, Z> f) {
        return lst1 -> lst2 -> apply(f, lst1, lst2);
    }

    /**
     * Combines two lists using a function.
     * @param <X> the first list element type
     * @param <Y> the second list element type
     * @param <Z> the result element type
     * @param f the combining function
     * @param lst1 the first list
     * @param lst2 the second list
     * @return a list of elements created by applying the function to pairs
     */
    public static <X, Y, Z> List<Z> apply(BiFunction<X, Y, Z> f, List<X> lst1, List<Y> lst2) {
        ConsList<Z> reversed = ConsList.empty();
        Iterator<X> it1 = lst1.iterator();
        Iterator<Y> it2 = lst2.iterator();
        while (it1.hasNext() && it2.hasNext()) {
            reversed = ConsList.cons(f.apply(it1.next(), it2.next()), reversed);
        }
        return reversed.reverse();
    }

    /**
     * Combines two lists using a curried function.
     * @param <X> the first list element type
     * @param <Y> the second list element type
     * @param <Z> the result element type
     * @param f the combining function (curried)
     * @param lst1 the first list
     * @param lst2 the second list
     * @return a list of elements created by applying the function to pairs
     */
    public static <X, Y, Z> List<Z> apply(Function<X, Function<Y, Z>> f, List<X> lst1, List<Y> lst2) {
        return apply((x, y) -> f.apply(x).apply(y), lst1, lst2);
    }
}

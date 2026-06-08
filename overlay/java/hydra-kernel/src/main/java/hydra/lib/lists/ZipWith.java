package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.ConsList;
import hydra.util.Optional;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import hydra.typing.InferenceContext;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Zips two lists with a function.
 */
public class ZipWith extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.zipWith");
    }

    @Override
    public TypeScheme type() {
        return new hydra.core.TypeScheme(
                Arrays.asList(new hydra.core.Name("a"), new hydra.core.Name("b"), new hydra.core.Name("c")),
                function(function(Types.var("a"), Types.var("b"), Types.var("c")), list("a"), list("b"), list("c")),
                Optional.none());
    }

    @Override
    protected Function<List<Term>, Function<InferenceContext, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.Core.list(graph, args.get(1)), lst1 ->
                hydra.lib.eithers.Bind.apply(hydra.extract.Core.list(graph, args.get(2)), lst2 -> {
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

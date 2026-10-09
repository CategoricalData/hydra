package hydra.core.overlay.java.lib.lists;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Traverse a list in the list monad: for each element of xs, f(x) gives the list of choices for
 * that position; the result is every combination obtainable by choosing one element from f(x)
 * for each x in xs, each result list having the same length as xs.
 */
public class MapList extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.Lists.mapList().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("x", "y",
            function(
                function(Types.var("x"), list("y")),
                list("x"),
                list(list("y"))));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> {
            Term f = args.get(0);
            Either<Error_, List<Term>> xsResult = hydra.core.extract.Model.list(graph, args.get(1));
            if (xsResult.isLeft()) return (Either) xsResult;
            List<Term> xs = ((Either.Right<Error_, List<Term>>) xsResult).value;

            List<List<Term>> choicesPerElement = new ArrayList<>();
            for (Term x : xs) {
                Either<Error_, Term> r = hydra.core.Reduction.reduceTerm(
                    hydra.core.Lexical.emptyInferenceContext(), graph, true, Terms.apply(f, x));
                if (r.isLeft()) return (Either) r;
                Either<Error_, List<Term>> choices = hydra.core.extract.Model.list(graph,
                    ((Either.Right<Error_, Term>) r).value);
                if (choices.isLeft()) return (Either) choices;
                choicesPerElement.add(((Either.Right<Error_, List<Term>>) choices).value);
            }

            List<List<Term>> combinations = cartesianProduct(choicesPerElement);
            List<Term> result = new ArrayList<>();
            for (List<Term> combo : combinations) {
                result.add(Terms.list(combo));
            }
            return Either.right(Terms.list(result));
        };
    }

    /**
     * Traverse a list in the list monad.
     * @param <A> the input element type
     * @param <B> the choice element type
     * @param f the list-returning function to apply to each element
     * @param xs the list to traverse
     * @return every combination obtainable by choosing one element from f(x) for each x in xs
     */
    public static <A, B> List<List<B>> apply(Function<A, List<B>> f, List<A> xs) {
        List<List<B>> choicesPerElement = new ArrayList<>();
        for (A x : xs) {
            choicesPerElement.add(f.apply(x));
        }
        return cartesianProduct(choicesPerElement);
    }

    /**
     * Computes the Cartesian product of a list of choice-lists, in lexicographic order with the
     * choice for the first element varying slowest. If any choice-list is empty, the result is
     * empty; for an empty input the result is the single-element list containing the empty list.
     */
    static <T> List<List<T>> cartesianProduct(List<List<T>> choicesPerElement) {
        List<List<T>> combinations = new ArrayList<>();
        combinations.add(new ArrayList<>());
        for (List<T> choices : choicesPerElement) {
            List<List<T>> next = new ArrayList<>();
            for (List<T> combo : combinations) {
                for (T choice : choices) {
                    List<T> extended = new ArrayList<>(combo);
                    extended.add(choice);
                    next.add(extended);
                }
            }
            combinations = next;
        }
        return combinations;
    }
}

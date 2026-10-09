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
import java.util.Set;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.set;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;
import hydra.core.overlay.java.util.PersistentSet;


/**
 * Traverse a set in the list monad: mapSet(f, s) returns every set obtainable by choosing one
 * element from f(x) for each element x of s. Elements of s are traversed in ascending order; a
 * result set may have fewer elements than s when distinct choices coincide.
 */
public class MapSet extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.Lists.mapSet().name;
    }

    @Override
    public TypeScheme type() {
        return Types.constrained2("x", Types.ORD, "y", Types.ORD,
            function(
                function(Types.var("x"), list("y")),
                set("x"),
                list(set("y"))));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> {
            Term f = args.get(0);
            Either<Error_, Set<Term>> sResult = hydra.core.extract.Model.set(graph, args.get(1));
            if (sResult.isLeft()) return (Either) sResult;
            Set<Term> s = ((Either.Right<Error_, Set<Term>>) sResult).value;

            List<List<Term>> choicesPerElement = new ArrayList<>();
            for (Term x : s) {
                Either<Error_, Term> r = hydra.core.Reduction.reduceTerm(
                    hydra.core.Lexical.emptyInferenceContext(), graph, true, Terms.apply(f, x));
                if (r.isLeft()) return (Either) r;
                Either<Error_, List<Term>> choices = hydra.core.extract.Model.list(graph,
                    ((Either.Right<Error_, Term>) r).value);
                if (choices.isLeft()) return (Either) choices;
                choicesPerElement.add(((Either.Right<Error_, List<Term>>) choices).value);
            }

            List<List<Term>> combinations = MapList.cartesianProduct(choicesPerElement);
            List<Term> result = new ArrayList<>();
            for (List<Term> combo : combinations) {
                PersistentSet<Term> resultSet = PersistentSet.<Term>empty();
                for (Term y : combo) {
                    resultSet = resultSet.insert(y);
                }
                result.add(Terms.set(resultSet));
            }
            return Either.right(Terms.list(result));
        };
    }

    /**
     * Traverse a set in the list monad.
     * @param <A> the input element type
     * @param <B> the choice element type
     * @param f the list-returning function to apply to each element
     * @param s the set to traverse
     * @return every set obtainable by choosing one element from f(x) for each element x of s,
     *         in ascending-order traversal and lexicographic result order
     */
    public static <A, B extends Comparable<B>> List<Set<B>> apply(Function<A, List<B>> f, Set<A> s) {
        List<List<B>> choicesPerElement = new ArrayList<>();
        for (A x : s) {
            choicesPerElement.add(f.apply(x));
        }
        List<List<B>> combinations = MapList.cartesianProduct(choicesPerElement);
        List<Set<B>> result = new ArrayList<>();
        for (List<B> combo : combinations) {
            PersistentSet<B> resultSet = PersistentSet.<B>empty();
            for (B y : combo) {
                resultSet = resultSet.insert(y);
            }
            result.add(resultSet);
        }
        return result;
    }
}

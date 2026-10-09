package hydra.core.overlay.java.lib.lists;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.ConsList;
import hydra.core.overlay.java.util.Either;


/**
 * Left-fold a list in the list monad (a nondeterministic fold): after each element, the set of
 * accumulators is replaced by all results of applying f to each current accumulator and that
 * element.
 */
public class FoldList extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.Lists.foldList().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("x", "y",
            function(
                function(Types.var("x"), function(Types.var("y"), list("x"))),
                Types.var("x"),
                list("y"),
                list("x")));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> {
            Term f = args.get(0);
            Term acc0 = args.get(1);
            Either<Error_, List<Term>> xsResult = hydra.core.extract.Model.list(graph, args.get(2));
            if (xsResult.isLeft()) return (Either) xsResult;
            List<Term> xs = ((Either.Right<Error_, List<Term>>) xsResult).value;

            ConsList<Term> accs = ConsList.cons(acc0, ConsList.empty());
            for (Term el : xs) {
                ConsList<Term> nextReversed = ConsList.empty();
                for (Term acc : accs.reverse()) {
                    Either<Error_, Term> r = hydra.core.Reduction.reduceTerm(
                        hydra.core.Lexical.emptyInferenceContext(), graph, true,
                        Terms.apply(Terms.apply(f, acc), el));
                    if (r.isLeft()) return (Either) r;
                    Either<Error_, List<Term>> branch = hydra.core.extract.Model.list(graph,
                        ((Either.Right<Error_, Term>) r).value);
                    if (branch.isLeft()) return (Either) branch;
                    for (Term nextAcc : ((Either.Right<Error_, List<Term>>) branch).value) {
                        nextReversed = ConsList.cons(nextAcc, nextReversed);
                    }
                }
                accs = nextReversed;
            }
            return Either.right(Terms.list(accs.reverse()));
        };
    }

    /**
     * Left-fold a list in the list monad.
     * @param <A> the accumulator type
     * @param <B> the element type
     * @param f the branching step function
     * @param acc0 the initial accumulator
     * @param xs the list to fold
     * @return the list of all accumulators reachable by branching over every element
     */
    public static <A, B> List<A> apply(BiFunction<A, B, List<A>> f, A acc0, List<B> xs) {
        List<A> accs = new java.util.ArrayList<>();
        accs.add(acc0);
        for (B el : xs) {
            List<A> next = new java.util.ArrayList<>();
            for (A acc : accs) {
                next.addAll(f.apply(acc, el));
            }
            accs = next;
        }
        return accs;
    }
}

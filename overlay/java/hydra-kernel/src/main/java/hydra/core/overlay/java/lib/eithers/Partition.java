package hydra.core.overlay.java.lib.eithers;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;
import hydra.core.overlay.java.util.ConsList;
import hydra.core.overlay.java.util.Pair;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.either;
import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.pair;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.var;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Partition a list of Eithers into separate lists of Left and Right values.
 */
public class Partition extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.core.lib.eithers.partition");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b",
            function(list(either(var("a"), var("b"))), pair(list(var("a")), list(var("b")))));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply((java.util.List<hydra.core.overlay.java.util.Either<Term, Term>> eithers) -> {
                Pair<List<Term>, List<Term>> p = apply(eithers);
                return Terms.pair(new Term.List(p.first), new Term.List(p.second));
            }, hydra.core.extract.Model.listOf(arg -> hydra.core.extract.Model.eitherTerm(t -> Either.right(t), t -> Either.right(t), graph, arg), graph, args.get(0)));
    }

    /**
     * Partition a list of Eithers into separate lists of Left and Right values.
     *
     * @param <A> the left type
     * @param <B> the right type
     * @param eithers the list of Either values to partition
     * @return a tuple containing the list of Left values and the list of Right values
     */
    public static <A, B> Pair<List<A>, List<B>> apply(List<hydra.core.overlay.java.util.Either<A, B>> eithers) {
        ConsList<A> leftsRev = ConsList.empty();
        ConsList<B> rightsRev = ConsList.empty();
        for (hydra.core.overlay.java.util.Either<A, B> either : eithers) {
            if (either.isLeft()) {
                leftsRev = ConsList.cons(((hydra.core.overlay.java.util.Either.Left<A, B>) either).value, leftsRev);
            } else {
                rightsRev = ConsList.cons(((hydra.core.overlay.java.util.Either.Right<A, B>) either).value, rightsRev);
            }
        }
        return new Pair<>(leftsRev.reverse(), rightsRev.reverse());
    }
}

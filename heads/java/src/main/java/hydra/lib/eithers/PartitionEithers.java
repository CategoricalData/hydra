package hydra.lib.eithers;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.ConsList;
import hydra.util.Pair;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.either;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.pair;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.var;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Partition a list of Eithers into separate lists of Left and Right values.
 */
public class PartitionEithers extends PrimitiveFunction {
    public static final Name NAME = new Name("hydra.lib.eithers.partitionEithers");

    public Name name() {
        return NAME;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", "b",
            function(list(either(var("a"), var("b"))), pair(list(var("a")), list(var("b")))));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((java.util.List<hydra.util.Either<Term, Term>> eithers) -> {
                Pair<List<Term>, List<Term>> p = apply(eithers);
                return Terms.pair(new Term.List(p.first), new Term.List(p.second));
            }, hydra.extract.Core.listOf(arg -> hydra.extract.Core.eitherTerm(t -> Either.right(t), t -> Either.right(t), graph, arg), graph, args.get(0)));
    }

    /**
     * Partition a list of Eithers into separate lists of Left and Right values.
     *
     * @param <A> the left type
     * @param <B> the right type
     * @param eithers the list of Either values to partition
     * @return a tuple containing the list of Left values and the list of Right values
     */
    public static <A, B> Pair<List<A>, List<B>> apply(List<hydra.util.Either<A, B>> eithers) {
        ConsList<A> leftsRev = ConsList.empty();
        ConsList<B> rightsRev = ConsList.empty();
        for (hydra.util.Either<A, B> either : eithers) {
            if (either.isLeft()) {
                leftsRev = ConsList.cons(((hydra.util.Either.Left<A, B>) either).value, leftsRev);
            } else {
                rightsRev = ConsList.cons(((hydra.util.Either.Right<A, B>) either).value, rightsRev);
            }
        }
        return new Pair<>(leftsRev.reverse(), rightsRev.reverse());
    }
}

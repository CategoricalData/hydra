package hydra.lib.eithers;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Pair;

import hydra.util.ConsList;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.either;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.pair;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.var;
import hydra.context.Context;
import hydra.context.InContext;
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((ConsList<hydra.util.Either<Term, Term>> eithers) -> {
                ArrayList<Term> lefts = new ArrayList<>();
                ArrayList<Term> rights = new ArrayList<>();
                for (hydra.util.Either<Term, Term> e : eithers) {
                    e.accept(new hydra.util.Either.Visitor<Term, Term, Void>() {
                        @Override
                        public Void visit(hydra.util.Either.Left<Term, Term> left) {
                            lefts.add(left.value);
                            return null;
                        }

                        @Override
                        public Void visit(hydra.util.Either.Right<Term, Term> right) {
                            rights.add(right.value);
                            return null;
                        }
                    });
                }
                return Terms.pair(new Term.List(ConsList.fromList(lefts)), new Term.List(ConsList.fromList(rights)));
            }, hydra.extract.Core.listOf(cx, arg -> hydra.extract.Core.eitherTerm(cx, t -> Either.right(t), t -> Either.right(t), graph, arg), graph, args.get(0)));
    }

    /**
     * Partition a list of Eithers into separate lists of Left and Right values.
     *
     * @param <A> the left type
     * @param <B> the right type
     * @param eithers the list of Either values to partition
     * @return a tuple containing the list of Left values and the list of Right values
     */
    public static <A, B> Pair<ConsList<A>, ConsList<B>> apply(ConsList<hydra.util.Either<A, B>> eithers) {
        ArrayList<A> lefts = new ArrayList<>();
        ArrayList<B> rights = new ArrayList<>();
        for (hydra.util.Either<A, B> either : eithers) {
            if (either.isLeft()) {
                lefts.add(((hydra.util.Either.Left<A, B>) either).value);
            } else {
                rights.add(((hydra.util.Either.Right<A, B>) either).value);
            }
        }
        return new Pair<>(ConsList.fromList(lefts), ConsList.fromList(rights));
    }
}

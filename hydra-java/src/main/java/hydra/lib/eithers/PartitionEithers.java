package hydra.lib.eithers;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Flows;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Tuple;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.either;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.pair;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.var;

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
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(
            Expect.list(arg -> Expect.<Graph, Term, Term>either(arg), args.get(0)),
            eithers -> {
                List<Term> lefts = new ArrayList<>();
                List<Term> rights = new ArrayList<>();
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
                return Terms.pair(new Term.List(lefts), new Term.List(rights));
            });
    }

    /**
     * Partition a list of Eithers into separate lists of Left and Right values.
     *
     * @param <A> the left type
     * @param <B> the right type
     * @param eithers the list of Either values to partition
     * @return a tuple containing the list of Left values and the list of Right values
     */
    public static <A, B> Tuple.Tuple2<List<A>, List<B>> apply(List<hydra.util.Either<A, B>> eithers) {
        List<A> lefts = new ArrayList<>();
        List<B> rights = new ArrayList<>();
        for (hydra.util.Either<A, B> either : eithers) {
            if (either.isLeft()) {
                lefts.add(((hydra.util.Either.Left<A, B>) either).value);
            } else {
                rights.add(((hydra.util.Either.Right<A, B>) either).value);
            }
        }
        return new Tuple.Tuple2<>(lefts, rights);
    }
}

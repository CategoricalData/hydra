package hydra.lib.equality;

import hydra.compute.Flow;
import hydra.core.Term;
import hydra.core.Type;
import hydra.graph.Graph;
import hydra.lib.literals.EqualityFunction;
import java.util.function.Function;

import static hydra.Flows.*;
import static hydra.dsl.Types.*;


public class EqualTerm<A> extends EqualityFunction<A, Term<A>> {
    @Override
    protected String typeName() {
        return "Term";
    }

    @Override
    protected Type<A> datatype() {
        return hydra.dsl.Types.apply(variable(Term.NAME), variable("a"));
    }

    @Override
    protected Flow<Graph<A>, Term<A>> expect(Term<A> term) {
        return pure(term);
    }

    @Override
    protected boolean checkEqual(Term<A> first, Term<A> second) {
        return apply(first, second);
    }

    public static <A> Function<Term<A>, Boolean> apply(Term<A> second) {
        return first -> apply(first, second);
    }

    public static <A> Boolean apply(Term<A> first, Term<A> second) {
        return first.equals(second);
    }
}

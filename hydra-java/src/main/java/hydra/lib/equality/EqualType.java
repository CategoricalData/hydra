package hydra.lib.equality;

import hydra.compute.Flow;
import hydra.core.Term;
import hydra.core.Type;
import hydra.graph.Graph;
import java.util.function.Function;

import static hydra.dsl.Types.*;


public class EqualType<A> extends EqualityFunction<A, Type<A>> {
    @Override
    protected String typeName() {
        return "Type";
    }

    @Override
    protected Type<A> datatype() {
        return hydra.dsl.Types.apply(variable(Type.NAME), variable("a"));
    }

    @Override
    protected Flow<Graph<A>, Type<A>> expect(Term<A> encoded) {
        throw new UnsupportedOperationException(); // TODO: implement epsilon decoding
    }

    @Override
    protected boolean checkEqual(Type<A> first, Type<A> second) {
        return apply(first, second);
    }

    public static <A> Function<Type<A>, Boolean> apply(Type<A> second) {
        return first -> apply(first, second);
    }

    public static <A> Boolean apply(Type<A> first, Type<A> second) {
        return first.equals(second);
    }
}

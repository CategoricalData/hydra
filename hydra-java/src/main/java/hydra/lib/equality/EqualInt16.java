package hydra.lib.equality;

import hydra.compute.Flow;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.graph.Graph;
import java.util.function.Function;

import static hydra.dsl.Types.*;


public class EqualInt16<A> extends EqualityFunction<A, Short> {
    @Override
    protected String typeName() {
        return "Int16";
    }

    @Override
    protected Type<A> datatype() {
        return int16();
    }

    @Override
    protected Flow<Graph<A>, Short> expect(Term<A> term) {
        return Expect.int16(term);
    }

    @Override
    protected boolean checkEqual(Short first, Short second) {
        return apply(first, second);
    }

    public static Function<Short, Boolean> apply(Short second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Short first, Short second) {
        return 0 == first.compareTo(second);
    }
}

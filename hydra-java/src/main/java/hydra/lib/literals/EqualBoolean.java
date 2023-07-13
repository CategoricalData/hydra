package hydra.lib.literals;

import hydra.compute.Flow;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.graph.Graph;
import java.util.function.Function;

import static hydra.dsl.Types.*;


public class EqualBoolean<A> extends EqualityFunction<A, Boolean> {
    @Override
    protected String typeName() {
        return "Boolean";
    }

    @Override
    protected Type<A> datatype() {
        return boolean_();
    }

    @Override
    protected Flow<Graph<A>, Boolean> expect(Term<A> term) {
        return Expect.boolean_(term);
    }

    @Override
    protected boolean checkEqual(Boolean first, Boolean second) {
        return apply(first, second);
    }

    public static Function<Boolean, Boolean> apply(Boolean second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Boolean first, Boolean second) {
        return 0 == first.compareTo(second);
    }
}

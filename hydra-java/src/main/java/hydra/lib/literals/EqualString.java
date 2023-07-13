package hydra.lib.literals;

import hydra.compute.Flow;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.graph.Graph;
import java.util.function.Function;

import static hydra.dsl.Types.*;


public class EqualString<A> extends EqualityFunction<A, String> {
    @Override
    protected String typeName() {
        return "String";
    }

    @Override
    protected Type<A> datatype() {
        return string();
    }

    @Override
    protected Flow<Graph<A>, String> expect(Term<A> term) {
        return Expect.string(term);
    }

    @Override
    protected boolean checkEqual(String first, String second) {
        return apply(first, second);
    }

    public static Function<String, Boolean> apply(String second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(String first, String second) {
        return 0 == first.compareTo(second);
    }
}

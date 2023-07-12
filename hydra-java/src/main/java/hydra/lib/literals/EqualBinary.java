package hydra.lib.literals;

import hydra.compute.Flow;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.graph.Graph;
import java.util.function.Function;

import static hydra.dsl.Types.*;


public class EqualBinary<A> extends EqualityFunction<A, String> {
    @Override
    protected String typeName() {
        return "Binary";
    }

    @Override
    protected Type<A> literalType() {
        return binary();
    }

    @Override
    protected Flow<Graph<A>, String> expect(Term<A> term) {
        return Expect.binary(term);
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

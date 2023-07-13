package hydra.lib.literals;

import hydra.compute.Flow;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.graph.Graph;
import java.util.function.Function;

import static hydra.dsl.Types.*;


public class EqualUint16<A> extends EqualityFunction<A, Character> {
    @Override
    protected String typeName() {
        return "Uint16";
    }

    @Override
    protected Type<A> datatype() {
        return uint16();
    }

    @Override
    protected Flow<Graph<A>, Character> expect(Term<A> term) {
        return Expect.uint16(term);
    }

    @Override
    protected boolean checkEqual(Character first, Character second) {
        return apply(first, second);
    }

    public static Function<Character, Boolean> apply(Character second) {
        return first -> apply(first, second);
    }

    public static Boolean apply(Character first, Character second) {
        return 0 == first.compareTo(second);
    }
}

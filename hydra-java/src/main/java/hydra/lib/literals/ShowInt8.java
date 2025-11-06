package hydra.lib.literals;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Flows;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int8;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;


/**
 * Primitive function: ShowInt8.
 */
public class ShowInt8 extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.literals.showInt8");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(int8(), string()));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.int8(args.get(0)),
            (Function<Byte, Term>) b -> Terms.string(apply(b)));
    }

    /**
     * Applies the ShowInt8 operation.
     * @param value the value
     * @return the result
     */
        public static String apply(Byte value) {
        return Byte.toString(value);
    }
}

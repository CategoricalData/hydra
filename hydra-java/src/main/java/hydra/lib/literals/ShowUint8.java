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
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import static hydra.dsl.Types.uint8;


/**
 * Primitive function: ShowUint8.
 */
public class ShowUint8 extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.literals.showUint8");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(uint8(), string()));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.int16(args.get(0)),
            (Function<Short, Term>) s -> Terms.string(apply(s)));
    }

    /**
     * Applies the ShowUint8 operation.
     * @param value the value
     * @return the result
     */
        public static String apply(Short value) {
        return Short.toString(value);
    }
}

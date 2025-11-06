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

import static hydra.dsl.Types.float32;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;


/**
 * Primitive function: ShowFloat32.
 */
public class ShowFloat32 extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.literals.showFloat32");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(float32(), string()));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.float32(args.get(0)),
            (Function<Float, Term>) f -> Terms.string(apply(f)));
    }

    /**
     * Applies the ShowFloat32 operation.
     * @param value the value
     * @return the result
     */
        public static String apply(Float value) {
        return Float.toString(value);
    }
}

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

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;


/**
 * Primitive function: ShowBoolean.
 */
public class ShowBoolean extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.literals.showBoolean");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(boolean_(), string()));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.boolean_(args.get(0)),
            (Function<Boolean, Term>) b -> Terms.string(apply(b)));
    }

    /**
     * Applies the ShowBoolean operation.
     * @param value the value
     * @return the result
     */
        public static String apply(Boolean value) {
        return value ? "true" : "false";
    }
}

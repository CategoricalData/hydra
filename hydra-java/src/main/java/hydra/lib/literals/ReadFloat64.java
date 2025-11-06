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
import hydra.util.Opt;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.float64;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;


/**
 * Primitive function: ReadFloat64.
 */
public class ReadFloat64 extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.literals.readFloat64");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(string(), optional(float64())));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.string(args.get(0)),
            (Function<String, Term>) s -> Terms.optional(apply(s).map(Terms::float64)));
    }

    /**
     * Applies the ReadFloat64 operation.
     * @param str the str
     * @return the result
     */
        public static Opt<Double> apply(String str) {
        try {
            return Opt.of(Double.parseDouble(str));
        } catch (NumberFormatException e) {
            return Opt.empty();
        }
    }
}

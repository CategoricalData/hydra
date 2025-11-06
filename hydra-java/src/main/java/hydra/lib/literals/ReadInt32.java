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

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;


/**
 * Primitive function: ReadInt32.
 */
public class ReadInt32 extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.literals.readInt32");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(string(), optional(int32())));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.string(args.get(0)),
            (Function<String, Term>) s -> Terms.optional(apply(s).map(Terms::int32)));
    }

    /**
     * Applies the ReadInt32 operation.
     * @param str the str
     * @return the result
     */
        public static Opt<Integer> apply(String str) {
        try {
            return Opt.of(Integer.parseInt(str));
        } catch (NumberFormatException e) {
            return Opt.empty();
        }
    }
}

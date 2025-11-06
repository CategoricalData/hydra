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

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;


/**
 * Primitive function: ReadBoolean.
 */
public class ReadBoolean extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.literals.readBoolean");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(string(), optional(boolean_())));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.string(args.get(0)),
            (Function<String, Term>) s -> Terms.optional(apply(s).map(Terms::boolean_)));
    }

    /**
     * Applies the ReadBoolean operation.
     * @param str the str
     * @return the result
     */
        public static Opt<Boolean> apply(String str) {
        if ("true".equals(str)) {
            return Opt.of(true);
        } else if ("false".equals(str)) {
            return Opt.of(false);
        } else {
            return Opt.empty();
        }
    }
}

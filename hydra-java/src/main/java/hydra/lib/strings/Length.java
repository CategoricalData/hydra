package hydra.lib.strings;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import java.util.List;
import java.util.function.Function;

import static hydra.Flows.map;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.string;


public class Length extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra/lib/strings.length");
    }

    @Override
    public Type type() {
        return function(string(), int32());
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> map(Expect.string(args.get(0)), s -> Terms.int32(apply(s)));
    }

    public static int apply(String s) {
        return s.length();
    }
}

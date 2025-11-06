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

import static hydra.dsl.Types.binary;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;


/**
 * Primitive function: StringToBinary.
 */
public class StringToBinary extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.literals.stringToBinary");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(string(), binary()));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.string(args.get(0)),
            (Function<String, Term>) s -> Terms.string(apply(s)));
    }

    /**
     * Applies the StringToBinary operation.
     * @param str the str
     * @return the result
     */
        public static String apply(String str) {
        return str;
    }
}

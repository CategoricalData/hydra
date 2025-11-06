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
 * Primitive function: BinaryToString.
 */
public class BinaryToString extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.literals.binaryToString");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(binary(), string()));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.string(args.get(0)),
            (Function<String, Term>) s -> Terms.string(apply(s)));
    }

    /**
     * Applies the BinaryToString operation.
     * @param binary the binary
     * @return the result
     */
        public static String apply(String binary) {
        return binary;
    }
}

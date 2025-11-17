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
 * Primitive function which converts a string to binary data.
 * This is currently an identity function as both types are represented as String.
 */
public class StringToBinary extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.stringToBinary"
     */
    public Name name() {
        return new Name("hydra.lib.literals.stringToBinary");
    }

    /**
     * Returns the type scheme for this function: string -&gt; binary.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(string(), binary()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts string terms to binary terms
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.string(args.get(0)),
            (Function<String, Term>) s -> Terms.string(apply(s)));
    }

    /**
     * Converts a string to binary data (identity function).
     * @param str the string to convert
     * @return the same string value
     */
    public static String apply(String str) {
        return str;
    }
}

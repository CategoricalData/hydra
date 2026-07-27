package hydra.overlay.java.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.binary;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.string;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


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
        return hydra.lib.Literals.stringToBinary().name;
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
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply((Function<String, Term>) s -> new Term.Literal(new hydra.core.Literal.Binary(apply(s))), hydra.extract.Core.string(graph, args.get(0)));
    }

    /**
     * Converts a base64-encoded string to binary data.
     * @param str the base64-encoded string to convert
     * @return the decoded byte array
     */
    public static byte[] apply(String str) {
        return java.util.Base64.getDecoder().decode(str);
    }
}

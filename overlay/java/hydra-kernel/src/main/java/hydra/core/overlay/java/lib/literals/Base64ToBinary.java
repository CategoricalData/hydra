package hydra.core.overlay.java.lib.literals;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.binary;
import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.string;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Primitive function which decodes a base64 string to binary data.
 * This is currently an identity function as both types are represented as String.
 */
public class Base64ToBinary extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.core.lib.literals.base64ToBinary"
     */
    public Name name() {
        return hydra.core.lib.Literals.base64ToBinary().name;
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
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply((Function<String, Term>) s -> new Term.Literal(new hydra.core.model.Literal.Binary(apply(s))), hydra.core.extract.Model.string(graph, args.get(0)));
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

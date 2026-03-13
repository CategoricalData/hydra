package hydra.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.binary;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.error.OtherError;
import hydra.util.Either;


/**
 * Primitive function which converts binary data to a string.
 * This is currently an identity function as both types are represented as String.
 */
public class BinaryToString extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.binaryToString"
     */
    public Name name() {
        return new Name("hydra.lib.literals.binaryToString");
    }

    /**
     * Returns the type scheme for this function: binary -&gt; string.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(binary(), string()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts binary terms to string terms
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> {
            Term term = args.get(0);
            if (term instanceof Term.Literal) {
                hydra.core.Literal lit = ((Term.Literal) term).value;
                if (lit instanceof hydra.core.Literal.Binary) {
                    byte[] bytes = ((hydra.core.Literal.Binary) lit).value;
                    return Either.right(Terms.string(apply(bytes)));
                }
            }
            return Either.left(new InContext<>(new Error_.Other(new OtherError("expected binary literal")), cx));
        };
    }

    /**
     * Converts binary data to a base64-encoded string.
     * @param binary the binary data as a byte array
     * @return the base64-encoded string
     */
    public static String apply(byte[] binary) {
        return java.util.Base64.getEncoder().encodeToString(binary);
    }
}

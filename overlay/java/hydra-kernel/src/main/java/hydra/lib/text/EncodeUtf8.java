package hydra.lib.text;

import hydra.core.Literal;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.binary;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Encode text as a sequence of UTF-8 bytes.
 * Pure and total: every Hydra string is valid Unicode and therefore always encodes.
 */
public class EncodeUtf8 extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.text.encodeUtf8"
     */
    public Name name() {
        return hydra.lib.Text.encodeUtf8().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme string -&gt; binary
     */
    @Override
    public TypeScheme type() {
        return scheme(function(string(), binary()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.lib.eithers.Map.apply(
            text -> Terms.literal(new Literal.Binary(EncodeUtf8.apply(text))),
            hydra.extract.Core.string(graph, args.get(0)));
    }

    /**
     * Encode a string as UTF-8 bytes.
     * @param text the text to encode
     * @return the UTF-8 encoding of text as raw bytes
     */
    public static byte[] apply(String text) {
        return text.getBytes(StandardCharsets.UTF_8);
    }
}

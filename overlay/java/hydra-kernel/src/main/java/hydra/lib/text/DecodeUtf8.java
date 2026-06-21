package hydra.lib.text;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.binary;
import static hydra.dsl.Types.either;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Decode a sequence of bytes as UTF-8 text.
 * Pure and partial: invalid UTF-8 yields left(error message), a successful decode yields right(text).
 */
public class DecodeUtf8 extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.text.decodeUtf8"
     */
    public Name name() {
        return hydra.lib.Text.decodeUtf8().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme binary -&gt; either&lt;string, string&gt;
     */
    @Override
    public TypeScheme type() {
        return scheme(function(binary(), either(string(), string())));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.lib.eithers.Map.apply(
            bytes -> DecodeUtf8.apply(bytes).accept(new Either.Visitor<String, String, Term>() {
                @Override
                public Term visit(Either.Left<String, String> left) {
                    return Terms.left(Terms.string(left.value));
                }
                @Override
                public Term visit(Either.Right<String, String> right) {
                    return Terms.right(Terms.string(right.value));
                }
            }),
            hydra.extract.Core.binary(graph, args.get(0)));
    }

    /**
     * Attempt to decode raw bytes as UTF-8 text using a strict decoder.
     * @param bytes the bytes to decode
     * @return right(text) on success, or left(error message) when the bytes are not valid UTF-8
     */
    public static Either<String, String> apply(byte[] bytes) {
        CharsetDecoder decoder = StandardCharsets.UTF_8.newDecoder()
            .onMalformedInput(CodingErrorAction.REPORT)
            .onUnmappableCharacter(CodingErrorAction.REPORT);
        try {
            return Either.right(decoder.decode(ByteBuffer.wrap(bytes)).toString());
        } catch (CharacterCodingException e) {
            String message = e.getMessage();
            return Either.left(message == null ? e.toString() : message);
        }
    }
}

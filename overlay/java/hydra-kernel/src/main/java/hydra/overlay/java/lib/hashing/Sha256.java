package hydra.overlay.java.lib.hashing;

import hydra.core.Literal;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.binary;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Compute the SHA-256 digest of a sequence of bytes.
 * Pure and total: hashing never fails.
 */
public class Sha256 extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.hashing.sha256"
     */
    public Name name() {
        return hydra.lib.Hashing.sha256().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme binary -&gt; binary
     */
    @Override
    public TypeScheme type() {
        return scheme(function(binary(), binary()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply(
            bytes -> Terms.literal(new Literal.Binary(Sha256.apply(bytes))),
            hydra.extract.Core.binary(graph, args.get(0)));
    }

    /**
     * Compute the SHA-256 digest of raw bytes.
     * @param bytes the bytes to hash
     * @return the 32-byte SHA-256 digest
     */
    public static byte[] apply(byte[] bytes) {
        try {
            return MessageDigest.getInstance("SHA-256").digest(bytes);
        } catch (NoSuchAlgorithmException e) {
            // SHA-256 is a required algorithm on every JRE; its absence is not a recoverable condition.
            throw new IllegalStateException("SHA-256 not available", e);
        }
    }
}

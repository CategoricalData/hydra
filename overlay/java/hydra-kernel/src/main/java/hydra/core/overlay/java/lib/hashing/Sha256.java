package hydra.core.overlay.java.lib.hashing;

import hydra.core.model.Literal;
import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.binary;
import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Compute the SHA-256 digest of a sequence of bytes.
 * Pure and total: hashing never fails.
 */
public class Sha256 extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.core.lib.hashing.sha256"
     */
    public Name name() {
        return hydra.core.lib.Hashing.sha256().name;
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
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply(
            bytes -> Terms.literal(new Literal.Binary(Sha256.apply(bytes))),
            hydra.core.extract.Model.binary(graph, args.get(0)));
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

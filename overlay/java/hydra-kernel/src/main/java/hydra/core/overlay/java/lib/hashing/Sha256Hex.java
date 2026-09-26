package hydra.core.overlay.java.lib.hashing;

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
import static hydra.core.overlay.java.dsl.Types.string;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Compute the SHA-256 digest of a sequence of bytes as a lowercase hex string.
 * Pure and total: hashing never fails.
 */
public class Sha256Hex extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.core.lib.hashing.sha256Hex"
     */
    public Name name() {
        return hydra.core.lib.Hashing.sha256Hex().name;
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme binary -&gt; string
     */
    @Override
    public TypeScheme type() {
        return scheme(function(binary(), string()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply(
            bytes -> Terms.string(Sha256Hex.apply(bytes)),
            hydra.core.extract.Model.binary(graph, args.get(0)));
    }

    /**
     * Compute the SHA-256 digest of raw bytes as a 64-character lowercase hex string.
     * @param bytes the bytes to hash
     * @return the SHA-256 digest as lowercase hexadecimal
     */
    public static String apply(byte[] bytes) {
        byte[] digest;
        try {
            digest = MessageDigest.getInstance("SHA-256").digest(bytes);
        } catch (NoSuchAlgorithmException e) {
            // SHA-256 is a required algorithm on every JRE; its absence is not a recoverable condition.
            throw new IllegalStateException("SHA-256 not available", e);
        }
        StringBuilder sb = new StringBuilder(digest.length * 2);
        for (byte b : digest) {
            sb.append(Character.forDigit((b >> 4) & 0xf, 16));
            sb.append(Character.forDigit(b & 0xf, 16));
        }
        return sb.toString();
    }
}

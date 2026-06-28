package hydra.overlay.java.util;
import hydra.coders.Coder;

import java.util.function.Function;

/**
 * A convenience class for stateless coders with String errors.
 *
 * @param <V1> the source value type
 * @param <V2> the target value type
 */
public class StatelessCoder<V1, V2> extends Coder<V1, V2, String> {

    /**
     * Construct a stateless coder from simple Either-based encode/decode functions.
     * @param encode the encoding function
     * @param decode the decoding function
     */
    public StatelessCoder(
            Function<V1, Either<String, V2>> encode,
            Function<V2, Either<String, V1>> decode) {
        super(encode, decode);
    }

    /**
     * Construct a stateless coder from simple Either-based encode/decode functions.
     * @param <V1> the source value type
     * @param <V2> the target value type
     * @param encode the encoding function
     * @param decode the decoding function
     * @return a new stateless coder
     */
    public static <V1, V2> StatelessCoder<V1, V2> of(
            Function<V1, Either<String, V2>> encode,
            Function<V2, Either<String, V1>> decode) {
        return new StatelessCoder<>(encode, decode);
    }

    /**
     * Construct a unidirectional coder; encoding follows the provided function, while decoding will fail gracefully.
     * @param <V1> the source value type
     * @param <V2> the target value type
     * @param encode the encoding function
     * @return a new unidirectional coder
     */
    public static <V1, V2> StatelessCoder<V1, V2> unidirectional(
            Function<V1, Either<String, V2>> encode) {
        return StatelessCoder.of(encode, v2 -> Either.left("decoding is not supported"));
    }
}

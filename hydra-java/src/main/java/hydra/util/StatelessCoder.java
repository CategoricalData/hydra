package hydra.util;

import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.error.OtherError;
import hydra.util.Either;

import java.util.function.Function;

/**
 * A convenience class for stateless coders that wraps simple encode/decode functions.
 * Since Coder no longer has state type parameters, this class provides convenience
 * constructors that accept simple functions and adapt them to the Coder signature
 * (which takes a Context parameter and returns Either).
 *
 * @param <V1> the source value type
 * @param <V2> the target value type
 */
public class StatelessCoder<V1, V2> extends Coder<V1, V2> {

    /**
     * Construct a stateless coder from simple Either-based encode/decode functions.
     * The Context parameter is ignored.
     * @param encode the encoding function
     * @param decode the decoding function
     */
    public StatelessCoder(
            Function<V1, Either<String, V2>> encode,
            Function<V2, Either<String, V1>> decode) {
        super(toCoderFn(encode), toCoderFn(decode));
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

    /**
     * Convert a simple Either-based function to the Coder encode/decode signature.
     * The Context parameter is ignored; Either Left strings are wrapped in InContext Error_.
     */
    private static <A, B> Function<Context, Function<A, Either<InContext<Error_>, B>>>
            toCoderFn(Function<A, Either<String, B>> fn) {
        return cx -> a -> {
            Either<String, B> result = fn.apply(a);
            if (result.isRight()) {
                return Either.right(((Either.Right<String, B>) result).value);
            } else {
                String msg = ((Either.Left<String, B>) result).value;
                return Either.left(new InContext<>(new Error_.Other(new OtherError(msg)), cx));
            }
        };
    }
}

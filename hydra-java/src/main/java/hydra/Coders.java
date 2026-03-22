package hydra;

import hydra.util.Coder;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;

import java.util.Collections;
import java.util.function.Function;


/**
 * Convenience functions for working with Hydra coders.
 */
public class Coders {
    /**
     * Compose two coders, chaining their encode and decode functions.
     * @param <V1> the first value type
     * @param <V2> the intermediate value type
     * @param <V3> the final value type
     * @param coder1 the first coder
     * @param coder2 the second coder
     * @return the composed coder
     */
    public static <V1, V2, V3> Coder<V1, V3> compose(
            Coder<V1, V2> coder1,
            Coder<V2, V3> coder2) {
        return new Coder<V1, V3>(
                composeEncode(coder1.encode, coder2.encode),
                composeEncode(coder2.decode, coder1.decode));
    }

    /**
     * Compose two stateless coders, chaining their encode and decode functions.
     * @param <V1> the first value type
     * @param <V2> the intermediate value type
     * @param <V3> the final value type
     * @param coder1 the first stateless coder
     * @param coder2 the second stateless coder
     * @return the composed stateless coder
     */
    public static <V1, V2, V3> Coder<V1, V3> composeStateless(
            Coder<V1, V2> coder1,
            Coder<V2, V3> coder2) {
        return compose(coder1, coder2);
    }

    /**
     * Find the inverse of a coder.
     * @param <V1> the first value type
     * @param <V2> the second value type
     * @param coder the coder to invert
     * @return the inverted coder
     */
    public static <V1, V2> Coder<V2, V1> inverse(Coder<V1, V2> coder) {
        return new Coder<>(coder.decode, coder.encode);
    }

    /**
     * Find the inverse of a stateless coder.
     * @param <V1> the first value type
     * @param <V2> the second value type
     * @param coder the stateless coder to invert
     * @return the inverted stateless coder
     */
    public static <V1, V2> Coder<V2, V1> inverseStateless(Coder<V1, V2> coder) {
        return new Coder<V2, V1>(coder.decode, coder.encode);
    }

    /**
     * Pass an initial value through the Coder's encode function, then back through the decode function.
     * @param <V1> the initial value type
     * @param <V2> the intermediate value type
     * @param coder the coder to use for encoding and decoding
     * @param initialValue the initial value to encode and decode
     * @return an Either containing the round-tripped value or an error
     */
    public static <V1, V2> Either<String, V1> roundTrip(Coder<V1, V2> coder, V1 initialValue) {
        Context cx = new Context(
            hydra.util.ConsList.empty(),
            hydra.util.ConsList.empty(),
            hydra.util.PersistentMap.empty());
        Either<InContext<Error_>, V2> encResult = coder.encode.apply(cx).apply(initialValue);
        if (encResult.isLeft()) {
            return Either.left(hydra.show.Errors.error(((Either.Left<InContext<Error_>, V2>) encResult).value.object));
        }
        V2 encoded = ((Either.Right<InContext<Error_>, V2>) encResult).value;
        Either<InContext<Error_>, V1> decResult = coder.decode.apply(cx).apply(encoded);
        if (decResult.isLeft()) {
            return Either.left(hydra.show.Errors.error(((Either.Left<InContext<Error_>, V1>) decResult).value.object));
        }
        return Either.right(((Either.Right<InContext<Error_>, V1>) decResult).value);
    }

    /**
     * Compose two Coder-style encode functions.
     */
    private static <A, B, C> Function<Context, Function<A, Either<InContext<Error_>, C>>>
            composeEncode(
                Function<Context, Function<A, Either<InContext<Error_>, B>>> first,
                Function<Context, Function<B, Either<InContext<Error_>, C>>> second) {
        return cx -> a -> {
            Either<InContext<Error_>, B> r1 = first.apply(cx).apply(a);
            return r1.accept(new Either.Visitor<InContext<Error_>, B, Either<InContext<Error_>, C>>() {
                @Override
                public Either<InContext<Error_>, C> visit(Either.Left<InContext<Error_>, B> left) {
                    return Either.left(left.value);
                }

                @Override
                public Either<InContext<Error_>, C> visit(Either.Right<InContext<Error_>, B> right) {
                    return second.apply(cx).apply(right.value);
                }
            });
        };
    }
}

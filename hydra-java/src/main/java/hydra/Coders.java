package hydra;

import hydra.compute.Coder;
import hydra.compute.Flow;
import hydra.compute.StatelessCoder;
import hydra.util.Unit;
import hydra.dsl.Flows;


/**
 * Convenience functions for working with Hydra coders.
 */
public class Coders {
    /**
     * Compose two coders, chaining their encode and decode functions.
     * @param <S1> the first state type
     * @param <S2> the second state type
     * @param <V1> the first value type
     * @param <V2> the intermediate value type
     * @param <V3> the final value type
     * @param coder1 the first coder
     * @param coder2 the second coder
     * @return the composed coder
     */
    public static <S1, S2, V1, V2, V3> Coder<S1, S2, V1, V3> compose(
            Coder<S1, S2, V1, V2> coder1,
            Coder<S1, S2, V2, V3> coder2) {
        return new Coder<S1, S2, V1, V3>(
                Flows.compose(coder1.encode, coder2.encode),
                Flows.compose(coder2.decode, coder1.decode));
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
    public static <V1, V2, V3> StatelessCoder<V1, V3> composeStateless(
            Coder<Unit, Unit, V1, V2> coder1,
            Coder<Unit, Unit, V2, V3> coder2) {
        return new StatelessCoder<>(
                Flows.compose(coder1.encode, coder2.encode),
                Flows.compose(coder2.decode, coder1.decode));
    }

    /**
     * Find the inverse of a coder.
     * @param <S1> the first state type
     * @param <S2> the second state type
     * @param <V1> the first value type
     * @param <V2> the second value type
     * @param coder the coder to invert
     * @return the inverted coder
     */
    public static <S1, S2, V1, V2> Coder<S2, S1, V2, V1> inverse(Coder<S1, S2, V1, V2> coder) {
        return new Coder<>(coder.decode, coder.encode);
    }

    /**
     * Find the inverse of a stateless coder.
     * @param <V1> the first value type
     * @param <V2> the second value type
     * @param coder the stateless coder to invert
     * @return the inverted stateless coder
     */
    public static <V1, V2> StatelessCoder<V2, V1> inverseStateless(Coder<Unit, Unit, V1, V2> coder) {
        return new StatelessCoder<V2, V1>(coder.decode, coder.encode);
    }

    /**
     * Pass an initial value through the Coder's encode function, then back through the decode function.
     * @param <S> the state type
     * @param <V1> the initial value type
     * @param <V2> the intermediate value type
     * @param coder the coder to use for encoding and decoding
     * @param initialValue the initial value to encode and decode
     * @return the result of the round trip
     */
    public static <S, V1, V2> Flow<S, V1> roundTrip(Coder<S, S, V1, V2> coder, V1 initialValue) {
        return Flows.compose(coder.encode, coder.decode).apply(initialValue);
    }
}

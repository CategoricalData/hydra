package hydra.compute;

import hydra.dsl.Flows;
import hydra.util.Unit;

/**
 * A convenience class for stateless coders; we use Hydra's Unit as the state type.
 * Note: using Java's Void is problematic because of the interaction of Optional with null values.
 * @param <V1> the source value type
 * @param <V2> the target value type
 */
public class StatelessCoder<V1, V2> extends Coder<Unit, Unit, V1, V2> {

    /**
     * Construct a stateless coder.
     * @param encode the encoding function
     * @param decode the decoding function
     */
    public StatelessCoder(
            java.util.function.Function<V1, hydra.compute.Flow<Unit, V2>> encode,
            java.util.function.Function<V2, hydra.compute.Flow<Unit, V1>> decode) {
        super(encode, decode);
    }

    /**
     * Construct a stateless coder.
     * @param <V1> the source value type
     * @param <V2> the target value type
     * @param encode the encoding function
     * @param decode the decoding function
     * @return a new stateless coder
     */
    public static <V1, V2> StatelessCoder<V1, V2> of(
            java.util.function.Function<V1, hydra.compute.Flow<Unit, V2>> encode,
            java.util.function.Function<V2, hydra.compute.Flow<Unit, V1>> decode) {
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
            java.util.function.Function<V1, hydra.compute.Flow<Unit, V2>> encode) {
        return StatelessCoder.of(encode, v2 -> Flows.fail("decoding is not supported"));
    }
}

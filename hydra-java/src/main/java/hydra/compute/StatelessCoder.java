package hydra.compute;

import hydra.core.Unit;

/**
 * A convenience class for stateless coders; we use Hydra's Unit as the state type.
 * Note: using Java's Void is problematic because of the interaction of Optional with null values.
 */
public class StatelessCoder<V1, V2> extends Coder<Unit, Unit, V1, V2> {

    public StatelessCoder(
            java.util.function.Function<V1, hydra.compute.Flow<Unit, V2>> encode,
            java.util.function.Function<V2, hydra.compute.Flow<Unit, V1>> decode) {
        super(encode, decode);
    }

    public static <V1, V2> StatelessCoder<V1, V2> of(
            java.util.function.Function<V1, hydra.compute.Flow<Unit, V2>> encode,
            java.util.function.Function<V2, hydra.compute.Flow<Unit, V1>> decode) {
        return new StatelessCoder<V1, V2>(encode, decode);
    }
}

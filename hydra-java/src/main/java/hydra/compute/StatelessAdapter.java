package hydra.compute;

import hydra.util.Unit;

import java.util.function.Function;

/**
 * A convenience class for stateless adapters; we use Hydra's Unit as the state type.
 * Note: using Java's Void is problematic because of the interaction of Optional with null values.
 */
public class StatelessAdapter<T1, T2, V1, V2> extends Adapter<Unit, Unit, T1, T2, V1, V2> {
    public StatelessAdapter(boolean isLossy, T1 source, T2 target, Coder<Unit, Unit, V1, V2> coder) {
        super(isLossy, source, target, coder);
    }

    /**
     * Construct a stateless adapter.
     */
    public static <T1, T2, V1, V2> StatelessAdapter<T1, T2, V1, V2> of(boolean isLossy,
                                                                       T1 source,
                                                                       T2 target,
                                                                       Coder<Unit, Unit, V1, V2> coder) {
        return new StatelessAdapter<T1, T2, V1, V2>(isLossy, source, target, coder);
    }

    /**
     * Construct a unidirectional adapter; encoding follows the provided function, while decoding will fail gracefully.
     */
    public static <T1, T2, V1, V2> StatelessAdapter<T1, T2, V1, V2> unidirectional(
            T1 source, T2 target, Function<V1, Flow<Unit, V2>> encode) {
        return StatelessAdapter.of(true, source, target, StatelessCoder.unidirectional(encode));
    }
}

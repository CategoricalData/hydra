package hydra.compute;

import hydra.core.Unit;

/**
 * A convenience class for stateless adapters; we use Hydra's Unit as the state type
 * Note: using Java's Void is problematic because of the interaction of Optional with null values.
 */
public class StatelessAdapter<T1, T2, V1, V2> extends Adapter<Unit, Unit, T1, T2, V1, V2> {
    public StatelessAdapter(boolean isLossy, T1 source, T2 target, Coder<Unit, Unit, V1, V2> coder) {
        super(isLossy, source, target, coder);
    }

    public static <T1, T2, V1, V2> StatelessAdapter<T1, T2, V1, V2> of(boolean isLossy, T1 source, T2 target, Coder<Unit, Unit, V1, V2> coder) {
        return new StatelessAdapter<T1, T2, V1, V2>(isLossy, source, target, coder);
    }
}

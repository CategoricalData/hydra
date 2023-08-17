package hydra.compute;

/**
 * A convenience class for stateless adapters; we simply use Void as the state type
 */
public class StatelessAdapter<T1, T2, V1, V2> extends Adapter<Void, Void, T1, T2, V1, V2> {
    public StatelessAdapter(boolean isLossy, T1 source, T2 target, StatelessCoder<V1, V2> coder) {
        super(isLossy, source, target, coder);
    }
}

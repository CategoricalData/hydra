package hydra.compute;

/**
 * A convenience class for stateless coders; we simply use Void as the state type
 */
public class StatelessCoder<V1, V2> extends Coder<Void, Void, V1, V2> {

    public StatelessCoder(
            java.util.function.Function<V1, hydra.compute.Flow<Void, V2>> encode,
            java.util.function.Function<V2, hydra.compute.Flow<Void, V1>> decode) {
        super(encode, decode);
    }
}

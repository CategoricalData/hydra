package hydra;

import hydra.coders.Adapter;
import hydra.util.StatelessAdapter;
import hydra.util.Either;

import java.util.function.Function;

/**
 * Convenience functions for working with Hydra adapters.
 */
public class Adapters {
    /**
     * Compose two adapters, in turn composing their coders.
     * @param <T1> the first source type
     * @param <T2> the intermediate target type
     * @param <T3> the final target type
     * @param <V1> the first value type
     * @param <V2> the intermediate value type
     * @param <V3> the final value type
     * @param adapter1 the first adapter
     * @param adapter2 the second adapter
     * @return the composed adapter
     */
    public static <T1, T2, T3, V1, V2, V3> Adapter<T1, T3, V1, V3> compose(
            Adapter<T1, T2, V1, V2> adapter1,
            Adapter<T2, T3, V2, V3> adapter2) {

        return new Adapter<>(
                adapter1.isLossy || adapter2.isLossy,
                adapter1.source,
                adapter2.target,
                Coders.compose(adapter1.coder, adapter2.coder));
    }

    /**
     * Compose two adapters constructed from their source types, in turn composing their coders.
     * @param <T1> the first source type
     * @param <T2> the intermediate target type
     * @param <T3> the final target type
     * @param <V1> the first value type
     * @param <V2> the intermediate value type
     * @param <V3> the final value type
     * @param constructor1 the first adapter constructor
     * @param constructor2 the second adapter constructor
     * @return a function that constructs the composed adapter
     */
    public static <T1, T2, T3, V1, V2, V3> Function<T1, Either<String, Adapter<T1, T3, V1, V3>>> compose(
            Function<T1, Either<String, Adapter<T1, T2, V1, V2>>> constructor1,
            Function<T2, Either<String, Adapter<T2, T3, V2, V3>>> constructor2) {
        return t1 -> {
            Either<String, Adapter<T1, T2, V1, V2>> r1 = constructor1.apply(t1);
            if (r1.isLeft()) return Either.left(((Either.Left<String, Adapter<T1, T2, V1, V2>>) r1).value);
            Adapter<T1, T2, V1, V2> firstMile = ((Either.Right<String, Adapter<T1, T2, V1, V2>>) r1).value;
            Either<String, Adapter<T2, T3, V2, V3>> r2 = constructor2.apply(firstMile.target);
            if (r2.isLeft()) return Either.left(((Either.Left<String, Adapter<T2, T3, V2, V3>>) r2).value);
            Adapter<T2, T3, V2, V3> lastMile = ((Either.Right<String, Adapter<T2, T3, V2, V3>>) r2).value;
            return Either.right(compose(firstMile, lastMile));
        };
    }

    /**
     * Compose two stateless adapters, in turn composing their coders.
     * @param <T1> the first source type
     * @param <T2> the intermediate target type
     * @param <T3> the final target type
     * @param <V1> the first value type
     * @param <V2> the intermediate value type
     * @param <V3> the final value type
     * @param adapter1 the first stateless adapter
     * @param adapter2 the second stateless adapter
     * @return the composed stateless adapter
     */
    public static <T1, T2, T3, V1, V2, V3> StatelessAdapter<T1, T3, V1, V3> composeStateless(
            StatelessAdapter<T1, T2, V1, V2> adapter1,
            StatelessAdapter<T2, T3, V2, V3> adapter2) {

        return new StatelessAdapter<>(
                adapter1.isLossy || adapter2.isLossy,
                adapter1.source,
                adapter2.target,
                Coders.composeStateless(adapter1.coder, adapter2.coder));
    }

    /**
     * Compose two stateless adapters constructed from their source types, in turn composing their coders.
     * @param <T1> the first source type
     * @param <T2> the intermediate target type
     * @param <T3> the final target type
     * @param <V1> the first value type
     * @param <V2> the intermediate value type
     * @param <V3> the final value type
     * @param constructor1 the first stateless adapter constructor
     * @param constructor2 the second stateless adapter constructor
     * @return a function that constructs the composed stateless adapter
     */
    public static <T1, T2, T3, V1, V2, V3> Function<T1, Either<String, StatelessAdapter<T1, T3, V1, V3>>> composeStateless(
            Function<T1, Either<String, StatelessAdapter<T1, T2, V1, V2>>> constructor1,
            Function<T2, Either<String, StatelessAdapter<T2, T3, V2, V3>>> constructor2) {
        return t1 -> {
            Either<String, StatelessAdapter<T1, T2, V1, V2>> r1 = constructor1.apply(t1);
            if (r1.isLeft()) return Either.left(((Either.Left<String, StatelessAdapter<T1, T2, V1, V2>>) r1).value);
            StatelessAdapter<T1, T2, V1, V2> firstMile = ((Either.Right<String, StatelessAdapter<T1, T2, V1, V2>>) r1).value;
            Either<String, StatelessAdapter<T2, T3, V2, V3>> r2 = constructor2.apply(firstMile.target);
            if (r2.isLeft()) return Either.left(((Either.Left<String, StatelessAdapter<T2, T3, V2, V3>>) r2).value);
            StatelessAdapter<T2, T3, V2, V3> lastMile = ((Either.Right<String, StatelessAdapter<T2, T3, V2, V3>>) r2).value;
            return Either.right(composeStateless(firstMile, lastMile));
        };
    }
}

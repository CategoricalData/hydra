package hydra.util;

import hydra.HydraTestBase;
import org.junit.jupiter.api.Test;

import static hydra.Coders.composeStateless;
import static hydra.Coders.inverseStateless;


/**
 * A collection of tests illustrating the use of stateless coders, for simplicity.
 */
public class StatelessCoderTest extends HydraTestBase {
    private static final StatelessCoder<Integer, Integer> addOneCoder = new StatelessCoder<>(
            i -> Either.right(i + 1),
            i -> Either.right(i - 1)
    );

    private static final StatelessCoder<Integer, Integer> mod7Coder = new StatelessCoder<>(
            i -> Either.right(i % 7),
            i -> Either.left("modulus operation is not reversible")
    );

    private static final StatelessCoder<Integer, String> toStringCoder = new StatelessCoder<>(
            i -> Either.right(i.toString()),
            s -> {
                try {
                    return Either.right(Integer.parseInt(s));
                } catch (NumberFormatException e) {
                    return Either.left(e.getMessage());
                }
            });

    @Test
    public void checkBidirectionalCoder() {
        // 1 is encoded to 2
        assertSucceedsWith(2, coderEncode(addOneCoder, 1));
        // 2 is decoded back to 1
        assertSucceedsWith(1, coderDecode(addOneCoder, 2));

        // Round-trip from 1 to 2 and back to 1 is a no-op
        assertRoundTripIsNoop(addOneCoder, 1);
    }

    @Test
    public void checkOneWayCoder() {
        // Normally, every coder has at least a working encode function
        assertSucceedsWith(2, coderEncode(mod7Coder, 9));

        // This coder has a decode function which always fails; it is not meant to be used for decoding
        assertFails(coderDecode(mod7Coder, 2));
    }

    @Test
    public void checkPartialBidirectionalCoder() {
        // For all values, a round-trip "from the left" is a no-op
        assertRoundTripIsNoop(toStringCoder, 1);

        // For certain values, a round-trip "from the right" is also a no-op
        Coder<String, Integer> inverse = inverseStateless(toStringCoder);
        assertRoundTripIsNoop(inverse, "42");

        // However, for some values, a round-trip "from the right" fails.
        // In this case, the value "not a number" cannot be decoded to an integer
        assertRoundTripFails(inverse, "not a number");
    }

    @Test
    public void checkInverses() {
        // The inverse of the "add one" coder is a "subtract one" coder;
        // it just swaps the encode and decode functions.
        Coder<Integer, Integer> subtractOneCoder = inverseStateless(addOneCoder);
        assertSucceedsWith(0, coderEncode(subtractOneCoder, 1));
        assertSucceedsWith(1, coderDecode(subtractOneCoder, 0));
    }

    @Test
    public void checkComposition() {
        // Compose two coders to create a new coder
        Coder<Integer, String> composed = composeStateless(addOneCoder, toStringCoder);
        Coder<String, Integer> inverse = inverseStateless(composed);

        // The composed encode function adds one, then converts the number to a string
        assertSucceedsWith("2", coderEncode(composed, 1));

        // The composed decode function converts the string to a number, then subtracts one
        assertSucceedsWith(41, coderEncode(inverse, "42"));

        // All round-trips "from the left" are no-ops
        assertRoundTripIsNoop(composed, 1);

        // Some round-trips "from the right" are no-ops
        assertRoundTripIsNoop(inverse, "42");

        // Other round-trips "from the right" fail, for the same reason that round-trips through toStringCoder fail
        assertRoundTripFails(inverse, "not a number");
    }

    /**
     * Apply a Coder's encode function, converting InContext errors to simple strings.
     */
    private static <V1, V2> Either<String, V2> coderEncode(Coder<V1, V2> coder, V1 value) {
        hydra.context.Context cx = new hydra.context.Context(
            hydra.util.ConsList.empty(),
            hydra.util.ConsList.empty(),
            hydra.util.PersistentMap.empty());
        Either<hydra.context.InContext<hydra.error.Error_>, V2> result =
            coder.encode.apply(cx).apply(value);
        if (result.isRight()) {
            return Either.right(((Either.Right<hydra.context.InContext<hydra.error.Error_>, V2>) result).value);
        } else {
            return Either.left(hydra.show.error.Error_.error(((Either.Left<hydra.context.InContext<hydra.error.Error_>, V2>) result).value.object));
        }
    }

    /**
     * Apply a Coder's decode function, converting InContext errors to simple strings.
     */
    private static <V1, V2> Either<String, V1> coderDecode(Coder<V1, V2> coder, V2 value) {
        hydra.context.Context cx = new hydra.context.Context(
            hydra.util.ConsList.empty(),
            hydra.util.ConsList.empty(),
            hydra.util.PersistentMap.empty());
        Either<hydra.context.InContext<hydra.error.Error_>, V1> result =
            coder.decode.apply(cx).apply(value);
        if (result.isRight()) {
            return Either.right(((Either.Right<hydra.context.InContext<hydra.error.Error_>, V1>) result).value);
        } else {
            return Either.left(hydra.show.error.Error_.error(((Either.Left<hydra.context.InContext<hydra.error.Error_>, V1>) result).value.object));
        }
    }
}

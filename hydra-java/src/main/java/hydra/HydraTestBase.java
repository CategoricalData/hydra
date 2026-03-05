package hydra;

import hydra.compute.Coder;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.graph.Graph;
import hydra.graph.Primitive;
import hydra.lib.Libraries;
import hydra.tools.PrimitiveFunction;
import hydra.util.Either;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

import static hydra.Coders.roundTrip;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;


/**
 * Base class for Hydra test utilities providing common assertion methods.
 */
public class HydraTestBase {

    /**
     * Assert that an Either is a Left (failure).
     * @param <X> the result type
     * @param result the Either to check
     */
    protected static <X> void assertFails(Either<String, X> result) {
        assertTrue(result.isLeft(), "Expected failure but got success");
    }

    /**
     * Assert that encoding and then decoding a value results in the original value.
     * @param <V1> the initial value type
     * @param <V2> the intermediate value type
     * @param coder the coder to test
     * @param initialValue the initial value
     */
    protected static <V1, V2> void assertRoundTripIsNoop(Coder<V1, V2> coder, V1 initialValue) {
        assertSucceedsWith(initialValue, roundTrip(coder, initialValue));
    }

    /**
     * Assert that encoding and then decoding a value fails.
     * @param <V1> the initial value type
     * @param <V2> the intermediate value type
     * @param coder the coder to test
     * @param initialValue the initial value
     */
    protected static <V1, V2> void assertRoundTripFails(Coder<V1, V2> coder, V1 initialValue) {
        assertFails(roundTrip(coder, initialValue));
    }

    /**
     * Assert that an Either is a Right (success).
     * @param <X> the result type
     * @param result the Either to check
     */
    protected static <X> void assertSucceeds(Either<String, X> result) {
        assertTrue(result.isRight(), "Expected success but got failure: " +
            (result.isLeft() ? ((Either.Left<String, X>) result).value : ""));
    }

    /**
     * Assert that an Either is a Right and contains the expected value.
     * @param <X> the result type
     * @param expected the expected result
     * @param result the Either to check
     */
    protected static <X> void assertSucceedsWith(X expected, Either<String, X> result) {
        assertTrue(result.isRight(), "Expected success but got failure: " +
            (result.isLeft() ? ((Either.Left<String, X>) result).value : ""));
        assertEquals(expected, ((Either.Right<String, X>) result).value);
    }

    /**
     * Check an Either result, applying a consumer to the Right value.
     * @param <X> the result type
     * @param result the Either to check
     * @param consumer the consumer to apply to the Right value
     */
    protected static <X> void checkResult(Either<String, X> result, Consumer<X> consumer) {
        assertTrue(result.isRight(), "Expected success but got failure: " +
            (result.isLeft() ? ((Either.Left<String, X>) result).value : ""));
        consumer.accept(((Either.Right<String, X>) result).value);
    }

    /**
     * Create an empty graph with standard primitives.
     * @return an empty graph
     */
    protected static Graph emptyGraph() {
        Map<Name, Term> boundTerms = Collections.emptyMap();
        Map<Name, TypeScheme> boundTypes = Collections.emptyMap();
        Map<Name, hydra.core.TypeVariableMetadata> classConstraints = Collections.emptyMap();
        java.util.Set<Name> lambdaVariables = Collections.emptySet();
        Map<Name, Term> metadata = Collections.emptyMap();

        Map<Name, Primitive> primitives = new HashMap<>();
        for (PrimitiveFunction prim : Libraries.standardPrimitives()) {
            primitives.put(prim.name(), prim.toNative());
        }

        Map<Name, TypeScheme> schemaTypes = Collections.emptyMap();
        java.util.Set<Name> typeVariables = Collections.emptySet();

        return new Graph(boundTerms, boundTypes, classConstraints, lambdaVariables, metadata, primitives, schemaTypes, typeVariables);
    }
}

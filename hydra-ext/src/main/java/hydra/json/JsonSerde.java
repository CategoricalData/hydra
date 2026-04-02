package hydra.json;

import com.cedarsoftware.util.io.JsonReader;
import com.cedarsoftware.util.io.JsonWriter;
import hydra.coders.Coder;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.errors.OtherError;
import hydra.json.model.Value;
import hydra.util.Either;

import java.util.HashMap;
import java.util.Map;

/**
 * A bidirectional coder between Hydra's native JSON values and strings (via json-io).
 */
public class JsonSerde extends Coder<Value, String> {
    private static final Map<String, Object> WRITER_ARGS = new HashMap<String, Object>() {{
        put(JsonWriter.TYPE, false);
        put(JsonWriter.PRETTY_PRINT, false);
    }};

    /**
     * Constructs a new JsonSerde.
     */
    public JsonSerde() {
        super(toCoderFn(JsonSerde::encode), toCoderFn(JsonSerde::decode));
    }

    /**
     * Convert a simple Either-based function to the Coder's Context-based Either signature.
     */
    private static <A, B> java.util.function.Function<Context, java.util.function.Function<A, Either<InContext<Error_>, B>>>
            toCoderFn(java.util.function.Function<A, Either<String, B>> fn) {
        return cx -> a -> {
            Either<String, B> result = fn.apply(a);
            if (result.isRight()) {
                return Either.right(((Either.Right<String, B>) result).value);
            } else {
                String msg = ((Either.Left<String, B>) result).value;
                return Either.left(new InContext<>(new Error_.Other(new OtherError(msg)), cx));
            }
        };
    }

    /**
     * Encode a JSON value to a string.
     *
     * @param value the JSON value to encode
     * @return an Either containing the encoded string or an error message
     */
    public static Either<String, String> encode(Value value) {
        Either<String, Object> ioResult = JsonIoCoder.encode(value);
        if (ioResult.isLeft()) {
            return Either.left(((Either.Left<String, Object>) ioResult).value);
        }
        Object raw = ((Either.Right<String, Object>) ioResult).value;
        return Either.right(JsonWriter.objectToJson(JsonIoCoder.normalizeEncoded(raw), WRITER_ARGS));
    }

    /**
     * Decode a JSON value from a string.
     *
     * @param value the string to decode
     * @return an Either containing the decoded JSON value or an error message
     */
    public static Either<String, Value> decode(String value) {
        Object json;
        try {
            json = JsonReader.jsonToJava(value);
        } catch (Exception e) {
            return Either.left("JSON parse error: " + e.getMessage());
        }

        return JsonIoCoder.decode(json);
    }
}

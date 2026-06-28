package hydra.json;

import com.cedarsoftware.util.io.JsonReader;
import com.cedarsoftware.util.io.JsonWriter;
import hydra.coders.Coder;
import hydra.json.model.Value;
import hydra.overlay.java.util.Either;

import java.util.HashMap;
import java.util.Map;

/**
 * A bidirectional coder between Hydra's native JSON values and strings (via json-io).
 */
public class JsonSerde extends Coder<Value, String, String> {
    private static final Map<String, Object> WRITER_ARGS = new HashMap<String, Object>() {{
        put(JsonWriter.TYPE, false);
        put(JsonWriter.PRETTY_PRINT, false);
    }};

    /**
     * Constructs a new JsonSerde.
     */
    public JsonSerde() {
        super(JsonSerde::encode, JsonSerde::decode);
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

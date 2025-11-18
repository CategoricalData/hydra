package hydra.json;

import com.cedarsoftware.util.io.JsonReader;
import com.cedarsoftware.util.io.JsonWriter;
import hydra.dsl.Flows;
import hydra.compute.Coder;
import hydra.compute.Flow;
import hydra.json.Value;

import java.util.HashMap;
import java.util.Map;

/**
 * A bidirectional coder between Hydra's native JSON values and strings (via json-io).
 *
 * @param <S1> the state type for encoding
 * @param <S2> the state type for decoding
 */
public class JsonSerde<S1, S2> extends Coder<S1, S2, Value, String> {
    private static final Map<String, Object> WRITER_ARGS = new HashMap<String, Object>() {{
        put(JsonWriter.TYPE, false);
        put(JsonWriter.PRETTY_PRINT, false);
        //put(JsonWriter.SKIP_NULL_FIELDS, true);
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
     * @param <S> the state type
     * @param value the JSON value to encode
     * @return a flow containing the encoded string
     */
    public static <S> Flow<S, String> encode(Value value) {
        return Flows.map(JsonIoCoder.encode(value),
                raw -> JsonWriter.objectToJson(JsonIoCoder.normalizeEncoded(raw), WRITER_ARGS));
    }

    /**
     * Decode a JSON value from a string.
     *
     * @param <S> the state type
     * @param value the string to decode
     * @return a flow containing the decoded JSON value
     */
    public static <S> Flow<S, Value> decode(String value) {
        Object json;
        try {
            json = JsonReader.jsonToJava(value);
        } catch (Exception e) {
            return Flows.fail("JSON parse error", e);
        }

        return JsonIoCoder.decode(json);
    }
}

package hydra.json;

import com.cedarsoftware.util.io.JsonObject;
import hydra.coders.Coder;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.errors.OtherError;
import hydra.json.model.Value;
import hydra.util.Either;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import hydra.util.ConsList;
import hydra.util.PersistentMap;

/**
 * A bidirectional coder between Hydra's native JSON values and the JSON objects supported by json-io.
 */
public class JsonIoCoder extends Coder<Value, Object> {
    /**
     * Constructs a new JsonIoCoder.
     */
    public JsonIoCoder() {
        super(toCoderFn(JsonIoCoder::encode), toCoderFn(JsonIoCoder::decode));
    }

    /**
     * Convert a simple Either-based function to the Coder's Context-based Either signature.
     */
    private static <A, B> java.util.function.Function<Context, java.util.function.Function<A, Either<Error_, B>>>
            toCoderFn(java.util.function.Function<A, Either<String, B>> fn) {
        return cx -> a -> {
            Either<String, B> result = fn.apply(a);
            if (result.isRight()) {
                return Either.right(((Either.Right<String, B>) result).value);
            } else {
                String msg = ((Either.Left<String, B>) result).value;
                return Either.left(new Error_.Other(new OtherError(msg)));
            }
        };
    }

    /**
     * Encode a JSON value as a json-io object.
     *
     * @param value the JSON value to encode
     * @return an Either containing the encoded json-io object or an error message
     */
    public static Either<String, Object> encode(Value value) {
        return value.accept(new Value.Visitor<Either<String, Object>>() {
            @Override
            public Either<String, Object> visit(Value.Array instance) {
                List<Object> list = new ArrayList<>();
                for (Value v : instance.value) {
                    Either<String, Object> r = encode(v);
                    if (r.isLeft()) return Either.left(((Either.Left<String, Object>) r).value);
                    list.add(((Either.Right<String, Object>) r).value);
                }
                return Either.right(list.toArray());
            }

            @Override
            public Either<String, Object> visit(Value.Boolean_ instance) {
                return Either.right(instance.value);
            }

            @Override
            public Either<String, Object> visit(Value.Null instance) {
                // Note: we return an object here rather than a null, as Either cannot accept nulls as values.
                //       This is acceptable so long as we are only using the generated json-io objects internally,
                //       for serialization. See normalizeEncoded().
                return Either.right(instance);
            }

            @Override
            public Either<String, Object> visit(Value.Number_ instance) {
                try {
                    return Either.right(instance.value.doubleValue());
                } catch (NumberFormatException e) {
                    return Either.left("Invalid number format: " + instance.value);
                }
            }

            @Override
            public Either<String, Object> visit(Value.Object_ instance) {
                JsonObject obj = new JsonObject();
                for (Map.Entry<String, Value> entry : instance.value.entrySet()) {
                    Either<String, Object> r = encode(entry.getValue());
                    if (r.isLeft()) return Either.left(((Either.Left<String, Object>) r).value);
                    obj.put(entry.getKey(), ((Either.Right<String, Object>) r).value);
                }
                return Either.right(obj);
            }

            @Override
            public Either<String, Object> visit(Value.String_ instance) {
                return Either.right(instance.value);
            }
        });
    }

    /**
     * Decode a json-io object as a JSON value.
     *
     * @param value the json-io object to decode
     * @return an Either containing the decoded JSON value or an error message
     */
    public static Either<String, Value> decode(Object value) {
        if (value == null) {
            return Either.right(new Value.Null());
        } else if (value.getClass().isArray()) {
            Object[] array = (Object[]) value;
            List<Value> list = new ArrayList<>();
            for (Object item : array) {
                Either<String, Value> r = decode(item);
                if (r.isLeft()) return Either.left(((Either.Left<String, Value>) r).value);
                list.add(((Either.Right<String, Value>) r).value);
            }
            return Either.right(new Value.Array(ConsList.fromList(list)));
        } else if (value instanceof JsonObject) {
            Map<String, Value> map = new LinkedHashMap<>();
            Collection<Map.Entry<Object, Object>> entries = ((JsonObject) value).entrySet();
            for (Map.Entry<Object, Object> entry : entries) {
                Either<String, String> keyResult = decodeKey(entry.getKey());
                if (keyResult.isLeft()) return Either.left(((Either.Left<String, String>) keyResult).value);
                Either<String, Value> valResult = decode(entry.getValue());
                if (valResult.isLeft()) return Either.left(((Either.Left<String, Value>) valResult).value);
                map.put(((Either.Right<String, String>) keyResult).value,
                        ((Either.Right<String, Value>) valResult).value);
            }
            return Either.right(new Value.Object_(PersistentMap.fromMap(map)));
        } else if (value instanceof String) {
            return Either.right(new Value.String_((String) value));
        } else if (value instanceof Boolean) {
            return Either.right(new Value.Boolean_((Boolean) value));
        } else if (value instanceof Number) {
            return Either.right(new Value.Number_(BigDecimal.valueOf(((Number) value).doubleValue())));
        } else {
            return Either.left("Expected object, array, string, boolean, or number, found " + value.getClass().getName());
        }
    }

    /**
     * Normalize an already-encoded json-io object.
     *
     * @param raw the raw json-io object to normalize
     * @return the normalized json-io object
     */
    public static Object normalizeEncoded(Object raw) {
        if (raw instanceof Value.Null) {
            return null;
        } else if (raw.getClass().isArray()) {
            Object[] rawArray = (Object[]) raw;
            Object[] normed = new Object[rawArray.length];
            for (int i = 0; i < rawArray.length; i++) {
                normed[i] = normalizeEncoded(rawArray[i]);
            }
            return normed;
        } else if (raw instanceof JsonObject) {
            JsonObject rawObj = (JsonObject) raw;
            JsonObject normed = new JsonObject();
            Collection<Map.Entry<Object, Object>> entries = rawObj.entrySet();
            for (Map.Entry<Object, Object> entry : entries) {
                normed.put(entry.getKey(), normalizeEncoded(entry.getValue()));
            }
            return normed;
        } else {
            return raw;
        }
    }

    /**
     * Decode a json-io object key as a string.
     *
     * @param key the key object to decode
     * @return an Either containing the decoded string key or an error message
     */
    private static Either<String, String> decodeKey(Object key) {
        if (key instanceof String) {
            return Either.right((String) key);
        } else {
            return Either.left("Expected string, found " + key.getClass().getName());
        }
    }
}

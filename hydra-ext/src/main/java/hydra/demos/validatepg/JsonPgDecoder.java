package hydra.demos.validatepg;

import hydra.core.FloatType;
import hydra.core.FloatValue;
import hydra.core.IntegerType;
import hydra.core.IntegerValue;
import hydra.core.Literal;
import hydra.core.LiteralType;
import hydra.json.model.Value;
import hydra.pg.model.Edge;
import hydra.pg.model.EdgeLabel;
import hydra.pg.model.EdgeType;
import hydra.pg.model.Graph;
import hydra.pg.model.GraphSchema;
import hydra.pg.model.PropertyKey;
import hydra.pg.model.PropertyType;
import hydra.pg.model.Vertex;
import hydra.pg.model.VertexLabel;
import hydra.pg.model.VertexType;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Decodes PG model objects from the JSON format produced by hydra.encode.pg.model + hydra.json.encode.
 *
 * <p>The JSON encoding follows Hydra's standard conventions:
 * <ul>
 *   <li>Records → JSON objects with field names as keys</li>
 *   <li>Unions → JSON objects with a single key (the variant name)</li>
 *   <li>Wraps (VertexLabel, EdgeLabel, PropertyKey) → the inner value directly</li>
 *   <li>Maps → JSON arrays of {"@key": k, "@value": v} objects</li>
 *   <li>Booleans → true/false</li>
 * </ul>
 */
class JsonPgDecoder {

    static GraphSchema<LiteralType> decodeGraphSchema(Value json) {
        Map<String, Value> obj = expectObject(json);
        Map<VertexLabel, VertexType<LiteralType>> vertices = decodeMap(
                requireField(obj, "vertices"),
                v -> new VertexLabel(expectString(v)),
                v -> decodeVertexType(v));
        Map<EdgeLabel, EdgeType<LiteralType>> edges = decodeMap(
                requireField(obj, "edges"),
                v -> new EdgeLabel(expectString(v)),
                v -> decodeEdgeType(v));
        return new GraphSchema<>(vertices, edges);
    }

    static Graph<Literal> decodeGraph(Value json) {
        Map<String, Value> obj = expectObject(json);
        Map<Literal, Vertex<Literal>> vertices = decodeMap(
                requireField(obj, "vertices"),
                JsonPgDecoder::decodeLiteral,
                JsonPgDecoder::decodeVertex);
        Map<Literal, Edge<Literal>> edges = decodeMap(
                requireField(obj, "edges"),
                JsonPgDecoder::decodeLiteral,
                JsonPgDecoder::decodeEdge);
        return new Graph<>(vertices, edges);
    }

    private static VertexType<LiteralType> decodeVertexType(Value json) {
        Map<String, Value> obj = expectObject(json);
        VertexLabel label = new VertexLabel(expectString(requireField(obj, "label")));
        LiteralType id = decodeLiteralType(requireField(obj, "id"));
        List<PropertyType<LiteralType>> properties = decodeList(
                requireField(obj, "properties"),
                JsonPgDecoder::decodePropertyType);
        return new VertexType<>(label, id, properties);
    }

    private static EdgeType<LiteralType> decodeEdgeType(Value json) {
        Map<String, Value> obj = expectObject(json);
        EdgeLabel label = new EdgeLabel(expectString(requireField(obj, "label")));
        LiteralType id = decodeLiteralType(requireField(obj, "id"));
        VertexLabel out = new VertexLabel(expectString(requireField(obj, "out")));
        VertexLabel in = new VertexLabel(expectString(requireField(obj, "in")));
        List<PropertyType<LiteralType>> properties = decodeList(
                requireField(obj, "properties"),
                JsonPgDecoder::decodePropertyType);
        return new EdgeType<>(label, id, out, in, properties);
    }

    private static PropertyType<LiteralType> decodePropertyType(Value json) {
        Map<String, Value> obj = expectObject(json);
        PropertyKey key = new PropertyKey(expectString(requireField(obj, "key")));
        LiteralType value = decodeLiteralType(requireField(obj, "value"));
        boolean required = expectBoolean(requireField(obj, "required"));
        return new PropertyType<>(key, value, required);
    }

    private static Vertex<Literal> decodeVertex(Value json) {
        Map<String, Value> obj = expectObject(json);
        VertexLabel label = new VertexLabel(expectString(requireField(obj, "label")));
        Literal id = decodeLiteral(requireField(obj, "id"));
        Map<PropertyKey, Literal> properties = decodeMap(
                requireField(obj, "properties"),
                v -> new PropertyKey(expectString(v)),
                JsonPgDecoder::decodeLiteral);
        return new Vertex<>(label, id, properties);
    }

    private static Edge<Literal> decodeEdge(Value json) {
        Map<String, Value> obj = expectObject(json);
        EdgeLabel label = new EdgeLabel(expectString(requireField(obj, "label")));
        Literal id = decodeLiteral(requireField(obj, "id"));
        Literal out = decodeLiteral(requireField(obj, "out"));
        Literal in = decodeLiteral(requireField(obj, "in"));
        Map<PropertyKey, Literal> properties = decodeMap(
                requireField(obj, "properties"),
                v -> new PropertyKey(expectString(v)),
                JsonPgDecoder::decodeLiteral);
        return new Edge<>(label, id, out, in, properties);
    }

    static LiteralType decodeLiteralType(Value json) {
        Map<String, Value> obj = expectObject(json);
        if (obj.get("binary") != null) return new LiteralType.Binary();
        if (obj.get("boolean") != null) return new LiteralType.Boolean_();
        if (obj.get("string") != null) return new LiteralType.String_();
        if (obj.get("float") != null) return new LiteralType.Float_(decodeFloatType(obj.get("float")));
        if (obj.get("integer") != null) return new LiteralType.Integer_(decodeIntegerType(obj.get("integer")));
        throw new RuntimeException("Unknown literal type: " + obj);
    }

    private static FloatType decodeFloatType(Value json) {
        Map<String, Value> obj = expectObject(json);
        if (obj.get("bigfloat") != null) return new FloatType.Bigfloat();
        if (obj.get("float32") != null) return new FloatType.Float32();
        if (obj.get("float64") != null) return new FloatType.Float64();
        throw new RuntimeException("Unknown float type: " + obj);
    }

    private static IntegerType decodeIntegerType(Value json) {
        Map<String, Value> obj = expectObject(json);
        if (obj.get("bigint") != null) return new IntegerType.Bigint();
        if (obj.get("int8") != null) return new IntegerType.Int8();
        if (obj.get("int16") != null) return new IntegerType.Int16();
        if (obj.get("int32") != null) return new IntegerType.Int32();
        if (obj.get("int64") != null) return new IntegerType.Int64();
        if (obj.get("uint8") != null) return new IntegerType.Uint8();
        if (obj.get("uint16") != null) return new IntegerType.Uint16();
        if (obj.get("uint32") != null) return new IntegerType.Uint32();
        if (obj.get("uint64") != null) return new IntegerType.Uint64();
        throw new RuntimeException("Unknown integer type: " + obj);
    }

    static Literal decodeLiteral(Value json) {
        Map<String, Value> obj = expectObject(json);
        if (obj.get("binary") != null) return new Literal.Binary(expectString(obj.get("binary")).getBytes(java.nio.charset.StandardCharsets.UTF_8));
        if (obj.get("boolean") != null) return new Literal.Boolean_(expectBoolean(obj.get("boolean")));
        if (obj.get("string") != null) return new Literal.String_(expectString(obj.get("string")));
        if (obj.get("float") != null) return new Literal.Float_(decodeFloatValue(obj.get("float")));
        if (obj.get("integer") != null) return new Literal.Integer_(decodeIntegerValue(obj.get("integer")));
        throw new RuntimeException("Unknown literal: " + obj);
    }

    private static FloatValue decodeFloatValue(Value json) {
        Map<String, Value> obj = expectObject(json);
        if (obj.get("bigfloat") != null) return new FloatValue.Bigfloat(expectNumber(obj.get("bigfloat")));
        if (obj.get("float32") != null) return new FloatValue.Float32(expectNumber(obj.get("float32")).floatValue());
        if (obj.get("float64") != null) return new FloatValue.Float64(expectNumber(obj.get("float64")).doubleValue());
        throw new RuntimeException("Unknown float value: " + obj);
    }

    private static IntegerValue decodeIntegerValue(Value json) {
        Map<String, Value> obj = expectObject(json);
        if (obj.get("bigint") != null) return new IntegerValue.Bigint(expectNumber(obj.get("bigint")).toBigInteger());
        if (obj.get("int8") != null) return new IntegerValue.Int8(expectNumber(obj.get("int8")).byteValue());
        if (obj.get("int16") != null) return new IntegerValue.Int16(expectNumber(obj.get("int16")).shortValue());
        if (obj.get("int32") != null) return new IntegerValue.Int32(expectNumber(obj.get("int32")).intValue());
        if (obj.get("int64") != null) return new IntegerValue.Int64(expectNumber(obj.get("int64")).longValue());
        if (obj.get("uint8") != null) return new IntegerValue.Uint8(expectNumber(obj.get("uint8")).shortValue());
        if (obj.get("uint16") != null) return new IntegerValue.Uint16((char) expectNumber(obj.get("uint16")).intValue());
        if (obj.get("uint32") != null) return new IntegerValue.Uint32(expectNumber(obj.get("uint32")).longValue());
        if (obj.get("uint64") != null) return new IntegerValue.Uint64(expectNumber(obj.get("uint64")).toBigInteger());
        throw new RuntimeException("Unknown integer value: " + obj);
    }

    // -- Helpers --

    private static <K, V> Map<K, V> decodeMap(Value json,
            java.util.function.Function<Value, K> decodeKey,
            java.util.function.Function<Value, V> decodeValue) {
        List<Value> arr = expectArray(json);
        Map<K, V> result = new LinkedHashMap<>();
        for (Value entry : arr) {
            Map<String, Value> entryObj = expectObject(entry);
            K key = decodeKey.apply(requireField(entryObj, "@key"));
            V value = decodeValue.apply(requireField(entryObj, "@value"));
            result.put(key, value);
        }
        return result;
    }

    private static <T> List<T> decodeList(Value json, java.util.function.Function<Value, T> decode) {
        List<Value> arr = expectArray(json);
        List<T> result = new ArrayList<>();
        for (Value v : arr) {
            result.add(decode.apply(v));
        }
        return result;
    }

    private static Map<String, Value> expectObject(Value json) {
        if (json instanceof Value.Object_) {
            return ((Value.Object_) json).value;
        }
        throw new RuntimeException("Expected JSON object, got " + json.getClass().getSimpleName());
    }

    private static List<Value> expectArray(Value json) {
        if (json instanceof Value.Array) {
            return ((Value.Array) json).value;
        }
        throw new RuntimeException("Expected JSON array, got " + json.getClass().getSimpleName());
    }

    private static String expectString(Value json) {
        if (json instanceof Value.String_) {
            return ((Value.String_) json).value;
        }
        throw new RuntimeException("Expected JSON string, got " + json.getClass().getSimpleName());
    }

    private static boolean expectBoolean(Value json) {
        if (json instanceof Value.Boolean_) {
            return ((Value.Boolean_) json).value;
        }
        throw new RuntimeException("Expected JSON boolean, got " + json.getClass().getSimpleName());
    }

    private static java.math.BigDecimal expectNumber(Value json) {
        if (json instanceof Value.Number_) {
            return ((Value.Number_) json).value;
        }
        throw new RuntimeException("Expected JSON number, got " + json.getClass().getSimpleName());
    }

    private static Value requireField(Map<String, Value> obj, String name) {
        Value value = obj.get(name);
        if (value == null) {
            throw new RuntimeException("Missing required field: " + name);
        }
        return value;
    }
}

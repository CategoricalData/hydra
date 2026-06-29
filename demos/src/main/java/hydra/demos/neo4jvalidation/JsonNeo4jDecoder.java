package hydra.demos.neo4jvalidation;

import hydra.json.model.Value;
import hydra.neo4j.model.Constraint;
import hydra.neo4j.model.ConstraintDefinition;
import hydra.neo4j.model.ElementId;
import hydra.neo4j.model.GraphType;
import hydra.neo4j.model.Key;
import hydra.neo4j.model.Node;
import hydra.neo4j.model.NodeElementType;
import hydra.neo4j.model.NodeLabel;
import hydra.neo4j.model.PropertyExistenceConstraint;
import hydra.neo4j.model.PropertyTypeConstraint;
import hydra.neo4j.model.Relationship;
import hydra.neo4j.model.RelationshipElementType;
import hydra.neo4j.model.RelationshipType;
import hydra.neo4j.model.ValueType;
import hydra.overlay.java.util.Optional;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Decodes Neo4j-model objects from the canonical Hydra term-JSON produced by
 * {@code hydra.encode.neo4j.Model} + {@code hydra.json}.
 *
 * <p>The JSON encoding follows Hydra's standard conventions:
 * <ul>
 *   <li>Records → JSON objects with field names as keys</li>
 *   <li>Unions → JSON objects with a single key (the variant name)</li>
 *   <li>Wraps (ElementId, Key, NodeLabel, RelationshipType) → the inner value directly</li>
 *   <li>Maps (a node/relationship's properties) → JSON arrays of {"key": k, "value": v} objects</li>
 *   <li>Sets (a node's labels) → JSON arrays</li>
 *   <li>64-bit integers → JSON strings</li>
 * </ul>
 *
 * <p>This hand-written decoder covers exactly the slice of the model the demo fixtures use; it is
 * kept simple and explicit so the JSON-to-model mapping is easy to read.
 */
class JsonNeo4jDecoder {

    // -- GraphType (schema) --

    static GraphType decodeGraphType(Value json) {
        Map<String, Value> obj = expectObject(json);
        List<NodeElementType> nodes = decodeList(requireField(obj, "nodes"),
            JsonNeo4jDecoder::decodeNodeElementType);
        List<RelationshipElementType> rels = decodeList(requireField(obj, "relationships"),
            JsonNeo4jDecoder::decodeRelationshipElementType);
        return new GraphType(nodes, rels);
    }

    private static NodeElementType decodeNodeElementType(Value json) {
        Map<String, Value> obj = expectObject(json);
        NodeLabel label = new NodeLabel(expectString(requireField(obj, "identifyingLabel")));
        Set<NodeLabel> implied = decodeSet(requireField(obj, "impliedLabels"),
            v -> new NodeLabel(expectString(v)));
        List<ConstraintDefinition> constraints = decodeList(requireField(obj, "constraints"),
            JsonNeo4jDecoder::decodeConstraintDefinition);
        return new NodeElementType(label, implied, constraints);
    }

    private static RelationshipElementType decodeRelationshipElementType(Value json) {
        Map<String, Value> obj = expectObject(json);
        RelationshipType type = new RelationshipType(expectString(requireField(obj, "type")));
        NodeLabel start = new NodeLabel(expectString(requireField(obj, "startLabel")));
        NodeLabel end = new NodeLabel(expectString(requireField(obj, "endLabel")));
        List<ConstraintDefinition> constraints = decodeList(requireField(obj, "constraints"),
            JsonNeo4jDecoder::decodeConstraintDefinition);
        return new RelationshipElementType(type, start, end, constraints);
    }

    private static ConstraintDefinition decodeConstraintDefinition(Value json) {
        Map<String, Value> obj = expectObject(json);
        // The fixtures leave constraint names unset (encoded as null); model them as none.
        Constraint body = decodeConstraint(requireField(obj, "body"));
        return new ConstraintDefinition(Optional.none(), body);
    }

    private static Constraint decodeConstraint(Value json) {
        Map<String, Value> obj = expectObject(json);
        if (obj.get("propertyExistence") != null) {
            Map<String, Value> c = expectObject(obj.get("propertyExistence"));
            return new Constraint.PropertyExistence(new PropertyExistenceConstraint(
                new Key(expectString(requireField(c, "property")))));
        }
        if (obj.get("propertyType") != null) {
            Map<String, Value> c = expectObject(obj.get("propertyType"));
            return new Constraint.PropertyType(new PropertyTypeConstraint(
                new Key(expectString(requireField(c, "property"))),
                decodeValueType(requireField(c, "type"))));
        }
        throw new RuntimeException("Unsupported constraint in demo fixtures: " + obj.keySet());
    }

    private static ValueType decodeValueType(Value json) {
        Map<String, Value> obj = expectObject(json);
        if (obj.get("boolean") != null) return new ValueType.Boolean_();
        if (obj.get("string") != null) return new ValueType.String_();
        if (obj.get("integer") != null) return new ValueType.Integer_();
        if (obj.get("float") != null) return new ValueType.Float_();
        throw new RuntimeException("Unsupported value type in demo fixtures: " + obj.keySet());
    }

    // -- Graph (nodes + relationships) --

    static List<Node> decodeNodes(Value json) {
        Map<String, Value> obj = expectObject(json);
        return decodeList(requireField(obj, "nodes"), JsonNeo4jDecoder::decodeNode);
    }

    static List<Relationship> decodeRelationships(Value json) {
        Map<String, Value> obj = expectObject(json);
        return decodeList(requireField(obj, "relationships"), JsonNeo4jDecoder::decodeRelationship);
    }

    private static Node decodeNode(Value json) {
        Map<String, Value> obj = expectObject(json);
        ElementId id = new ElementId(expectString(requireField(obj, "id")));
        Set<NodeLabel> labels = decodeSet(requireField(obj, "labels"),
            v -> new NodeLabel(expectString(v)));
        Map<Key, hydra.neo4j.model.Value> props = decodeProperties(requireField(obj, "properties"));
        return new Node(id, labels, props);
    }

    private static Relationship decodeRelationship(Value json) {
        Map<String, Value> obj = expectObject(json);
        ElementId id = new ElementId(expectString(requireField(obj, "id")));
        Map<Key, hydra.neo4j.model.Value> props = decodeProperties(requireField(obj, "properties"));
        RelationshipType type = new RelationshipType(expectString(requireField(obj, "type")));
        ElementId start = new ElementId(expectString(requireField(obj, "start")));
        ElementId end = new ElementId(expectString(requireField(obj, "end")));
        return new Relationship(id, props, type, start, end);
    }

    private static Map<Key, hydra.neo4j.model.Value> decodeProperties(Value json) {
        List<Value> arr = expectArray(json);
        Map<Key, hydra.neo4j.model.Value> result = new LinkedHashMap<>();
        for (Value entry : arr) {
            Map<String, Value> e = expectObject(entry);
            Key k = new Key(expectString(requireField(e, "key")));
            result.put(k, decodeValueData(requireField(e, "value")));
        }
        return result;
    }

    private static hydra.neo4j.model.Value decodeValueData(Value json) {
        Map<String, Value> obj = expectObject(json);
        if (obj.get("boolean") != null) {
            return new hydra.neo4j.model.Value.Boolean_(expectBoolean(obj.get("boolean")));
        }
        if (obj.get("string") != null) {
            return new hydra.neo4j.model.Value.String_(expectString(obj.get("string")));
        }
        if (obj.get("integer") != null) {
            return new hydra.neo4j.model.Value.Integer_(expectLong(obj.get("integer")));
        }
        if (obj.get("float") != null) {
            return new hydra.neo4j.model.Value.Float_(expectNumber(obj.get("float")).doubleValue());
        }
        if (obj.get("list") != null) {
            List<hydra.neo4j.model.Value> vs = new ArrayList<>();
            for (Value v : expectArray(obj.get("list"))) {
                vs.add(decodeValueData(v));
            }
            return new hydra.neo4j.model.Value.List(vs);
        }
        throw new RuntimeException("Unsupported value in demo fixtures: " + obj.keySet());
    }

    // -- Helpers --

    private static <T> List<T> decodeList(Value json, java.util.function.Function<Value, T> decode) {
        List<T> result = new ArrayList<>();
        for (Value v : expectArray(json)) {
            result.add(decode.apply(v));
        }
        return result;
    }

    private static <T> Set<T> decodeSet(Value json, java.util.function.Function<Value, T> decode) {
        Set<T> result = new LinkedHashSet<>();
        for (Value v : expectArray(json)) {
            result.add(decode.apply(v));
        }
        return result;
    }

    private static Map<String, Value> expectObject(Value json) {
        if (json instanceof Value.Object_) {
            Map<String, Value> result = new LinkedHashMap<>();
            for (hydra.overlay.java.util.Pair<String, Value> pair : ((Value.Object_) json).value) {
                result.put(pair.first, pair.second);
            }
            return result;
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

    // 64-bit integers are encoded as JSON strings; accept either string or number.
    private static long expectLong(Value json) {
        if (json instanceof Value.String_) {
            return Long.parseLong(((Value.String_) json).value);
        }
        if (json instanceof Value.Number_) {
            return ((Value.Number_) json).value.longValue();
        }
        throw new RuntimeException("Expected JSON integer, got " + json.getClass().getSimpleName());
    }

    private static Value requireField(Map<String, Value> obj, String name) {
        Value value = obj.get(name);
        if (value == null) {
            throw new RuntimeException("Missing required field: " + name);
        }
        return value;
    }
}

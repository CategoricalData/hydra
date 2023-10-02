package hydra.langs.tinkerpop;

import hydra.Flows;
import hydra.compute.Flow;
import hydra.compute.StatelessAdapter;
import hydra.compute.StatelessCoder;
import hydra.langs.tinkerpop.propertyGraph.Edge;
import hydra.langs.tinkerpop.propertyGraph.EdgeLabel;
import hydra.langs.tinkerpop.propertyGraph.EdgeType;
import hydra.langs.tinkerpop.propertyGraph.PropertyKey;
import hydra.langs.tinkerpop.propertyGraph.PropertyType;
import hydra.langs.tinkerpop.propertyGraph.Vertex;
import hydra.langs.tinkerpop.propertyGraph.VertexLabel;
import hydra.langs.tinkerpop.propertyGraph.VertexType;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;


/**
 * Utilities for combining multiple vertex or edge types into a single "merged" vertex or edge type,
 * and correspondingly encoding and decoding vertices and edges of these types.
 */
public class Merging {
    public static VertexLabel DEFAULT_VERTEX_LABEL = new VertexLabel("_Merged");
    public static EdgeLabel DEFAULT_EDGE_LABEL = new EdgeLabel("_merged");

    public static <T, V> Flow<Void, StatelessAdapter<List<VertexType<T>>, VertexType<T>, Vertex<V>, Vertex<V>>>
    createVertexAdapter(List<VertexType<T>> types) {
        return Flows.map(Flows.check(types,
                Merging::checkNontrivial,
                Merging::checkNoDuplicatedVertexLabels,
                Merging::checkSameVertexIdType), safeTypes -> new StatelessAdapter<List<VertexType<T>>, VertexType<T>, Vertex<V>, Vertex<V>>(
                false, types, mergeVertexTypes(safeTypes), constructMergedVertexCoder(safeTypes)));
    }

    public static <T, V> Flow<Void, StatelessAdapter<List<EdgeType<T>>, EdgeType<T>, Edge<V>, Edge<V>>>
    createEdgeAdapter(List<EdgeType<T>> types) {
        return Flows.map(Flows.check(types,
                Merging::checkNontrivial,
                Merging::checkNoDuplicatedEdgeLabels,
                Merging::checkSameEdgeIdType), safeTypes -> new StatelessAdapter<List<EdgeType<T>>, EdgeType<T>, Edge<V>, Edge<V>>(
                false, types, mergeEdgeTypes(safeTypes), constructMergedEdgeCoder(safeTypes)));
    }

    private static <A> Optional<String> checkNontrivial(List<A> types) {
        return types.isEmpty()
                ? Optional.of("No types provided")
                : Optional.empty();
    }

    private static <T> Optional<String> checkNoDuplicatedVertexLabels(List<VertexType<T>> types) {
        Set<VertexLabel> labels = new HashSet<>();
        for (VertexType<T> type : types) {
            if (!labels.add(type.label)) {
                return Optional.of("Duplicate vertex label: " + type.label);
            }
        }
        return Optional.empty();
    }

    private static <T> Optional<String> checkNoDuplicatedEdgeLabels(List<EdgeType<T>> types) {
        Set<EdgeLabel> labels = new HashSet<>();
        for (EdgeType<T> type : types) {
            if (!labels.add(type.label)) {
                return Optional.of("Duplicate edge label: " + type.label);
            }
        }
        return Optional.empty();
    }

    private static <T> Optional<String> checkSameVertexIdType(List<VertexType<T>> types) {
        Set<T> idTypes = new HashSet<>();
        for (VertexType<T> type : types) {
            idTypes.add(type.id);
        }
        return idTypes.size() > 1
                ? Optional.of("" + idTypes.size() + " distinct id types; expected exactly one id type")
                : Optional.empty();
    }

    private static <T> Optional<String> checkSameEdgeIdType(List<EdgeType<T>> types) {
        Set<T> idTypes = new HashSet<>();
        for (EdgeType<T> type : types) {
            idTypes.add(type.id);
        }
        return idTypes.size() > 1
                ? Optional.of("" + idTypes.size() + " distinct id types; expected exactly one id type")
                : Optional.empty();
    }

    private static <T, V> StatelessCoder<Vertex<V>, Vertex<V>> constructMergedVertexCoder(List<VertexType<T>> types) {
        Map<VertexLabel, StatelessCoder<Vertex<V>, Vertex<V>>> coders = constructVertexCoders(types);
        return new StatelessCoder<Vertex<V>, Vertex<V>>(
                v -> Flows.bind(getCoder(coders, v.label), coder -> coder.encode.apply(v)),
                v -> Flows.bind(getCoder(coders, v.label), coder -> coder.decode.apply(v)));
    }

    private static <T, V> StatelessCoder<Edge<V>, Edge<V>> constructMergedEdgeCoder(List<EdgeType<T>> types) {
        Map<EdgeLabel, StatelessCoder<Edge<V>, Edge<V>>> coders = constructEdgeCoders(types);
        return new StatelessCoder<Edge<V>, Edge<V>>(
                e -> Flows.bind(getCoder(coders, e.label), coder -> coder.encode.apply(e)),
                e -> Flows.bind(getCoder(coders, e.label), coder -> coder.decode.apply(e)));
    }

    private static <T, V> StatelessCoder<Vertex<V>, Vertex<V>> constructVertexCoder(VertexType<T> type) {
        StatelessCoder<Map<PropertyKey, V>, Map<PropertyKey, V>> propertiesCoder
                = constructPropertiesCoder(type.label.value);

        return new StatelessCoder<Vertex<V>, Vertex<V>>(
                v -> Flows.map(propertiesCoder.encode.apply(v.properties),
                        props -> new Vertex<V>(v.label, v.id, props)),
                v -> Flows.map(propertiesCoder.decode.apply(v.properties),
                        props -> new Vertex<V>(v.label, v.id, props)));
    }

    private static <T, V> StatelessCoder<Edge<V>, Edge<V>> constructEdgeCoder(EdgeType<T> type) {
        StatelessCoder<Map<PropertyKey, V>, Map<PropertyKey, V>> propertiesCoder
                = constructPropertiesCoder(type.label.value);

        return new StatelessCoder<Edge<V>, Edge<V>>(
                e -> Flows.map(propertiesCoder.encode.apply(e.properties),
                        props -> new Edge<V>(e.label, e.id, e.out, e.in, props)),
                e -> Flows.map(propertiesCoder.decode.apply(e.properties),
                        props -> new Edge<V>(e.label, e.id, e.out, e.in, props)));
    }

    private static <V> StatelessCoder<Map<PropertyKey, V>, Map<PropertyKey, V>> constructPropertiesCoder(
            String label) {
        Function<Map<PropertyKey, V>, Flow<Void, Map<PropertyKey, V>>> encode = before -> {
            Map<PropertyKey, V> after = new HashMap<PropertyKey, V>();
            for (Map.Entry<PropertyKey, V> entry : before.entrySet()) {
                after.put(encodePropertyKey(label, entry.getKey()), entry.getValue());
            }
            return Flows.pure(after);
        };

        Function<Map<PropertyKey, V>, Flow<Void, Map<PropertyKey, V>>> decode = before -> {
            Map<PropertyKey, V> after = new HashMap<PropertyKey, V>();
            for (Map.Entry<PropertyKey, V> entry : before.entrySet()) {
                after.put(decodePropertyKey(label, entry.getKey()), entry.getValue());
            }
            return Flows.pure(after);
        };

        return new StatelessCoder<Map<PropertyKey, V>, Map<PropertyKey, V>>(encode, decode);
    }

    private static <EV, ET, L> Map<L, StatelessCoder<EV, EV>> constructCoders(
            List<ET> types,
            Function<ET, L> getLabel,
            Function<ET, StatelessCoder<EV, EV>> constructCoder) {
        Map<L, StatelessCoder<EV, EV>> coders = new HashMap<L, StatelessCoder<EV, EV>>();
        for (ET type : types) {
            coders.put(getLabel.apply(type), constructCoder.apply(type));
        }
        return coders;
    }

    private static <T, V> Map<VertexLabel, StatelessCoder<Vertex<V>, Vertex<V>>> constructVertexCoders(
            List<VertexType<T>> types) {
        return constructCoders(types, type -> type.label, Merging::constructVertexCoder);
    }

    private static <T, V> Map<EdgeLabel, StatelessCoder<Edge<V>, Edge<V>>> constructEdgeCoders(
            List<EdgeType<T>> types) {
        return constructCoders(types, type -> type.label, Merging::constructEdgeCoder);
    }

    private static PropertyKey encodePropertyKey(String label, PropertyKey key) {
        String prefix = label.toLowerCase() + "_";
        return new PropertyKey(prefix + key.value);
    }

    private static PropertyKey decodePropertyKey(String label, PropertyKey key) {
        return new PropertyKey(key.value.substring(label.length() + 1));
    }

    private static <L, E> Flow<Void, StatelessCoder<E, E>> getCoder(Map<L, StatelessCoder<E, E>> coders, L label) {
        StatelessCoder<E, E> helper = coders.get(label);
        return helper == null
                ? Flows.fail("No coder associated with label " + label)
                : Flows.pure(helper);
    }

    private static <T> VertexType<T> mergeVertexTypes(List<VertexType<T>> types) {
        return new VertexType<T>(DEFAULT_VERTEX_LABEL, types.get(0).id,
                mergePropertyTypes(types, t -> t.label.value, t -> t.properties));
    }

    private static <T> EdgeType<T> mergeEdgeTypes(List<EdgeType<T>> types) {
        return new EdgeType<T>(DEFAULT_EDGE_LABEL, types.get(0).id, DEFAULT_VERTEX_LABEL, DEFAULT_VERTEX_LABEL,
                mergePropertyTypes(types, t -> t.label.value, t -> t.properties));
    }

    private static <T, E> List<PropertyType<T>> mergePropertyTypes(
            List<E> types,
            Function<E, String> getLabel,
            Function<E, List<PropertyType<T>>> getProperties) {
        List<PropertyType<T>> ptypes = new ArrayList<PropertyType<T>>();
        for (E e : types) {
            for (PropertyType<T> ptype : getProperties.apply(e)) {
                ptypes.add(new PropertyType<T>(encodePropertyKey(getLabel.apply(e), ptype.key), ptype.value, false));
            }
        }
        return ptypes;
    }
}

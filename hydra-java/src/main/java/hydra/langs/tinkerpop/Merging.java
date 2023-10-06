package hydra.langs.tinkerpop;

import hydra.Flows;
import hydra.basics.Basics;
import hydra.compute.Flow;
import hydra.compute.StatelessAdapter;
import hydra.compute.StatelessCoder;
import hydra.core.Unit;
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
import java.util.function.BiFunction;
import java.util.function.Function;


/**
 * Utilities for combining multiple vertex or edge types into a single "merged" vertex or edge type,
 * and correspondingly encoding and decoding vertices and edges of these types.
 */
public class Merging {
    public static VertexLabel DEFAULT_VERTEX_LABEL = new VertexLabel("_Merged");
    public static EdgeLabel DEFAULT_EDGE_LABEL = new EdgeLabel("_merged");

    public static <T, V> Flow<Unit, StatelessAdapter<List<VertexType<T>>, VertexType<T>, Vertex<V>, Vertex<V>>>
    createVertexAdapter(List<VertexType<T>> types, IdCoders<T, V> idCoders) {
        return Flows.map(Flows.check(types,
                Merging::checkNontrivial,
                Merging::checkNoDuplicatedVertexLabels), safeTypes -> new StatelessAdapter<List<VertexType<T>>, VertexType<T>, Vertex<V>, Vertex<V>>(
                false, types, mergeVertexTypes(safeTypes, idCoders),
                constructMergedVertexCoder(safeTypes, idCoders)));
    }

    public static <T, V> Flow<Unit, StatelessAdapter<List<EdgeType<T>>, EdgeType<T>, Edge<V>, Edge<V>>>
    createEdgeAdapter(List<EdgeType<T>> types, IdCoders<T, V> idCoders) {
        return Flows.map(Flows.check(types,
                Merging::checkNontrivial,
                Merging::checkNoDuplicatedEdgeLabels), safeTypes -> new StatelessAdapter<List<EdgeType<T>>, EdgeType<T>, Edge<V>, Edge<V>>(
                false, types, mergeEdgeTypes(safeTypes, idCoders),
                constructMergedEdgeCoder(safeTypes, idCoders)));
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

    private static <T, V> StatelessCoder<Vertex<V>, Vertex<V>> constructMergedVertexCoder(
            List<VertexType<T>> types,
            IdCoders<T, V> idCoders) {
        Map<VertexLabel, StatelessCoder<Vertex<V>, Vertex<V>>> coders = constructVertexCoders(types, idCoders);
        return new StatelessCoder<Vertex<V>, Vertex<V>>(
                v -> Flows.bind(getCoder(coders, v.label), coder -> coder.encode.apply(v)),
                v -> Flows.bind(getCoder(coders, v.label), coder -> coder.decode.apply(v)));
    }

    private static <T, V> StatelessCoder<Edge<V>, Edge<V>> constructMergedEdgeCoder(
            List<EdgeType<T>> types,
            IdCoders<T, V> idCoders) {
        Map<EdgeLabel, StatelessCoder<Edge<V>, Edge<V>>> coders = constructEdgeCoders(types, idCoders);
        return new StatelessCoder<Edge<V>, Edge<V>>(
                e -> Flows.bind(getCoder(coders, e.label), coder -> coder.encode.apply(e)),
                e -> Flows.bind(getCoder(coders, e.label), coder -> coder.decode.apply(e)));
    }

    private static <T, V> StatelessCoder<Vertex<V>, Vertex<V>> constructVertexCoder(
            VertexType<T> type,
            IdCoders<T, V> idCoders) {
        StatelessCoder<Map<PropertyKey, V>, Map<PropertyKey, V>> propertiesCoder
                = constructPropertiesCoder(type.label.value);

        return new StatelessCoder<Vertex<V>, Vertex<V>>(
                v -> Flows.map(propertiesCoder.encode.apply(v.properties),
                        props -> new Vertex<V>(v.label, idCoders.encodeVertexId.apply(type.label, v.id), props)),
                v -> Flows.map(propertiesCoder.decode.apply(v.properties),
                        props -> new Vertex<V>(v.label, idCoders.decodeVertexId.apply(type.label, v.id), props)));
    }

    private static <T, V> StatelessCoder<Edge<V>, Edge<V>> constructEdgeCoder(
            EdgeType<T> type,
            IdCoders<T, V> idCoders) {
        StatelessCoder<Map<PropertyKey, V>, Map<PropertyKey, V>> propertiesCoder
                = constructPropertiesCoder(type.label.value);

        return new StatelessCoder<Edge<V>, Edge<V>>(
                e -> Flows.map(propertiesCoder.encode.apply(e.properties),
                        props -> new Edge<V>(
                                e.label,
                                idCoders.encodeEdgeId.apply(type.label, e.id),
                                idCoders.encodeVertexId.apply(type.out, e.out),
                                idCoders.encodeVertexId.apply(type.in, e.in),
                                props)),
                e -> Flows.map(propertiesCoder.decode.apply(e.properties),
                        props -> new Edge<V>(
                                e.label,
                                idCoders.decodeEdgeId.apply(type.label, e.id),
                                idCoders.decodeVertexId.apply(type.out, e.out),
                                idCoders.decodeVertexId.apply(type.in, e.in),
                                props)));
    }

    private static <V> StatelessCoder<Map<PropertyKey, V>, Map<PropertyKey, V>> constructPropertiesCoder(
            String label) {
        Function<Map<PropertyKey, V>, Flow<Unit, Map<PropertyKey, V>>> encode = before -> {
            Map<PropertyKey, V> after = new HashMap<PropertyKey, V>();
            for (Map.Entry<PropertyKey, V> entry : before.entrySet()) {
                after.put(encodePropertyKey(label, entry.getKey()), entry.getValue());
            }
            return Flows.pure(after);
        };

        Function<Map<PropertyKey, V>, Flow<Unit, Map<PropertyKey, V>>> decode = before -> {
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
            List<VertexType<T>> types,
            IdCoders<T, V> idCoders) {
        return constructCoders(types, type -> type.label, type -> constructVertexCoder(type, idCoders));
    }

    private static <T, V> Map<EdgeLabel, StatelessCoder<Edge<V>, Edge<V>>> constructEdgeCoders(
            List<EdgeType<T>> types,
            IdCoders<T, V> idCoders) {
        return constructCoders(types, type -> type.label, type -> constructEdgeCoder(type, idCoders));
    }

    private static PropertyKey encodePropertyKey(String label, PropertyKey key) {
        String prefix = Basics.decapitalize(label) + "_";
        return new PropertyKey(prefix + key.value);
    }

    private static PropertyKey decodePropertyKey(String label, PropertyKey key) {
        return new PropertyKey(key.value.substring(label.length() + 1));
    }

    private static <L, E> Flow<Unit, StatelessCoder<E, E>> getCoder(Map<L, StatelessCoder<E, E>> coders, L label) {
        StatelessCoder<E, E> helper = coders.get(label);
        return helper == null
                ? Flows.fail("No coder associated with label " + label)
                : Flows.pure(helper);
    }

    private static <T, V> VertexType<T> mergeVertexTypes(List<VertexType<T>> types, IdCoders<T, V> idCoders) {
        return new VertexType<T>(DEFAULT_VERTEX_LABEL, idCoders.commonVertexIdType,
                mergePropertyTypes(types, t -> t.label.value, t -> t.properties));
    }

    private static <T, V> EdgeType<T> mergeEdgeTypes(List<EdgeType<T>> types, IdCoders<T, V> idCoders) {
        return new EdgeType<T>(DEFAULT_EDGE_LABEL, idCoders.commonEdgeIdType, DEFAULT_VERTEX_LABEL, DEFAULT_VERTEX_LABEL,
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

    public static class IdCoders<T, V> {
        public final T commonVertexIdType;
        public final T commonEdgeIdType;
        public final BiFunction<VertexLabel, V, V> encodeVertexId;
        public final BiFunction<VertexLabel, V, V> decodeVertexId;
        public final BiFunction<EdgeLabel, V, V> encodeEdgeId;
        public final BiFunction<EdgeLabel, V, V> decodeEdgeId;

        public IdCoders(
                T commonVertexIdType,
                T commonEdgeIdType,
                BiFunction<VertexLabel, V, V> encodeVertexId,
                BiFunction<VertexLabel, V, V> decodeVertexId,
                BiFunction<EdgeLabel, V, V> encodeEdgeId,
                BiFunction<EdgeLabel, V, V> decodeEdgeId) {
            this.commonVertexIdType = commonVertexIdType;
            this.commonEdgeIdType = commonEdgeIdType;
            this.encodeVertexId = encodeVertexId;
            this.decodeVertexId = decodeVertexId;
            this.encodeEdgeId = encodeEdgeId;
            this.decodeEdgeId = decodeEdgeId;
        }
    }
}

package hydra.langs.tinkerpop;

import hydra.Flows;
import hydra.basics.Basics;
import hydra.compute.Flow;
import hydra.compute.StatelessAdapter;
import hydra.compute.StatelessCoder;
import hydra.core.Literal;
import hydra.core.LiteralType;
import hydra.core.Unit;
import hydra.dsl.LiteralTypes;
import hydra.dsl.Literals;
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

import static hydra.Flows.pure;


/**
 * Utilities for combining multiple vertex or edge types into a single "merged" vertex or edge type,
 * and correspondingly encoding and decoding vertices and edges of these types.
 */
public class Merging {
    public static VertexLabel DEFAULT_VERTEX_LABEL = new VertexLabel("_Merged");
    public static EdgeLabel DEFAULT_EDGE_LABEL = new EdgeLabel("_merged");

    public static <T, V> Flow<Unit, StatelessAdapter<List<VertexType<T>>, VertexType<T>, Vertex<V>, Vertex<V>>>
    createVertexAdapter(List<VertexType<T>> types, IdAdapters<T, V> idAdapters) {
        return Flows.map(Flows.check(types,
                Merging::checkNontrivial,
                Merging::checkNoDuplicatedVertexLabels), safeTypes -> new StatelessAdapter<List<VertexType<T>>, VertexType<T>, Vertex<V>, Vertex<V>>(
                false, types, mergeVertexTypes(safeTypes, idAdapters),
                constructMergedVertexCoder(safeTypes, idAdapters)));
    }

    public static <T, V> Flow<Unit, StatelessAdapter<List<EdgeType<T>>, EdgeType<T>, Edge<V>, Edge<V>>>
    createEdgeAdapter(List<EdgeType<T>> types, IdAdapters<T, V> idAdapters) {
        return Flows.map(Flows.check(types,
                Merging::checkNontrivial,
                Merging::checkNoDuplicatedEdgeLabels), safeTypes -> new StatelessAdapter<List<EdgeType<T>>, EdgeType<T>, Edge<V>, Edge<V>>(
                false, types, mergeEdgeTypes(safeTypes, idAdapters),
                constructMergedEdgeCoder(safeTypes, idAdapters)));
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
            IdAdapters<T, V> idAdapters) {
        Map<VertexLabel, StatelessCoder<Vertex<V>, Vertex<V>>> coders = constructVertexCoders(types, idAdapters);
        return new StatelessCoder<Vertex<V>, Vertex<V>>(
                v -> Flows.bind(getCoder(coders, v.label), coder -> coder.encode.apply(v)),
                v -> Flows.bind(getCoder(coders, v.label), coder -> coder.decode.apply(v)));
    }

    private static <T, V> StatelessCoder<Edge<V>, Edge<V>> constructMergedEdgeCoder(
            List<EdgeType<T>> types,
            IdAdapters<T, V> idAdapters) {
        Map<EdgeLabel, StatelessCoder<Edge<V>, Edge<V>>> coders = constructEdgeCoders(types, idAdapters);
        return new StatelessCoder<Edge<V>, Edge<V>>(
                e -> Flows.bind(getCoder(coders, e.label), coder -> coder.encode.apply(e)),
                e -> Flows.bind(getCoder(coders, e.label), coder -> coder.decode.apply(e)));
    }

    private static <T, V> StatelessCoder<Vertex<V>, Vertex<V>> constructVertexCoder(
            VertexType<T> type,
            IdAdapters<T, V> idAdapters) {
        StatelessCoder<Map<PropertyKey, V>, Map<PropertyKey, V>> propertiesCoder
                = constructPropertiesCoder(type.label.value);

        return new StatelessCoder<Vertex<V>, Vertex<V>>(
                v -> Flows.map2(
                        idAdapters.forVertexId.apply(type.label).encode.apply(v.id),
                        propertiesCoder.encode.apply(v.properties),
                        (id, props) -> new Vertex<V>(v.label, id, props)),
                v -> Flows.map2(
                        idAdapters.forVertexId.apply(type.label).decode.apply(v.id),
                        propertiesCoder.decode.apply(v.properties),
                        (id, props) -> new Vertex<V>(v.label, id, props)));
    }

    private static <T, V> StatelessCoder<Edge<V>, Edge<V>> constructEdgeCoder(
            EdgeType<T> type,
            IdAdapters<T, V> idAdapters) {
        StatelessCoder<Map<PropertyKey, V>, Map<PropertyKey, V>> propertiesCoder
                = constructPropertiesCoder(type.label.value);

        return new StatelessCoder<Edge<V>, Edge<V>>(
                e -> Flows.map4(
                        idAdapters.forEdgeId.apply(type.label).encode.apply(e.id),
                        idAdapters.forVertexId.apply(type.out).encode.apply(e.out),
                        idAdapters.forVertexId.apply(type.in).encode.apply(e.in),
                        propertiesCoder.encode.apply(e.properties),
                        (id, outId, inId, props) -> new Edge<V>(e.label, id, outId, inId, props)),
                e -> Flows.map4(
                        idAdapters.forEdgeId.apply(type.label).decode.apply(e.id),
                        idAdapters.forVertexId.apply(type.out).decode.apply(e.out),
                        idAdapters.forVertexId.apply(type.in).decode.apply(e.in),
                        propertiesCoder.decode.apply(e.properties),
                        (id, outId, inId, props) -> new Edge<V>(e.label, id, outId, inId, props)));
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
            IdAdapters<T, V> idAdapters) {
        return constructCoders(types, type -> type.label, type -> constructVertexCoder(type, idAdapters));
    }

    private static <T, V> Map<EdgeLabel, StatelessCoder<Edge<V>, Edge<V>>> constructEdgeCoders(
            List<EdgeType<T>> types,
            IdAdapters<T, V> idAdapters) {
        return constructCoders(types, type -> type.label, type -> constructEdgeCoder(type, idAdapters));
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

    private static <T, V> VertexType<T> mergeVertexTypes(List<VertexType<T>> types, IdAdapters<T, V> idAdapters) {
        return new VertexType<T>(DEFAULT_VERTEX_LABEL, idAdapters.mergedVertexIdType,
                mergePropertyTypes(types, t -> t.label.value, t -> t.properties));
    }

    private static <T, V> EdgeType<T> mergeEdgeTypes(List<EdgeType<T>> types, IdAdapters<T, V> idAdapters) {
        return new EdgeType<T>(DEFAULT_EDGE_LABEL, idAdapters.mergedEdgeIdType, DEFAULT_VERTEX_LABEL, DEFAULT_VERTEX_LABEL,
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

    public static class IdAdapters<T, V> {
        public final T mergedVertexIdType;
        public final T mergedEdgeIdType;
        public final Function<VertexLabel, StatelessCoder<V, V>> forVertexId;
        public final Function<EdgeLabel, StatelessCoder<V, V>> forEdgeId;

        public IdAdapters(
                T mergedVertexIdType,
                T mergedEdgeIdType,
                Function<VertexLabel, StatelessCoder<V, V>> forVertexId,
                Function<EdgeLabel, StatelessCoder<V, V>> forEdgeId) {
            this.mergedVertexIdType = mergedVertexIdType;
            this.mergedEdgeIdType = mergedEdgeIdType;
            this.forVertexId = forVertexId;
            this.forEdgeId = forEdgeId;
        }
    }

    public static final Merging.IdAdapters<LiteralType, Literal> STRING_ID_ADAPTERS = new Merging.IdAdapters<>(
            LiteralTypes.string(),
            LiteralTypes.string(),
            label -> StatelessCoder.of(literal -> literal.accept(new Literal.PartialVisitor<Flow<Unit, Literal>>() {
                @Override
                public Flow<Unit, Literal> otherwise(Literal instance) {
                    return Flows.unexpected("string literal", instance);
                }

                @Override
                public Flow<Unit, Literal> visit(Literal.String_ id) {
                    return pure(Literals.string(
                            Basics.decapitalize(label.value) + "_" + ((Literal.String_) literal).value));
                }
            }), literal -> literal.accept(new Literal.PartialVisitor<Flow<Unit, Literal>>() {
                @Override
                public Flow<Unit, Literal> otherwise(Literal instance) {
                    return Flows.unexpected("string literal", instance);
                }

                @Override
                public Flow<Unit, Literal> visit(Literal.String_ id) {
                    return pure(Literals.string(id.value.substring(label.value.length() + 1)));
                }
            })),
            label -> StatelessCoder.of(literal -> literal.accept(new Literal.PartialVisitor<Flow<Unit, Literal>>() {
                @Override
                public Flow<Unit, Literal> otherwise(Literal instance) {
                    return Flows.unexpected("string literal", instance);
                }

                @Override
                public Flow<Unit, Literal> visit(Literal.String_ id) {
                    return pure(Literals.string(
                            Basics.decapitalize(label.value) + "_" + ((Literal.String_) literal).value));
                }
            }), literal -> literal.accept(new Literal.PartialVisitor<Flow<Unit, Literal>>() {
                @Override
                public Flow<Unit, Literal> otherwise(Literal instance) {
                    return Flows.unexpected("string literal", instance);
                }

                @Override
                public Flow<Unit, Literal> visit(Literal.String_ id) {
                    return pure(Literals.string(id.value.substring(label.value.length() + 1)));
                }
            })));
}

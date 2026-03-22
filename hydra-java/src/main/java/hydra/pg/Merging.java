package hydra.pg;

import hydra.util.StatelessAdapter;
import hydra.util.StatelessCoder;
import hydra.core.Literal;
import hydra.core.LiteralType;
import hydra.util.Either;
import hydra.util.Maybe;
import hydra.dsl.LiteralTypes;
import hydra.dsl.Literals;
import hydra.pg.model.Edge;
import hydra.pg.model.EdgeLabel;
import hydra.pg.model.EdgeType;
import hydra.pg.model.PropertyKey;
import hydra.pg.model.PropertyType;
import hydra.pg.model.Vertex;
import hydra.pg.model.VertexLabel;
import hydra.pg.model.VertexType;

import hydra.util.ConsList;
import hydra.util.PersistentMap;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;


/**
 * Utilities for combining multiple vertex or edge types into a single "merged" vertex or edge type,
 * and correspondingly encoding and decoding vertices and edges of these types.
 */
public class Merging {
    public static VertexLabel DEFAULT_VERTEX_LABEL = new VertexLabel("_Merged");
    public static EdgeLabel DEFAULT_EDGE_LABEL = new EdgeLabel("_merged");

    public static final Merging.IdAdapters<LiteralType, Literal> STRING_ID_ADAPTERS
        = stringIdAdapters(
        LiteralTypes.string(),
        new Function<Literal, Either<String, String>>() {
            @Override
            public Either<String, String> apply(Literal literal) {
                return literal.accept(new Literal.PartialVisitor<>() {
                    @Override
                    public Either<String, String> otherwise(Literal instance) {
                        return Either.left("Expected string literal, found " + instance);
                    }

                    @Override
                    public Either<String, String> visit(Literal.String_ instance) {
                        return Either.right(instance.value);
                    }
                });
            }
        },
        Literals::string);

    /**
     * Create a vertex adapter based on a list of vertex types and given id adapters.
     *
     * @param <T> the type parameter for vertex types
     * @param <V> the value type for vertex properties
     * @param types the list of vertex types to merge
     * @param idAdapters the id adapters for encoding/decoding vertex and edge ids
     * @param unifyIdenticalTypes whether to unify identical property types across vertex types
     * @return an Either containing a stateless adapter for the merged vertex type, or an error
     */
    public static <T, V> Either<String, StatelessAdapter<List<VertexType<T>>, VertexType<T>, Vertex<V>, Vertex<V>>>
    createVertexAdapter(List<VertexType<T>> types,
        IdAdapters<T, V> idAdapters,
        boolean unifyIdenticalTypes) {
        Maybe<String> err1 = checkNontrivial(types);
        if (err1.isJust()) return Either.left(err1.fromJust());
        Maybe<String> err2 = checkNoDuplicatedVertexLabels(types);
        if (err2.isJust()) return Either.left(err2.fromJust());

        MergedEntity<VertexType<T>> mergedType = mergeVertexTypes(types, idAdapters, unifyIdenticalTypes);
        return Either.right(new StatelessAdapter<List<VertexType<T>>, VertexType<T>, Vertex<V>, Vertex<V>>(
            false, types, mergedType.entity,
            constructMergedVertexCoder(types, idAdapters, mergedType.unifiedProperties)));
    }

    /**
     * Create an edge adapter based on a list of edge types and given id adapters.
     *
     * @param <T> the type parameter for edge types
     * @param <V> the value type for edge properties
     * @param types the list of edge types to merge
     * @param idAdapters the id adapters for encoding/decoding vertex and edge ids
     * @param unifyIdenticalTypes whether to unify identical property types across edge types
     * @return an Either containing a stateless adapter for the merged edge type, or an error
     */
    public static <T, V> Either<String, StatelessAdapter<List<EdgeType<T>>, EdgeType<T>, Edge<V>, Edge<V>>>
    createEdgeAdapter(List<EdgeType<T>> types,
        IdAdapters<T, V> idAdapters,
        boolean unifyIdenticalTypes) {
        Maybe<String> err1 = checkNontrivial(types);
        if (err1.isJust()) return Either.left(err1.fromJust());
        Maybe<String> err2 = checkNoDuplicatedEdgeLabels(types);
        if (err2.isJust()) return Either.left(err2.fromJust());

        MergedEntity<EdgeType<T>> mergedType = mergeEdgeTypes(types, idAdapters, unifyIdenticalTypes);
        return Either.right(new StatelessAdapter<List<EdgeType<T>>, EdgeType<T>, Edge<V>, Edge<V>>(
            false, types, mergedType.entity,
            constructMergedEdgeCoder(types, idAdapters, mergedType.unifiedProperties)));
    }

    private static <A> Maybe<String> checkNontrivial(List<A> types) {
        return types.isEmpty()
            ? Maybe.just("No types provided")
            : Maybe.nothing();
    }

    private static <T> Maybe<String> checkNoDuplicatedVertexLabels(List<VertexType<T>> types) {
        Set<VertexLabel> labels = new HashSet<>();
        for (VertexType<T> type : types) {
            if (!labels.add(type.label)) {
                return Maybe.just("Duplicate vertex label: " + type.label);
            }
        }
        return Maybe.nothing();
    }

    private static <T> Maybe<String> checkNoDuplicatedEdgeLabels(List<EdgeType<T>> types) {
        Set<EdgeLabel> labels = new HashSet<>();
        for (EdgeType<T> type : types) {
            if (!labels.add(type.label)) {
                return Maybe.just("Duplicate edge label: " + type.label);
            }
        }
        return Maybe.nothing();
    }

    private static <T, V> StatelessCoder<Vertex<V>, Vertex<V>> constructMergedVertexCoder(
        List<VertexType<T>> types,
        IdAdapters<T, V> idAdapters,
        Set<PropertyKey> unifiedPropertyKeys) {
        Map<VertexLabel, StatelessCoder<Vertex<V>, Vertex<V>>> coders
            = constructVertexCoders(types, idAdapters, unifiedPropertyKeys);
        return new StatelessCoder<Vertex<V>, Vertex<V>>(
            v -> {
                Either<String, StatelessCoder<Vertex<V>, Vertex<V>>> coderResult = getCoder(coders, v.label);
                if (coderResult.isLeft()) return Either.left(((Either.Left<String, StatelessCoder<Vertex<V>, Vertex<V>>>) coderResult).value);
                StatelessCoder<Vertex<V>, Vertex<V>> coder = ((Either.Right<String, StatelessCoder<Vertex<V>, Vertex<V>>>) coderResult).value;
                return applyCoderEncode(coder, v);
            },
            v -> {
                Either<String, StatelessCoder<Vertex<V>, Vertex<V>>> coderResult = getCoder(coders, v.label);
                if (coderResult.isLeft()) return Either.left(((Either.Left<String, StatelessCoder<Vertex<V>, Vertex<V>>>) coderResult).value);
                StatelessCoder<Vertex<V>, Vertex<V>> coder = ((Either.Right<String, StatelessCoder<Vertex<V>, Vertex<V>>>) coderResult).value;
                return applyCoderDecode(coder, v);
            });
    }

    private static <T, V> StatelessCoder<Edge<V>, Edge<V>> constructMergedEdgeCoder(
        List<EdgeType<T>> types,
        IdAdapters<T, V> idAdapters,
        Set<PropertyKey> unifiedPropertyKeys) {
        Map<EdgeLabel, StatelessCoder<Edge<V>, Edge<V>>> coders
            = constructEdgeCoders(types, idAdapters, unifiedPropertyKeys);
        return new StatelessCoder<Edge<V>, Edge<V>>(
            e -> {
                Either<String, StatelessCoder<Edge<V>, Edge<V>>> coderResult = getCoder(coders, e.label);
                if (coderResult.isLeft()) return Either.left(((Either.Left<String, StatelessCoder<Edge<V>, Edge<V>>>) coderResult).value);
                StatelessCoder<Edge<V>, Edge<V>> coder = ((Either.Right<String, StatelessCoder<Edge<V>, Edge<V>>>) coderResult).value;
                return applyCoderEncode(coder, e);
            },
            e -> {
                Either<String, StatelessCoder<Edge<V>, Edge<V>>> coderResult = getCoder(coders, e.label);
                if (coderResult.isLeft()) return Either.left(((Either.Left<String, StatelessCoder<Edge<V>, Edge<V>>>) coderResult).value);
                StatelessCoder<Edge<V>, Edge<V>> coder = ((Either.Right<String, StatelessCoder<Edge<V>, Edge<V>>>) coderResult).value;
                return applyCoderDecode(coder, e);
            });
    }

    /**
     * Apply a StatelessCoder's encode function (via its Coder.encode, which takes Context).
     */
    private static <V> Either<String, V> applyCoderEncode(StatelessCoder<V, V> coder, V value) {
        hydra.context.Context cx = new hydra.context.Context(
            hydra.util.ConsList.empty(), hydra.util.ConsList.empty(), hydra.util.PersistentMap.empty());
        hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, V> result = coder.encode.apply(cx).apply(value);
        if (result.isRight()) {
            return Either.right(((Either.Right<hydra.context.InContext<hydra.errors.Error_>, V>) result).value);
        } else {
            return Either.left(hydra.show.Errors.error(((Either.Left<hydra.context.InContext<hydra.errors.Error_>, V>) result).value.object));
        }
    }

    /**
     * Apply a StatelessCoder's decode function (via its Coder.decode, which takes Context).
     */
    private static <V> Either<String, V> applyCoderDecode(StatelessCoder<V, V> coder, V value) {
        hydra.context.Context cx = new hydra.context.Context(
            hydra.util.ConsList.empty(), hydra.util.ConsList.empty(), hydra.util.PersistentMap.empty());
        hydra.util.Either<hydra.context.InContext<hydra.errors.Error_>, V> result = coder.decode.apply(cx).apply(value);
        if (result.isRight()) {
            return Either.right(((Either.Right<hydra.context.InContext<hydra.errors.Error_>, V>) result).value);
        } else {
            return Either.left(hydra.show.Errors.error(((Either.Left<hydra.context.InContext<hydra.errors.Error_>, V>) result).value.object));
        }
    }

    private static <T, V> StatelessCoder<Vertex<V>, Vertex<V>> constructVertexCoder(
        VertexType<T> type,
        IdAdapters<T, V> idAdapters,
        Set<PropertyKey> unifiedPropertyKeys) {
        StatelessCoder<PersistentMap<PropertyKey, V>, PersistentMap<PropertyKey, V>> propertiesCoder
            = constructPropertiesCoder(type.label.value, unifiedPropertyKeys);

        return new StatelessCoder<Vertex<V>, Vertex<V>>(
            v -> {
                Either<String, V> idResult = applyCoderEncode(idAdapters.forVertexId.apply(type.label), v.id);
                if (idResult.isLeft()) return Either.left(((Either.Left<String, V>) idResult).value);
                Either<String, PersistentMap<PropertyKey, V>> propsResult = applyCoderEncode(propertiesCoder, v.properties);
                if (propsResult.isLeft()) return Either.left(((Either.Left<String, PersistentMap<PropertyKey, V>>) propsResult).value);
                return Either.right(new Vertex<V>(v.label,
                    ((Either.Right<String, V>) idResult).value,
                    ((Either.Right<String, PersistentMap<PropertyKey, V>>) propsResult).value));
            },
            v -> {
                Either<String, V> idResult = applyCoderDecode(idAdapters.forVertexId.apply(type.label), v.id);
                if (idResult.isLeft()) return Either.left(((Either.Left<String, V>) idResult).value);
                Either<String, PersistentMap<PropertyKey, V>> propsResult = applyCoderDecode(propertiesCoder, v.properties);
                if (propsResult.isLeft()) return Either.left(((Either.Left<String, PersistentMap<PropertyKey, V>>) propsResult).value);
                return Either.right(new Vertex<V>(v.label,
                    ((Either.Right<String, V>) idResult).value,
                    ((Either.Right<String, PersistentMap<PropertyKey, V>>) propsResult).value));
            });
    }

    private static <T, V> StatelessCoder<Edge<V>, Edge<V>> constructEdgeCoder(
        EdgeType<T> type,
        IdAdapters<T, V> idAdapters,
        Set<PropertyKey> unifiedPropertyKeys) {
        StatelessCoder<PersistentMap<PropertyKey, V>, PersistentMap<PropertyKey, V>> propertiesCoder
            = constructPropertiesCoder(type.label.value, unifiedPropertyKeys);

        return new StatelessCoder<Edge<V>, Edge<V>>(
            e -> {
                Either<String, V> idResult = applyCoderEncode(idAdapters.forEdgeId.apply(type.label), e.id);
                if (idResult.isLeft()) return Either.left(((Either.Left<String, V>) idResult).value);
                Either<String, V> outResult = applyCoderEncode(idAdapters.forVertexId.apply(type.out), e.out);
                if (outResult.isLeft()) return Either.left(((Either.Left<String, V>) outResult).value);
                Either<String, V> inResult = applyCoderEncode(idAdapters.forVertexId.apply(type.in), e.in);
                if (inResult.isLeft()) return Either.left(((Either.Left<String, V>) inResult).value);
                Either<String, PersistentMap<PropertyKey, V>> propsResult = applyCoderEncode(propertiesCoder, e.properties);
                if (propsResult.isLeft()) return Either.left(((Either.Left<String, PersistentMap<PropertyKey, V>>) propsResult).value);
                return Either.right(new Edge<V>(e.label,
                    ((Either.Right<String, V>) idResult).value,
                    ((Either.Right<String, V>) outResult).value,
                    ((Either.Right<String, V>) inResult).value,
                    ((Either.Right<String, PersistentMap<PropertyKey, V>>) propsResult).value));
            },
            e -> {
                Either<String, V> idResult = applyCoderDecode(idAdapters.forEdgeId.apply(type.label), e.id);
                if (idResult.isLeft()) return Either.left(((Either.Left<String, V>) idResult).value);
                Either<String, V> outResult = applyCoderDecode(idAdapters.forVertexId.apply(type.out), e.out);
                if (outResult.isLeft()) return Either.left(((Either.Left<String, V>) outResult).value);
                Either<String, V> inResult = applyCoderDecode(idAdapters.forVertexId.apply(type.in), e.in);
                if (inResult.isLeft()) return Either.left(((Either.Left<String, V>) inResult).value);
                Either<String, PersistentMap<PropertyKey, V>> propsResult = applyCoderDecode(propertiesCoder, e.properties);
                if (propsResult.isLeft()) return Either.left(((Either.Left<String, PersistentMap<PropertyKey, V>>) propsResult).value);
                return Either.right(new Edge<V>(e.label,
                    ((Either.Right<String, V>) idResult).value,
                    ((Either.Right<String, V>) outResult).value,
                    ((Either.Right<String, V>) inResult).value,
                    ((Either.Right<String, PersistentMap<PropertyKey, V>>) propsResult).value));
            });
    }

    private static <V> StatelessCoder<PersistentMap<PropertyKey, V>, PersistentMap<PropertyKey, V>> constructPropertiesCoder(
        String label,
        Set<PropertyKey> unifiedPropertyKeys) {
        Function<PersistentMap<PropertyKey, V>, Either<String, PersistentMap<PropertyKey, V>>> encode = before -> {
            PersistentMap<PropertyKey, V> after = PersistentMap.empty();
            for (Map.Entry<PropertyKey, V> entry : before.entrySet()) {
                after = after.insert(encodePropertyKey(label, entry.getKey(), unifiedPropertyKeys), entry.getValue());
            }
            return Either.right(after);
        };

        Function<PersistentMap<PropertyKey, V>, Either<String, PersistentMap<PropertyKey, V>>> decode = before -> {
            PersistentMap<PropertyKey, V> after = PersistentMap.empty();
            for (Map.Entry<PropertyKey, V> entry : before.entrySet()) {
                after = after.insert(decodePropertyKey(label, entry.getKey(), unifiedPropertyKeys), entry.getValue());
            }
            return Either.right(after);
        };

        return new StatelessCoder<PersistentMap<PropertyKey, V>, PersistentMap<PropertyKey, V>>(encode, decode);
    }

    private static <V, T, L> Map<L, StatelessCoder<V, V>> constructCoders(
        List<T> types,
        Function<T, L> getLabel,
        Function<T, StatelessCoder<V, V>> constructCoder) {
        Map<L, StatelessCoder<V, V>> coders = new HashMap<L, StatelessCoder<V, V>>();
        for (T type : types) {
            coders.put(getLabel.apply(type), constructCoder.apply(type));
        }
        return coders;
    }

    private static <T, V> Map<VertexLabel, StatelessCoder<Vertex<V>, Vertex<V>>> constructVertexCoders(
        List<VertexType<T>> types,
        IdAdapters<T, V> idAdapters,
        Set<PropertyKey> unifiedPropertyKeys) {
        return constructCoders(types, type -> type.label,
            type -> constructVertexCoder(type, idAdapters, unifiedPropertyKeys));
    }

    private static <T, V> Map<EdgeLabel, StatelessCoder<Edge<V>, Edge<V>>> constructEdgeCoders(
        List<EdgeType<T>> types,
        IdAdapters<T, V> idAdapters,
        Set<PropertyKey> unifiedPropertyKeys) {
        return constructCoders(types, type -> type.label,
            type -> constructEdgeCoder(type, idAdapters, unifiedPropertyKeys));
    }

    private static PropertyKey encodePropertyKey(String label, PropertyKey key, Set<PropertyKey> unifiedPropertyKeys) {
        if (unifiedPropertyKeys.contains(key)) {
            return key;
        } else {
            String prefix = decapitalize(label) + "_";
            return new PropertyKey(prefix + key.value);
        }
    }

    private static PropertyKey decodePropertyKey(String label, PropertyKey key, Set<PropertyKey> unifiedPropertyKeys) {
        if (unifiedPropertyKeys.contains(key)) {
            return key;
        } else {
            return new PropertyKey(key.value.substring(label.length() + 1));
        }
    }

    private static <L, E> Either<String, StatelessCoder<E, E>> getCoder(Map<L, StatelessCoder<E, E>> coders, L label) {
        StatelessCoder<E, E> helper = coders.get(label);
        return helper == null
            ? Either.left("No coder associated with label " + label)
            : Either.right(helper);
    }

    private static <T, V> MergedEntity<VertexType<T>> mergeVertexTypes(List<VertexType<T>> types,
        IdAdapters<T, V> idAdapters,
        boolean unifyIdenticalTypes) {
        MergedEntity<List<PropertyType<T>>> mergedProps = mergePropertyTypes(
            types, t -> t.label, l -> l.value, t -> new ArrayList<>(t.properties), unifyIdenticalTypes);
        VertexType<T> type = new VertexType<T>(DEFAULT_VERTEX_LABEL, idAdapters.mergedVertexIdType,
            ConsList.fromList(mergedProps.entity));
        return new MergedEntity<VertexType<T>>(type, mergedProps.unifiedProperties);
    }

    private static <T, V> MergedEntity<EdgeType<T>> mergeEdgeTypes(List<EdgeType<T>> types,
        IdAdapters<T, V> idAdapters,
        boolean unifyIdenticalTypes) {
        MergedEntity<List<PropertyType<T>>> mergedProps = mergePropertyTypes(
            types, t -> t.label, l -> l.value, t -> new ArrayList<>(t.properties), unifyIdenticalTypes);
        EdgeType<T> type = new EdgeType<T>(DEFAULT_EDGE_LABEL, idAdapters.mergedEdgeIdType,
            DEFAULT_VERTEX_LABEL, DEFAULT_VERTEX_LABEL,
            ConsList.fromList(mergedProps.entity));
        return new MergedEntity<EdgeType<T>>(type, mergedProps.unifiedProperties);
    }

    /**
     * Produces a set of merged properties together with some metadata on unification:
     * if all property types for a given key (before merging) are equal, then a single merged property type is produced
     * for that key. If any are unequal, then an individual property type is produced for each, where the key is
     * prefixed by the vertex or edge label for disambiguation.
     */
    private static <T, E, L> MergedEntity<List<PropertyType<T>>> mergePropertyTypes(
        List<E> types,
        Function<E, L> getLabel,
        Function<L, String> labelToString,
        Function<E, List<PropertyType<T>>> getProperties,
        boolean unifyIdenticalTypes) {

        // Properties to be unified across types
        Set<PropertyKey> unifiedProperties;
        // Properties which are also present in all types, and required in all types
        Set<PropertyKey> requiredProperties;

        if (unifyIdenticalTypes) {
            Map<PropertyKey, List<PropertyType<T>>> typesForPropertyKey
                = new HashMap<PropertyKey, List<PropertyType<T>>>();
            for (E type : types) {
                for (PropertyType<T> ptype : getProperties.apply(type)) {
                    List<PropertyType<T>> typesForThisKey = typesForPropertyKey
                        .computeIfAbsent(ptype.key, k -> new ArrayList<PropertyType<T>>());
                    typesForThisKey.add(ptype);
                }
            }
            unifiedProperties = new HashSet<>();
            requiredProperties = new HashSet<>();
            for (Map.Entry<PropertyKey, List<PropertyType<T>>> entry : typesForPropertyKey.entrySet()) {
                if (allPropertyTypesAreEqual(entry.getValue())) {
                    unifiedProperties.add(entry.getKey());

                    // Also make the property required if it is present and required in all types
                    if (entry.getValue().size() == types.size() && allPropertyTypesAreRequired(entry.getValue())) {
                        requiredProperties.add(entry.getKey());
                    }
                }
            }
        } else {
            unifiedProperties = Collections.emptySet();
            requiredProperties = Collections.emptySet();
        }

        List<PropertyType<T>> ptypes = new ArrayList<PropertyType<T>>();
        Set<PropertyKey> visited = new HashSet<PropertyKey>();
        for (E e : types) {
            for (PropertyType<T> ptype : getProperties.apply(e)) {
                if (unifiedProperties.contains(ptype.key)) {
                    if (visited.contains(ptype.key)) {
                        continue;
                    }
                    ptypes.add(new PropertyType<T>(ptype.key, ptype.value, requiredProperties.contains(ptype.key)));
                    visited.add(ptype.key);
                } else {
                    boolean required = ptype.required && types.size() == 1;
                    ptypes.add(new PropertyType<T>(
                        encodePropertyKey(labelToString.apply(getLabel.apply(e)), ptype.key, unifiedProperties),
                        ptype.value, required));
                }
            }
        }
        return new MergedEntity<List<PropertyType<T>>>(ptypes, unifiedProperties);
    }

    private static <T> boolean allPropertyTypesAreEqual(List<PropertyType<T>> types) {
        if (types.isEmpty()) {
            return true;
        }
        PropertyType<T> first = types.get(0);
        for (int i = 1; i < types.size(); ++i) {
            if (!types.get(i).value.equals(first.value)) {
                return false;
            }
        }
        return true;
    }

    private static <T> boolean allPropertyTypesAreRequired(List<PropertyType<T>> types) {
        for (PropertyType<T> type : types) {
            if (!type.required) {
                return false;
            }
        }
        return true;
    }

    /**
     * A helper object which defines merged vertex and edge id types, and a value coder for each vertex and edge label.
     *
     * @param <T> the type parameter for id types
     * @param <V> the value type for ids
     */
    public static class IdAdapters<T, V> {
        public final T mergedVertexIdType;
        public final T mergedEdgeIdType;
        public final Function<VertexLabel, StatelessCoder<V, V>> forVertexId;
        public final Function<EdgeLabel, StatelessCoder<V, V>> forEdgeId;

        /**
         * Construct the helper object.
         *
         * @param mergedVertexIdType the merged type for vertex ids
         * @param mergedEdgeIdType the merged type for edge ids
         * @param forVertexId function to get a coder for a given vertex label
         * @param forEdgeId function to get a coder for a given edge label
         */
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

    /**
     * Create string-based id adapters for merging vertex and edge types.
     *
     * @param <T> the type parameter for id types
     * @param <V> the value type for ids
     * @param stringType the string type to use for merged ids
     * @param fromLiteral function to extract a string from a literal value
     * @param toLiteral function to create a literal value from a string
     * @return id adapters configured for string-based ids
     */
    public static <T, V> Merging.IdAdapters<T, V> stringIdAdapters(
        T stringType,
        Function<V, Either<String, String>> fromLiteral,
        Function<String, V> toLiteral) {
        return new Merging.IdAdapters<>(
            stringType,
            stringType,
            label -> StatelessCoder.of(
                literal -> {
                    Either<String, String> r = fromLiteral.apply(literal);
                    if (r.isLeft()) return Either.left(((Either.Left<String, String>) r).value);
                    return Either.right(toLiteral.apply(decapitalize(label.value) + "_" + ((Either.Right<String, String>) r).value));
                },
                literal -> {
                    Either<String, String> r = fromLiteral.apply(literal);
                    if (r.isLeft()) return Either.left(((Either.Left<String, String>) r).value);
                    return Either.right(toLiteral.apply(((Either.Right<String, String>) r).value.substring(label.value.length() + 1)));
                }),
            label -> StatelessCoder.of(
                literal -> {
                    Either<String, String> r = fromLiteral.apply(literal);
                    if (r.isLeft()) return Either.left(((Either.Left<String, String>) r).value);
                    return Either.right(toLiteral.apply(decapitalize(label.value) + "_" + ((Either.Right<String, String>) r).value));
                },
                literal -> {
                    Either<String, String> r = fromLiteral.apply(literal);
                    if (r.isLeft()) return Either.left(((Either.Left<String, String>) r).value);
                    return Either.right(toLiteral.apply(((Either.Right<String, String>) r).value.substring(label.value.length() + 1)));
                }));
    }

    private static class MergedEntity<A> {
        private final A entity;
        private final Set<PropertyKey> unifiedProperties;

        private MergedEntity(A entity, Set<PropertyKey> unifiedProperties) {
            this.entity = entity;
            this.unifiedProperties = unifiedProperties;
        }
    }

    // TODO: inline implementation until hydra.basics.Basics is generated
    private static String decapitalize(String s) {
        if (s == null || s.isEmpty()) {
            return s;
        }
        return Character.toLowerCase(s.charAt(0)) + s.substring(1);
    }
}

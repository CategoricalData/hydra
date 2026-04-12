package hydra.pg;

import hydra.util.StatelessAdapter;
import hydra.core.Literal;
import hydra.core.LiteralType;
import hydra.util.Either;
import hydra.dsl.LiteralTypes;
import hydra.dsl.Literals;
import hydra.pg.model.Edge;
import hydra.pg.model.EdgeType;
import hydra.pg.model.PropertyKey;
import hydra.pg.model.Vertex;
import hydra.pg.model.VertexLabel;
import hydra.pg.model.VertexType;
import java.util.Arrays;
import java.util.List;
import java.util.function.Function;
import org.junit.jupiter.api.Test;


public class MergingTest extends PropertyGraphTestBase {
    private static final boolean DO_NOT_UNIFY = false;
    private static final boolean UNIFY = true;

    /** Map over an Either Right value. */
    private static <A, B> Either<String, B> map(Either<String, A> either, Function<A, B> fn) {
        if (either.isLeft()) return Either.left(((Either.Left<String, A>) either).value);
        return Either.right(fn.apply(((Either.Right<String, A>) either).value));
    }

    @Test
    public void testFailOnEmptyListOfInputTypes() {
        assertFails(Merging.createVertexAdapter(Arrays.asList(), Merging.STRING_ID_ADAPTERS, DO_NOT_UNIFY));
        assertFails(Merging.createEdgeAdapter(Arrays.asList(), Merging.STRING_ID_ADAPTERS, DO_NOT_UNIFY));
    }

    @Test
    public void failOnDuplicateLabels() {
        assertFails(Merging.createVertexAdapter(
                Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_PERSON_C),
                Merging.STRING_ID_ADAPTERS, DO_NOT_UNIFY));
        assertFails(Merging.createEdgeAdapter(
                Arrays.asList(EDGE_TYPE_WORKSAT_A, EDGE_TYPE_WORKSAT_C),
                Merging.STRING_ID_ADAPTERS, DO_NOT_UNIFY));
    }

    @Test
    public void testVertexTypeIsAsExpectedWithoutUnification() {
        boolean unify = DO_NOT_UNIFY;
        assertSucceedsWith(2,
                map(createVA(unify), adapter -> adapter.source.size()));
        assertSucceedsWith(Merging.DEFAULT_VERTEX_LABEL,
                map(createVA(unify), adapter -> adapter.target.label));
        assertSucceedsWith(9,
                map(createVA(unify), adapter -> adapter.target.properties.size()));
        assertSucceedsWith(false,
                map(createVA(unify), adapter -> adapter.isLossy));

        assertSucceedsWith("person_name",
                map(createVA(unify), adapter -> adapter.target.properties.get(0).key.value));
        assertSucceedsWith("person_nickname",
                map(createVA(unify), adapter -> adapter.target.properties.get(1).key.value));
        assertSucceedsWith("person_age",
                map(createVA(unify), adapter -> adapter.target.properties.get(2).key.value));
        assertSucceedsWith("person_birthTimeUnixMilliseconds",
                map(createVA(unify), adapter -> adapter.target.properties.get(3).key.value));
        assertSucceedsWith("organization_name",
                map(createVA(unify), adapter -> adapter.target.properties.get(4).key.value));
        assertSucceedsWith("organization_nickname",
                map(createVA(unify), adapter -> adapter.target.properties.get(5).key.value));
        assertSucceedsWith("organization_age",
                map(createVA(unify), adapter -> adapter.target.properties.get(6).key.value));
        assertSucceedsWith("organization_industry",
                map(createVA(unify), adapter -> adapter.target.properties.get(7).key.value));

        assertSucceedsWith(false,
                map(createVA(unify), adapter -> adapter.target.properties.get(0).required));
        assertSucceedsWith(LiteralTypes.string(),
                map(createVA(unify), adapter -> adapter.target.properties.get(0).value));
    }

    @Test
    public void testVertexTypeIsAsExpectedWithUnification() {
        boolean unify = UNIFY;

        assertSucceedsWith(2,
                map(createVA(unify), adapter -> adapter.source.size()));
        assertSucceedsWith(Merging.DEFAULT_VERTEX_LABEL,
                map(createVA(unify), adapter -> adapter.target.label));
        assertSucceedsWith(7,
                map(createVA(unify), adapter -> adapter.target.properties.size()));
        assertSucceedsWith(false,
                map(createVA(unify), adapter -> adapter.isLossy));

        assertSucceedsWith("name",
                map(createVA(unify), adapter -> adapter.target.properties.get(0).key.value));
        assertSucceedsWith("nickname",
                map(createVA(unify), adapter -> adapter.target.properties.get(1).key.value));
        assertSucceedsWith("person_age",
                map(createVA(unify), adapter -> adapter.target.properties.get(2).key.value));
        assertSucceedsWith("birthTimeUnixMilliseconds",
                map(createVA(unify), adapter -> adapter.target.properties.get(3).key.value));
        assertSucceedsWith("organization_age",
                map(createVA(unify), adapter -> adapter.target.properties.get(4).key.value));
        assertSucceedsWith("industry",
                map(createVA(unify), adapter -> adapter.target.properties.get(5).key.value));

        assertSucceedsWith(true,
                map(createVA(unify), adapter -> adapter.target.properties.get(0).required));
        assertSucceedsWith(LiteralTypes.string(),
                map(createVA(unify), adapter -> adapter.target.properties.get(0).value));
        assertSucceedsWith(false,
                map(createVA(unify), adapter -> adapter.target.properties.get(1).required));
        assertSucceedsWith(false,
                map(createVA(unify), adapter -> adapter.target.properties.get(2).required));
        assertSucceedsWith(LiteralTypes.int32(),
                map(createVA(unify), adapter -> adapter.target.properties.get(2).value));
        assertSucceedsWith(false,
                map(createVA(unify), adapter -> adapter.target.properties.get(3).required));
        assertSucceedsWith(LiteralTypes.uint64(),
                map(createVA(unify), adapter -> adapter.target.properties.get(3).value));
        assertSucceedsWith(false,
                map(createVA(unify), adapter -> adapter.target.properties.get(4).required));
        assertSucceedsWith(LiteralTypes.int64(),
                map(createVA(unify), adapter -> adapter.target.properties.get(4).value));
        assertSucceedsWith(false,
                map(createVA(unify), adapter -> adapter.target.properties.get(5).required));
    }

    @Test
    public void testVertexEncodingIsAsExpectedWithoutUnification() {
        boolean unify = DO_NOT_UNIFY;

        assertSucceedsWith(Literals.string("person_arthur"), map(encodeVertex(VERTEX_PERSON_1, unify), v -> v.id));
        assertSucceedsWith(VERTEX_PERSON_1.label, map(encodeVertex(VERTEX_PERSON_1, unify), v -> v.label));
        assertSucceedsWith(true, map(encodeVertex(VERTEX_PERSON_1, unify),
                v -> v.properties.get(new PropertyKey("name")) == null));
        assertSucceedsWith(true, map(encodeVertex(VERTEX_PERSON_1, unify),
                v -> v.properties.get(new PropertyKey("organization_name")) == null));
        assertSucceedsWith(Literals.string("Arthur Dent"), map(encodeVertex(VERTEX_PERSON_1, unify),
                v -> v.properties.get(new PropertyKey("person_name"))));
        assertSucceedsWith(true, map(encodeVertex(VERTEX_PERSON_1, unify),
                v -> v.properties.get(new PropertyKey("person_nickname")) == null));
        assertSucceedsWith(Literals.int32(42), map(encodeVertex(VERTEX_PERSON_1, unify),
                v -> v.properties.get(new PropertyKey("person_age"))));

        assertSucceedsWith(Literals.string("Ford Prefect"), map(encodeVertex(VERTEX_PERSON_2, unify),
                v -> v.properties.get(new PropertyKey("person_name"))));
        assertSucceedsWith(Literals.string("Ix"), map(encodeVertex(VERTEX_PERSON_2, unify),
                v -> v.properties.get(new PropertyKey("person_nickname"))));

        assertSucceedsWith(Literals.string("organization_megadodo"),
                map(encodeVertex(VERTEX_ORGANIZATION_1, unify), v -> v.id));
        assertSucceedsWith(VERTEX_ORGANIZATION_1.label,
                map(encodeVertex(VERTEX_ORGANIZATION_1, unify), v -> v.label));
        assertSucceedsWith(true, map(encodeVertex(VERTEX_ORGANIZATION_1, unify),
                v -> v.properties.get(new PropertyKey("name")) == null));
        assertSucceedsWith(true, map(encodeVertex(VERTEX_ORGANIZATION_1, unify),
                v -> v.properties.get(new PropertyKey("person_name")) == null));
        assertSucceedsWith(Literals.string("Megadodo Publications"),
                map(encodeVertex(VERTEX_ORGANIZATION_1, unify),
                        v -> v.properties.get(new PropertyKey("organization_name"))));
        assertSucceedsWith(Literals.string("publishers"),
                map(encodeVertex(VERTEX_ORGANIZATION_1, unify),
                        v -> v.properties.get(new PropertyKey("organization_industry"))));
        assertSucceedsWith(Literals.int32(1000042),
                map(encodeVertex(VERTEX_ORGANIZATION_1, unify),
                        v -> v.properties.get(new PropertyKey("organization_numberOfEmployees"))));

        assertSucceedsWith(Literals.string("Infinidim Enterprises"),
                map(encodeVertex(VERTEX_ORGANIZATION_2, unify),
                        v -> v.properties.get(new PropertyKey("organization_name"))));
        assertSucceedsWith(Literals.string("all-powerful conglomerates"),
                map(encodeVertex(VERTEX_ORGANIZATION_2, unify),
                        v -> v.properties.get(new PropertyKey("organization_industry"))));
        assertSucceedsWith(true, map(encodeVertex(VERTEX_ORGANIZATION_2, unify),
                v -> v.properties.get(new PropertyKey("organization_numberOfEmployees")) == null));
    }

    @Test
    public void testVertexEncodingIsAsExpectedWithUnification() {
        boolean unify = UNIFY;

        assertSucceedsWith(Literals.string("person_arthur"), map(encodeVertex(VERTEX_PERSON_1, unify), v -> v.id));
        assertSucceedsWith(VERTEX_PERSON_1.label, map(encodeVertex(VERTEX_PERSON_1, unify), v -> v.label));
        assertSucceedsWith(Literals.string("Arthur Dent"), map(encodeVertex(VERTEX_PERSON_1, unify),
                v -> v.properties.get(new PropertyKey("name"))));
        assertSucceedsWith(true, map(encodeVertex(VERTEX_PERSON_1, unify),
                v -> v.properties.get(new PropertyKey("nickname")) == null));
        assertSucceedsWith(Literals.int32(42), map(encodeVertex(VERTEX_PERSON_1, unify),
                v -> v.properties.get(new PropertyKey("person_age"))));

        assertSucceedsWith(Literals.string("Ford Prefect"), map(encodeVertex(VERTEX_PERSON_2, unify),
                v -> v.properties.get(new PropertyKey("name"))));
        assertSucceedsWith(Literals.string("Ix"), map(encodeVertex(VERTEX_PERSON_2, unify),
                v -> v.properties.get(new PropertyKey("nickname"))));

        assertSucceedsWith(Literals.string("organization_megadodo"),
                map(encodeVertex(VERTEX_ORGANIZATION_1, unify), v -> v.id));
        assertSucceedsWith(VERTEX_ORGANIZATION_1.label,
                map(encodeVertex(VERTEX_ORGANIZATION_1, unify), v -> v.label));
        assertSucceedsWith(Literals.string("Megadodo Publications"),
                map(encodeVertex(VERTEX_ORGANIZATION_1, unify),
                        v -> v.properties.get(new PropertyKey("name"))));
        assertSucceedsWith(Literals.string("Megadodo"), map(encodeVertex(VERTEX_ORGANIZATION_1, unify),
                v -> v.properties.get(new PropertyKey("nickname"))));
        assertSucceedsWith(Literals.string("publishers"), map(encodeVertex(VERTEX_ORGANIZATION_1, unify),
                v -> v.properties.get(new PropertyKey("industry"))));
        assertSucceedsWith(Literals.int32(1000042), map(encodeVertex(VERTEX_ORGANIZATION_1, unify),
                v -> v.properties.get(new PropertyKey("numberOfEmployees"))));

        assertSucceedsWith(Literals.string("Infinidim Enterprises"),
                map(encodeVertex(VERTEX_ORGANIZATION_2, unify),
                        v -> v.properties.get(new PropertyKey("name"))));
        assertSucceedsWith(Literals.string("Infinidim"), map(encodeVertex(VERTEX_ORGANIZATION_2, unify),
                v -> v.properties.get(new PropertyKey("nickname"))));
        assertSucceedsWith(Literals.string("all-powerful conglomerates"),
                map(encodeVertex(VERTEX_ORGANIZATION_2, unify),
                        v -> v.properties.get(new PropertyKey("industry"))));
        assertSucceedsWith(true, map(encodeVertex(VERTEX_ORGANIZATION_2, unify),
                v -> v.properties.get(new PropertyKey("numberOfEmployees")) == null));
    }

    @Test
    public void testEdgeEncodingIsAsExpected() {
        boolean unify = DO_NOT_UNIFY;

        assertSucceedsWith(Literals.string("worksAt_ford-megadodo"), map(encodeEdge(EDGE_WORKSAT_1, unify), e -> e.id));
        assertSucceedsWith(EDGE_WORKSAT_1.label, map(encodeEdge(EDGE_WORKSAT_1, unify), e -> e.label));
        assertSucceedsWith(Literals.string("person_ford"), map(encodeEdge(EDGE_WORKSAT_1, unify), e -> e.out));
        assertSucceedsWith(Literals.string("organization_megadodo"), map(encodeEdge(EDGE_WORKSAT_1, unify), e -> e.in));
        assertSucceedsWith(true, map(encodeEdge(EDGE_WORKSAT_1, unify),
                v -> v.properties.get(new PropertyKey("employeeStatus")) == null));
        assertSucceedsWith(Literals.string("current"), map(encodeEdge(EDGE_WORKSAT_1, unify),
                v -> v.properties.get(new PropertyKey("worksAt_employeeStatus"))));

        assertSucceedsWith(Literals.string("person_hoopy"), map(encodeEdge(EDGE_FOUNDED_1, unify), e -> e.out));
        assertSucceedsWith(Literals.string("organization_megadodo"), map(encodeEdge(EDGE_FOUNDED_1, unify), e -> e.in));
        assertSucceedsWith(Literals.float32(51.0f), map(encodeEdge(EDGE_FOUNDED_1, unify),
                v -> v.properties.get(new PropertyKey("founded_ownershipPercentage"))));

        assertSucceedsWith(Literals.string("organization_megadodo"), map(encodeEdge(EDGE_PARTOF_1, unify), e -> e.out));
        assertSucceedsWith(Literals.string("organization_infinidim"), map(encodeEdge(EDGE_PARTOF_1, unify), e -> e.in));
    }

    @Test
    public void testVertexRoundTripsAreNoop() {
        Either<String, StatelessAdapter<
                List<VertexType<LiteralType>>,
                VertexType<LiteralType>,
                Vertex<Literal>,
                Vertex<Literal>>> adapterResult
                = Merging.createVertexAdapter(
                Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_ORGANIZATION),
                Merging.STRING_ID_ADAPTERS,
                DO_NOT_UNIFY);
        checkResult(adapterResult, adapter -> {
            assertRoundTripIsNoop(adapter.coder, VERTEX_PERSON_1);
            assertRoundTripIsNoop(adapter.coder, VERTEX_PERSON_2);
            assertRoundTripIsNoop(adapter.coder, VERTEX_PERSON_3);
            assertRoundTripIsNoop(adapter.coder, VERTEX_ORGANIZATION_1);
            assertRoundTripIsNoop(adapter.coder, VERTEX_ORGANIZATION_2);
        });
    }

    @Test
    public void testEdgeRoundTripsAreNoop() {
        Either<String, StatelessAdapter<
                List<EdgeType<LiteralType>>,
                EdgeType<LiteralType>,
                Edge<Literal>,
                Edge<Literal>>> adapterResult
                = Merging.createEdgeAdapter(
                Arrays.asList(EDGE_TYPE_WORKSAT_A, EDGE_TYPE_FOUNDED, EDGE_TYPE_PARTOF),
                Merging.STRING_ID_ADAPTERS,
                DO_NOT_UNIFY);
        checkResult(adapterResult, adapter -> {
            assertRoundTripIsNoop(adapter.coder, EDGE_WORKSAT_1);
            assertRoundTripIsNoop(adapter.coder, EDGE_FOUNDED_1);
            assertRoundTripIsNoop(adapter.coder, EDGE_PARTOF_1);
        });
    }

    /** Helper: create vertex adapter for standard types. */
    private static Either<String, StatelessAdapter<List<VertexType<LiteralType>>, VertexType<LiteralType>, Vertex<Literal>, Vertex<Literal>>>
    createVA(boolean unify) {
        return Merging.createVertexAdapter(
                Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_ORGANIZATION),
                Merging.STRING_ID_ADAPTERS, unify);
    }

    private static Either<String, Vertex<Literal>> encodeVertex(Vertex<Literal> v,
                                                            boolean unifyIdenticalTypes) {
        Either<String, StatelessAdapter<List<VertexType<LiteralType>>, VertexType<LiteralType>, Vertex<Literal>, Vertex<Literal>>> adapterResult =
                Merging.createVertexAdapter(
                        Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_ORGANIZATION),
                        Merging.STRING_ID_ADAPTERS,
                        unifyIdenticalTypes);
        if (adapterResult.isLeft()) return Either.left(((Either.Left<String, ?>) adapterResult).value);
        StatelessAdapter<List<VertexType<LiteralType>>, VertexType<LiteralType>, Vertex<Literal>, Vertex<Literal>> adapter =
                ((Either.Right<String, StatelessAdapter<List<VertexType<LiteralType>>, VertexType<LiteralType>, Vertex<Literal>, Vertex<Literal>>>) adapterResult).value;
        return applyCoderEncode(adapter.coder, v);
    }

    private static Either<String, Edge<Literal>> encodeEdge(Edge<Literal> e,
                                                        boolean unifyIdenticalTypes) {
        Either<String, StatelessAdapter<List<EdgeType<LiteralType>>, EdgeType<LiteralType>, Edge<Literal>, Edge<Literal>>> adapterResult =
                Merging.createEdgeAdapter(
                        Arrays.asList(EDGE_TYPE_WORKSAT_A, EDGE_TYPE_FOUNDED, EDGE_TYPE_PARTOF),
                        Merging.STRING_ID_ADAPTERS,
                        unifyIdenticalTypes);
        if (adapterResult.isLeft()) return Either.left(((Either.Left<String, ?>) adapterResult).value);
        StatelessAdapter<List<EdgeType<LiteralType>>, EdgeType<LiteralType>, Edge<Literal>, Edge<Literal>> adapter =
                ((Either.Right<String, StatelessAdapter<List<EdgeType<LiteralType>>, EdgeType<LiteralType>, Edge<Literal>, Edge<Literal>>>) adapterResult).value;
        return applyCoderEncode(adapter.coder, e);
    }

    /**
     * Apply a Coder's encode function using an empty context, returning Either<String, B>.
     */
    private static <A, B> Either<String, B> applyCoderEncode(
            hydra.coders.Coder<A, B> coder, A value) {
        hydra.context.Context cx = new hydra.context.Context(
            hydra.util.ConsList.empty(),
            hydra.util.ConsList.empty(),
            hydra.util.PersistentMap.empty());
        hydra.util.Either<hydra.errors.Error_, B> result = coder.encode.apply(cx).apply(value);
        if (result.isRight()) {
            return Either.right(((Either.Right<hydra.errors.Error_, B>) result).value);
        } else {
            return Either.left(hydra.show.Errors.error(((Either.Left<hydra.errors.Error_, B>) result).value));
        }
    }
}

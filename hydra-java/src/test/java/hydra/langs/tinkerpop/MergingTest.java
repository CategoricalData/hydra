package hydra.langs.tinkerpop;

import hydra.Flows;
import hydra.HydraTestBase;
import hydra.compute.Flow;
import hydra.compute.StatelessAdapter;
import hydra.core.Literal;
import hydra.core.LiteralType;
import hydra.dsl.LiteralTypes;
import hydra.dsl.Literals;
import hydra.langs.tinkerpop.dsl.Graphs;
import hydra.langs.tinkerpop.propertyGraph.Edge;
import hydra.langs.tinkerpop.propertyGraph.EdgeType;
import hydra.langs.tinkerpop.propertyGraph.PropertyKey;
import hydra.langs.tinkerpop.propertyGraph.Vertex;
import hydra.langs.tinkerpop.propertyGraph.VertexType;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;
import java.util.function.Function;

public class MergingTest extends HydraTestBase {
    private static LiteralType idType = LiteralTypes.string();

    private static final VertexType<LiteralType> VERTEX_TYPE_PERSON_A = Graphs.vertexType("Person", idType)
            .property("name", LiteralTypes.string(), true)
            .property("nickname", LiteralTypes.string(), false)
            .build();
    private static final VertexType<LiteralType> VERTEX_TYPE_PERSON_B = VERTEX_TYPE_PERSON_A.withId(LiteralTypes.int32());
    private static final VertexType<LiteralType> VERTEX_TYPE_PERSON_C = VERTEX_TYPE_PERSON_A.withProperties(Arrays.asList());

    private static final VertexType<LiteralType> VERTEX_TYPE_ORGANIZATION = Graphs.vertexType("Organization", idType)
            .property("name", LiteralTypes.string(), true)
            .property("industry", LiteralTypes.string(), true)
            .property("numberOfEmployees", LiteralTypes.int32(), false)
            .build();

    private static final EdgeType<LiteralType> EDGE_TYPE_WORKSAT_A = Graphs.edgeType("worksAt", idType, "Person", "Organization")
            .property("employeeStatus", LiteralTypes.string(), true)
            .property("effectiveAtUnixSeconds", LiteralTypes.uint32(), false)
            .build();
    private static final EdgeType<LiteralType> EDGE_TYPE_WORKSAT_B = EDGE_TYPE_WORKSAT_A.withId(LiteralTypes.int32());
    private static final EdgeType<LiteralType> EDGE_TYPE_WORKSAT_C = EDGE_TYPE_WORKSAT_A.withProperties(Arrays.asList());

    private static final EdgeType<LiteralType> EDGE_TYPE_FOUNDED = Graphs.edgeType("founded", idType, "Person", "Organization")
            .property("ownershipPercentage", LiteralTypes.float32(), false)
            .build();

    private static final EdgeType<LiteralType> EDGE_TYPE_PARTOF = Graphs.edgeType("partOf", idType, "Organization", "Organization")
            .build();

    private static final Vertex<Literal> VERTEX_PERSON_1 = Graphs.vertex(VERTEX_TYPE_PERSON_A, Literals.string("arthur"))
            .property("name", Literals.string("Arthur Dent"))
            .build();
    private static final Vertex<Literal> VERTEX_PERSON_2 = Graphs.vertex(VERTEX_TYPE_PERSON_A, Literals.string("ford"))
            .property("name", Literals.string("Ford Prefect"))
            .property("nickname", Literals.string("Ix"))
            .build();
    private static final Vertex<Literal> VERTEX_PERSON_3 = Graphs.vertex(VERTEX_TYPE_PERSON_A, Literals.string("hoopy"))
            .property("name", Literals.string("Some Hoopy Frood"))
            .build();
    private static final Vertex<Literal> VERTEX_ORGANIZATION_1 = Graphs.vertex(VERTEX_TYPE_ORGANIZATION, Literals.string("megadodo"))
            .property("name", Literals.string("Megadodo Publications"))
            .property("industry", Literals.string("publishers"))
            .property("numberOfEmployees", Literals.int32(1000042))
            .build();
    private static final Vertex<Literal> VERTEX_ORGANIZATION_2 = Graphs.vertex(VERTEX_TYPE_ORGANIZATION, Literals.string("infidim"))
            .property("name", Literals.string("Infinidim Enterprises"))
            .property("industry", Literals.string("all-powerful conglomerates"))
            .build();
    private static final Edge<Literal> EDGE_WORKSAT_1 = Graphs.edge(EDGE_TYPE_WORKSAT_A, Literals.string("ford-megadodo"),
                    VERTEX_PERSON_2.id, VERTEX_ORGANIZATION_1.id)
            .property("employeeStatus", Literals.string("current"))
            .property("effectiveAtUnixSeconds", Literals.uint32(252460800))
            .build();
    private static final Edge<Literal> EDGE_FOUNDED_1 = Graphs.edge(EDGE_TYPE_FOUNDED, Literals.string("hoopy-founded"),
                    VERTEX_PERSON_3.id, VERTEX_ORGANIZATION_1.id)
            .property("ownershipPercentage", Literals.float32(51.0f))
            .build();
    private static final Edge<Literal> EDGE_PARTOF_1 = Graphs.edge(EDGE_TYPE_PARTOF, Literals.string("megadodo-parent"),
                    VERTEX_ORGANIZATION_1.id, VERTEX_ORGANIZATION_2.id)
            .build();

    @Test
    public void testFailOnEmptyListOfInputTypes() {
        assertFails(Merging.createVertexAdapter(Arrays.asList()));
        assertFails(Merging.createEdgeAdapter(Arrays.asList()));
    }

    @Test
    public void testFailOnMismatchedIdTypes() {
        assertFails(Merging.createVertexAdapter(Arrays.asList(VERTEX_TYPE_PERSON_B, VERTEX_TYPE_ORGANIZATION)));
        assertFails(Merging.createEdgeAdapter(Arrays.asList(EDGE_TYPE_WORKSAT_B, EDGE_TYPE_FOUNDED)));
    }

    @Test
    public void failOnDuplicateLabels() {
        assertFails(Merging.createVertexAdapter(Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_PERSON_C)));
        assertFails(Merging.createEdgeAdapter(Arrays.asList(EDGE_TYPE_WORKSAT_A, EDGE_TYPE_WORKSAT_C)));
    }

    @Test
    public void testVertexTypeIsAsExpected() {
        assertSucceedsWith(2,
                Flows.map(Merging.createVertexAdapter(Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_ORGANIZATION)),
                        adapter -> adapter.source.size()));
        assertSucceedsWith(Merging.DEFAULT_VERTEX_LABEL,
                Flows.map(Merging.createVertexAdapter(Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_ORGANIZATION)),
                        adapter -> adapter.target.label));
        assertSucceedsWith(5,
                Flows.map(Merging.createVertexAdapter(Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_ORGANIZATION)),
                        adapter -> adapter.target.properties.size()));
        assertSucceedsWith(false,
                Flows.map(Merging.createVertexAdapter(Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_ORGANIZATION)),
                        adapter -> adapter.isLossy));

        assertSucceedsWith(new PropertyKey("person_name"),
                Flows.map(Merging.createVertexAdapter(Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_ORGANIZATION)),
                        adapter -> adapter.target.properties.get(0).key));
        assertSucceedsWith(new PropertyKey("person_nickname"),
                Flows.map(Merging.createVertexAdapter(Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_ORGANIZATION)),
                        adapter -> adapter.target.properties.get(1).key));
        assertSucceedsWith(new PropertyKey("organization_name"),
                Flows.map(Merging.createVertexAdapter(Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_ORGANIZATION)),
                        adapter -> adapter.target.properties.get(2).key));

        assertSucceedsWith(false,
                Flows.map(Merging.createVertexAdapter(Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_ORGANIZATION)),
                        adapter -> adapter.target.properties.get(0).required));
        assertSucceedsWith(LiteralTypes.string(),
                Flows.map(Merging.createVertexAdapter(Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_ORGANIZATION)),
                        adapter -> adapter.target.properties.get(0).value));
    }

    @Test
    public void testVertexEncodingIsAsExpected() {
        assertSucceedsWith(VERTEX_PERSON_1.id, Flows.map(encodeVertex(VERTEX_PERSON_1),
                v -> v.id));
        assertSucceedsWith(VERTEX_PERSON_1.label, Flows.map(encodeVertex(VERTEX_PERSON_1), v -> v.label));
        assertSucceedsWith(true, Flows.map(encodeVertex(VERTEX_PERSON_1),
                v -> v.properties.get(new PropertyKey("name")) == null));
        assertSucceedsWith(true, Flows.map(encodeVertex(VERTEX_PERSON_1),
                v -> v.properties.get(new PropertyKey("organization_name")) == null));
        assertSucceedsWith(Literals.string("Arthur Dent"), Flows.map(encodeVertex(VERTEX_PERSON_1),
                v -> v.properties.get(new PropertyKey("person_name"))));
        assertSucceedsWith(true, Flows.map(encodeVertex(VERTEX_PERSON_1),
                v -> v.properties.get(new PropertyKey("person_nickname")) == null));

        assertSucceedsWith(Literals.string("Ford Prefect"), Flows.map(encodeVertex(VERTEX_PERSON_2),
                v -> v.properties.get(new PropertyKey("person_name"))));
        assertSucceedsWith(Literals.string("Ix"), Flows.map(encodeVertex(VERTEX_PERSON_2),
                v -> v.properties.get(new PropertyKey("person_nickname"))));

        assertSucceedsWith(VERTEX_ORGANIZATION_1.id, Flows.map(encodeVertex(VERTEX_ORGANIZATION_1),
                v -> v.id));
        assertSucceedsWith(VERTEX_ORGANIZATION_1.label, Flows.map(encodeVertex(VERTEX_ORGANIZATION_1), v -> v.label));
        assertSucceedsWith(true, Flows.map(encodeVertex(VERTEX_ORGANIZATION_1),
                v -> v.properties.get(new PropertyKey("name")) == null));
        assertSucceedsWith(true, Flows.map(encodeVertex(VERTEX_ORGANIZATION_1),
                v -> v.properties.get(new PropertyKey("person_name")) == null));
        assertSucceedsWith(Literals.string("Megadodo Publications"), Flows.map(encodeVertex(VERTEX_ORGANIZATION_1),
                v -> v.properties.get(new PropertyKey("organization_name"))));
        assertSucceedsWith(Literals.string("publishers"), Flows.map(encodeVertex(VERTEX_ORGANIZATION_1),
                v -> v.properties.get(new PropertyKey("organization_industry"))));
        assertSucceedsWith(Literals.int32(1000042), Flows.map(encodeVertex(VERTEX_ORGANIZATION_1),
                v -> v.properties.get(new PropertyKey("organization_numberOfEmployees"))));

        assertSucceedsWith(Literals.string("Infinidim Enterprises"), Flows.map(encodeVertex(VERTEX_ORGANIZATION_2),
                v -> v.properties.get(new PropertyKey("organization_name"))));
        assertSucceedsWith(Literals.string("all-powerful conglomerates"), Flows.map(encodeVertex(VERTEX_ORGANIZATION_2),
                v -> v.properties.get(new PropertyKey("organization_industry"))));
        assertSucceedsWith(true, Flows.map(encodeVertex(VERTEX_ORGANIZATION_2),
                v -> v.properties.get(new PropertyKey("organization_numberOfEmployees")) == null));
    }

    @Test
    public void testEdgeEncodingIsAsExpected() {
        assertSucceedsWith(EDGE_WORKSAT_1.id, Flows.map(encodeEdge(EDGE_WORKSAT_1),
                e -> e.id));
        assertSucceedsWith(EDGE_WORKSAT_1.label, Flows.map(encodeEdge(EDGE_WORKSAT_1),
                e -> e.label));
        assertSucceedsWith(VERTEX_PERSON_2.id, Flows.map(encodeEdge(EDGE_WORKSAT_1),
                e -> e.out));
        assertSucceedsWith(VERTEX_ORGANIZATION_1.id, Flows.map(encodeEdge(EDGE_WORKSAT_1),
                e -> e.in));
        assertSucceedsWith(true, Flows.map(encodeEdge(EDGE_WORKSAT_1),
                v -> v.properties.get(new PropertyKey("employeeStatus")) == null));
        assertSucceedsWith(Literals.string("current"), Flows.map(encodeEdge(EDGE_WORKSAT_1),
                v -> v.properties.get(new PropertyKey("worksat_employeeStatus"))));

        assertSucceedsWith(VERTEX_PERSON_3.id, Flows.map(encodeEdge(EDGE_FOUNDED_1),
                e -> e.out));
        assertSucceedsWith(VERTEX_ORGANIZATION_1.id, Flows.map(encodeEdge(EDGE_FOUNDED_1),
                e -> e.in));
        assertSucceedsWith(Literals.float32(51.0f), Flows.map(encodeEdge(EDGE_FOUNDED_1),
                v -> v.properties.get(new PropertyKey("founded_ownershipPercentage"))));

        assertSucceedsWith(VERTEX_ORGANIZATION_1.id, Flows.map(encodeEdge(EDGE_PARTOF_1),
                e -> e.out));
        assertSucceedsWith(VERTEX_ORGANIZATION_2.id, Flows.map(encodeEdge(EDGE_PARTOF_1),
                e -> e.in));
    }

    @Test
    public void testVertexRoundTripsAreNoop() {
        Flow<Void, StatelessAdapter<List<VertexType<LiteralType>>, VertexType<LiteralType>, Vertex<Literal>, Vertex<Literal>>> adapterFlow
                = Merging.createVertexAdapter(Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_ORGANIZATION));
        Flows.map(adapterFlow, adapter -> {
            assertRoundTripIsNoop(adapter.coder, VERTEX_PERSON_1);
            assertRoundTripIsNoop(adapter.coder, VERTEX_PERSON_2);
            assertRoundTripIsNoop(adapter.coder, VERTEX_PERSON_3);
            assertRoundTripIsNoop(adapter.coder, VERTEX_ORGANIZATION_1);
            assertRoundTripIsNoop(adapter.coder, VERTEX_ORGANIZATION_2);
            return true;
        });
    }

    @Test
    public void testEdgeRoundTripsAreNoop() {
        Flow<Void, StatelessAdapter<List<EdgeType<LiteralType>>, EdgeType<LiteralType>, Edge<Literal>, Edge<Literal>>> adapterFlow
                = Merging.createEdgeAdapter(Arrays.asList(EDGE_TYPE_WORKSAT_A, EDGE_TYPE_FOUNDED, EDGE_TYPE_PARTOF));
        Flows.map(adapterFlow, adapter -> {
            assertRoundTripIsNoop(adapter.coder, EDGE_WORKSAT_1);
            assertRoundTripIsNoop(adapter.coder, EDGE_FOUNDED_1);
            assertRoundTripIsNoop(adapter.coder, EDGE_PARTOF_1);
            return true;
        });
    }

    private static Flow<Void, Vertex<Literal>> encodeVertex(Vertex<Literal> v) {
        return Flows.bind(
                Merging.createVertexAdapter(Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_ORGANIZATION)),
                (Function<StatelessAdapter<List<VertexType<LiteralType>>, VertexType<LiteralType>, Vertex<Literal>, Vertex<Literal>>, Flow<Void, Vertex<Literal>>>)
                        adapter -> adapter.coder.encode.apply(v));
    }

    private static Flow<Void, Edge<Literal>> encodeEdge(Edge<Literal> e) {
        return Flows.bind(
                Merging.createEdgeAdapter(Arrays.asList(EDGE_TYPE_WORKSAT_A, EDGE_TYPE_FOUNDED, EDGE_TYPE_PARTOF)),
                (Function<StatelessAdapter<List<EdgeType<LiteralType>>, EdgeType<LiteralType>, Edge<Literal>, Edge<Literal>>, Flow<Void, Edge<Literal>>>)
                        adapter -> adapter.coder.encode.apply(e));
    }
}

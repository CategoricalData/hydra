package hydra.langs.tinkerpop;

import hydra.Flows;
import hydra.compute.Flow;
import hydra.compute.StatelessAdapter;
import hydra.core.Literal;
import hydra.core.LiteralType;
import hydra.core.Unit;
import hydra.dsl.LiteralTypes;
import hydra.dsl.Literals;
import hydra.langs.tinkerpop.propertyGraph.Edge;
import hydra.langs.tinkerpop.propertyGraph.EdgeType;
import hydra.langs.tinkerpop.propertyGraph.PropertyKey;
import hydra.langs.tinkerpop.propertyGraph.Vertex;
import hydra.langs.tinkerpop.propertyGraph.VertexType;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;
import java.util.function.Function;

public class MergingTest extends PropertyGraphTestBase {
    @Test
    public void testFailOnEmptyListOfInputTypes() {
        assertFails(Merging.createVertexAdapter(Arrays.asList(), Merging.STRING_ID_ADAPTERS));
        assertFails(Merging.createEdgeAdapter(Arrays.asList(), Merging.STRING_ID_ADAPTERS));
    }

    @Test
    public void failOnDuplicateLabels() {
        assertFails(Merging.createVertexAdapter(Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_PERSON_C), Merging.STRING_ID_ADAPTERS));
        assertFails(Merging.createEdgeAdapter(Arrays.asList(EDGE_TYPE_WORKSAT_A, EDGE_TYPE_WORKSAT_C), Merging.STRING_ID_ADAPTERS));
    }

    @Test
    public void testVertexTypeIsAsExpected() {
        assertSucceedsWith(2,
                Flows.map(Merging.createVertexAdapter(Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_ORGANIZATION), Merging.STRING_ID_ADAPTERS),
                        adapter -> adapter.source.size()));
        assertSucceedsWith(Merging.DEFAULT_VERTEX_LABEL,
                Flows.map(Merging.createVertexAdapter(Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_ORGANIZATION), Merging.STRING_ID_ADAPTERS),
                        adapter -> adapter.target.label));
        assertSucceedsWith(5,
                Flows.map(Merging.createVertexAdapter(Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_ORGANIZATION), Merging.STRING_ID_ADAPTERS),
                        adapter -> adapter.target.properties.size()));
        assertSucceedsWith(false,
                Flows.map(Merging.createVertexAdapter(Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_ORGANIZATION), Merging.STRING_ID_ADAPTERS),
                        adapter -> adapter.isLossy));

        assertSucceedsWith(new PropertyKey("person_name"),
                Flows.map(Merging.createVertexAdapter(Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_ORGANIZATION), Merging.STRING_ID_ADAPTERS),
                        adapter -> adapter.target.properties.get(0).key));
        assertSucceedsWith(new PropertyKey("person_nickname"),
                Flows.map(Merging.createVertexAdapter(Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_ORGANIZATION), Merging.STRING_ID_ADAPTERS),
                        adapter -> adapter.target.properties.get(1).key));
        assertSucceedsWith(new PropertyKey("organization_name"),
                Flows.map(Merging.createVertexAdapter(Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_ORGANIZATION), Merging.STRING_ID_ADAPTERS),
                        adapter -> adapter.target.properties.get(2).key));

        assertSucceedsWith(false,
                Flows.map(Merging.createVertexAdapter(Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_ORGANIZATION), Merging.STRING_ID_ADAPTERS),
                        adapter -> adapter.target.properties.get(0).required));
        assertSucceedsWith(LiteralTypes.string(),
                Flows.map(Merging.createVertexAdapter(Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_ORGANIZATION), Merging.STRING_ID_ADAPTERS),
                        adapter -> adapter.target.properties.get(0).value));
    }

    @Test
    public void testVertexEncodingIsAsExpected() {
        assertSucceedsWith(Literals.string("person_arthur"), Flows.map(encodeVertex(VERTEX_PERSON_1),
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

        assertSucceedsWith(Literals.string("organization_megadodo"), Flows.map(encodeVertex(VERTEX_ORGANIZATION_1),
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
        assertSucceedsWith(Literals.string("worksAt_ford-megadodo"), Flows.map(encodeEdge(EDGE_WORKSAT_1),
                e -> e.id));
        assertSucceedsWith(EDGE_WORKSAT_1.label, Flows.map(encodeEdge(EDGE_WORKSAT_1),
                e -> e.label));
        assertSucceedsWith(Literals.string("person_ford"), Flows.map(encodeEdge(EDGE_WORKSAT_1),
                e -> e.out));
        assertSucceedsWith(Literals.string("organization_megadodo"), Flows.map(encodeEdge(EDGE_WORKSAT_1),
                e -> e.in));
        assertSucceedsWith(true, Flows.map(encodeEdge(EDGE_WORKSAT_1),
                v -> v.properties.get(new PropertyKey("employeeStatus")) == null));
        assertSucceedsWith(Literals.string("current"), Flows.map(encodeEdge(EDGE_WORKSAT_1),
                v -> v.properties.get(new PropertyKey("worksAt_employeeStatus"))));

        assertSucceedsWith(Literals.string("person_hoopy"), Flows.map(encodeEdge(EDGE_FOUNDED_1),
                e -> e.out));
        assertSucceedsWith(Literals.string("organization_megadodo"), Flows.map(encodeEdge(EDGE_FOUNDED_1),
                e -> e.in));
        assertSucceedsWith(Literals.float32(51.0f), Flows.map(encodeEdge(EDGE_FOUNDED_1),
                v -> v.properties.get(new PropertyKey("founded_ownershipPercentage"))));

        assertSucceedsWith(Literals.string("organization_megadodo"), Flows.map(encodeEdge(EDGE_PARTOF_1),
                e -> e.out));
        assertSucceedsWith(Literals.string("organization_infinidim"), Flows.map(encodeEdge(EDGE_PARTOF_1),
                e -> e.in));
    }

    @Test
    public void testVertexRoundTripsAreNoop() {
        Flow<Unit, StatelessAdapter<List<VertexType<LiteralType>>, VertexType<LiteralType>, Vertex<Literal>, Vertex<Literal>>> adapterFlow
                = Merging.createVertexAdapter(Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_ORGANIZATION), Merging.STRING_ID_ADAPTERS);
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
        Flow<Unit, StatelessAdapter<List<EdgeType<LiteralType>>, EdgeType<LiteralType>, Edge<Literal>, Edge<Literal>>> adapterFlow
                = Merging.createEdgeAdapter(Arrays.asList(EDGE_TYPE_WORKSAT_A, EDGE_TYPE_FOUNDED, EDGE_TYPE_PARTOF), Merging.STRING_ID_ADAPTERS);
        Flows.map(adapterFlow, adapter -> {
            assertRoundTripIsNoop(adapter.coder, EDGE_WORKSAT_1);
            assertRoundTripIsNoop(adapter.coder, EDGE_FOUNDED_1);
            assertRoundTripIsNoop(adapter.coder, EDGE_PARTOF_1);
            return true;
        });
    }

    private static Flow<Unit, Vertex<Literal>> encodeVertex(Vertex<Literal> v) {
        return Flows.bind(
                Merging.createVertexAdapter(Arrays.asList(VERTEX_TYPE_PERSON_A, VERTEX_TYPE_ORGANIZATION), Merging.STRING_ID_ADAPTERS),
                (Function<StatelessAdapter<List<VertexType<LiteralType>>, VertexType<LiteralType>, Vertex<Literal>, Vertex<Literal>>, Flow<Unit, Vertex<Literal>>>)
                        adapter -> adapter.coder.encode.apply(v));
    }

    private static Flow<Unit, Edge<Literal>> encodeEdge(Edge<Literal> e) {
        return Flows.bind(
                Merging.createEdgeAdapter(Arrays.asList(EDGE_TYPE_WORKSAT_A, EDGE_TYPE_FOUNDED, EDGE_TYPE_PARTOF), Merging.STRING_ID_ADAPTERS),
                (Function<StatelessAdapter<List<EdgeType<LiteralType>>, EdgeType<LiteralType>, Edge<Literal>, Edge<Literal>>, Flow<Unit, Edge<Literal>>>)
                        adapter -> adapter.coder.encode.apply(e));
    }
}

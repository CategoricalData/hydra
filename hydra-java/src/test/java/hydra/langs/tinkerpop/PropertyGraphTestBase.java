package hydra.langs.tinkerpop;

import hydra.HydraTestBase;
import hydra.core.Literal;
import hydra.core.LiteralType;
import hydra.dsl.LiteralTypes;
import hydra.dsl.Literals;
import hydra.langs.tinkerpop.dsl.Graphs;
import hydra.langs.tinkerpop.propertyGraph.Edge;
import hydra.langs.tinkerpop.propertyGraph.EdgeType;
import hydra.langs.tinkerpop.propertyGraph.Element;
import hydra.langs.tinkerpop.propertyGraph.ElementType;
import hydra.langs.tinkerpop.propertyGraph.Vertex;
import hydra.langs.tinkerpop.propertyGraph.VertexType;

import java.util.Arrays;


public abstract class PropertyGraphTestBase extends HydraTestBase {
    protected static final LiteralType ID_TYPE = LiteralTypes.string();

    protected static final VertexType<LiteralType> VERTEX_TYPE_PERSON_A = Graphs.vertexType("Person", ID_TYPE)
            .property("name", LiteralTypes.string(), true)
            .property("nickname", LiteralTypes.string(), false)
            .property("age", LiteralTypes.int32(), false)
            .build();
    protected static final VertexType<LiteralType> VERTEX_TYPE_PERSON_B
            = VERTEX_TYPE_PERSON_A.withId(LiteralTypes.int32());
    protected static final VertexType<LiteralType> VERTEX_TYPE_PERSON_C
            = VERTEX_TYPE_PERSON_A.withProperties(Arrays.asList());

    protected static final VertexType<LiteralType> VERTEX_TYPE_ORGANIZATION = Graphs.vertexType(
            "Organization", ID_TYPE)
            .property("name", LiteralTypes.string(), true)
            // Intentionally different in requiredness with Person.nickname
            .property("nickname", LiteralTypes.string(), true)
            // Intentionally different in datatype from Person.age
            .property("age", LiteralTypes.int64(), false)
            .property("industry", LiteralTypes.string(), true)
            .property("numberOfEmployees", LiteralTypes.int64(), false)
            .build();

    protected static final EdgeType<LiteralType> EDGE_TYPE_WORKSAT_A = Graphs.edgeType(
            "worksAt", ID_TYPE, "Person", "Organization")
            .property("employeeStatus", LiteralTypes.string(), true)
            .property("effectiveAtUnixSeconds", LiteralTypes.uint32(), false)
            .build();
    protected static final EdgeType<LiteralType> EDGE_TYPE_WORKSAT_B
            = EDGE_TYPE_WORKSAT_A.withId(LiteralTypes.int32());
    protected static final EdgeType<LiteralType> EDGE_TYPE_WORKSAT_C
            = EDGE_TYPE_WORKSAT_A.withProperties(Arrays.asList());

    protected static final EdgeType<LiteralType> EDGE_TYPE_FOUNDED = Graphs.edgeType(
            "founded", ID_TYPE, "Person", "Organization")
            .property("ownershipPercentage", LiteralTypes.float32(), false)
            .build();

    protected static final EdgeType<LiteralType> EDGE_TYPE_PARTOF = Graphs.edgeType(
                    "partOf", ID_TYPE, "Organization", "Organization")
            .build();

    protected static final Vertex<Literal> VERTEX_PERSON_1 = Graphs.vertex(
                    VERTEX_TYPE_PERSON_A, Literals.string("arthur"))
            .property("name", Literals.string("Arthur Dent"))
            .property("age", Literals.int32(42))
            .build();
    protected static final Vertex<Literal> VERTEX_PERSON_2 = Graphs.vertex(
                    VERTEX_TYPE_PERSON_A, Literals.string("ford"))
            .property("name", Literals.string("Ford Prefect"))
            .property("nickname", Literals.string("Ix"))
            .build();
    protected static final Vertex<Literal> VERTEX_PERSON_3 = Graphs.vertex(
                    VERTEX_TYPE_PERSON_A, Literals.string("hoopy"))
            .property("name", Literals.string("Some Hoopy Frood"))
            .build();
    protected static final Vertex<Literal> VERTEX_ORGANIZATION_1 = Graphs.vertex(
                    VERTEX_TYPE_ORGANIZATION, Literals.string("megadodo"))
            .property("name", Literals.string("Megadodo Publications"))
            .property("nickname", Literals.string("Megadodo"))
            .property("industry", Literals.string("publishers"))
            .property("numberOfEmployees", Literals.int32(1000042))
            .build();
    protected static final Vertex<Literal> VERTEX_ORGANIZATION_2 = Graphs.vertex(
                    VERTEX_TYPE_ORGANIZATION, Literals.string("infinidim"))
            .property("name", Literals.string("Infinidim Enterprises"))
            .property("nickname", Literals.string("Infinidim"))
            .property("industry", Literals.string("all-powerful conglomerates"))
            .build();
    protected static final Edge<Literal> EDGE_WORKSAT_1 = Graphs.edge(
                    EDGE_TYPE_WORKSAT_A,
                    Literals.string("ford-megadodo"),
                    VERTEX_PERSON_2.id,
                    VERTEX_ORGANIZATION_1.id)
            .property("employeeStatus", Literals.string("current"))
            .property("effectiveAtUnixSeconds", Literals.uint32(252460800))
            .build();
    protected static final Edge<Literal> EDGE_FOUNDED_1 = Graphs.edge(
                    EDGE_TYPE_FOUNDED,
                    Literals.string("hoopy-founded"),
                    VERTEX_PERSON_3.id,
                    VERTEX_ORGANIZATION_1.id)
            .property("ownershipPercentage", Literals.float32(51.0f))
            .build();
    protected static final Edge<Literal> EDGE_PARTOF_1 = Graphs.edge(
                    EDGE_TYPE_PARTOF,
                    Literals.string("megadodo-parent"),
                    VERTEX_ORGANIZATION_1.id,
                    VERTEX_ORGANIZATION_2.id)
            .build();

    protected static Element<Literal> toElement(Vertex<Literal> vertex) {
        return new Element.Vertex<Literal>(vertex);
    }

    protected static Element<Literal> toElement(Edge<Literal> edge) {
        return new Element.Edge<Literal>(edge);
    }

    protected static ElementType<LiteralType> toElementType(VertexType<LiteralType> vertex) {
        return new ElementType.Vertex<LiteralType>(vertex);
    }

    protected static ElementType<LiteralType> toElementType(EdgeType<LiteralType> edge) {
        return new ElementType.Edge<LiteralType>(edge);
    }

}

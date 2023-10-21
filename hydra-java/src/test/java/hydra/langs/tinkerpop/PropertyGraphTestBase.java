package hydra.langs.tinkerpop;

import hydra.HydraTestBase;
import hydra.core.FloatType;
import hydra.core.FloatValue;
import hydra.core.IntegerType;
import hydra.core.IntegerValue;
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
            .build();
    protected static final VertexType<LiteralType> VERTEX_TYPE_PERSON_B = VERTEX_TYPE_PERSON_A.withId(LiteralTypes.int32());
    protected static final VertexType<LiteralType> VERTEX_TYPE_PERSON_C = VERTEX_TYPE_PERSON_A.withProperties(Arrays.asList());

    protected static final VertexType<LiteralType> VERTEX_TYPE_ORGANIZATION = Graphs.vertexType("Organization", ID_TYPE)
            .property("name", LiteralTypes.string(), true)
            .property("industry", LiteralTypes.string(), true)
            .property("numberOfEmployees", LiteralTypes.int32(), false)
            .build();

    protected static final EdgeType<LiteralType> EDGE_TYPE_WORKSAT_A = Graphs.edgeType("worksAt", ID_TYPE, "Person", "Organization")
            .property("employeeStatus", LiteralTypes.string(), true)
            .property("effectiveAtUnixSeconds", LiteralTypes.uint32(), false)
            .build();
    protected static final EdgeType<LiteralType> EDGE_TYPE_WORKSAT_B = EDGE_TYPE_WORKSAT_A.withId(LiteralTypes.int32());
    protected static final EdgeType<LiteralType> EDGE_TYPE_WORKSAT_C = EDGE_TYPE_WORKSAT_A.withProperties(Arrays.asList());

    protected static final EdgeType<LiteralType> EDGE_TYPE_FOUNDED = Graphs.edgeType("founded", ID_TYPE, "Person", "Organization")
            .property("ownershipPercentage", LiteralTypes.float32(), false)
            .build();

    protected static final EdgeType<LiteralType> EDGE_TYPE_PARTOF = Graphs.edgeType("partOf", ID_TYPE, "Organization", "Organization")
            .build();

    protected static final Vertex<Literal> VERTEX_PERSON_1 = Graphs.vertex(VERTEX_TYPE_PERSON_A, Literals.string("arthur"))
            .property("name", Literals.string("Arthur Dent"))
            .build();
    protected static final Vertex<Literal> VERTEX_PERSON_2 = Graphs.vertex(VERTEX_TYPE_PERSON_A, Literals.string("ford"))
            .property("name", Literals.string("Ford Prefect"))
            .property("nickname", Literals.string("Ix"))
            .build();
    protected static final Vertex<Literal> VERTEX_PERSON_3 = Graphs.vertex(VERTEX_TYPE_PERSON_A, Literals.string("hoopy"))
            .property("name", Literals.string("Some Hoopy Frood"))
            .build();
    protected static final Vertex<Literal> VERTEX_ORGANIZATION_1 = Graphs.vertex(VERTEX_TYPE_ORGANIZATION, Literals.string("megadodo"))
            .property("name", Literals.string("Megadodo Publications"))
            .property("industry", Literals.string("publishers"))
            .property("numberOfEmployees", Literals.int32(1000042))
            .build();
    protected static final Vertex<Literal> VERTEX_ORGANIZATION_2 = Graphs.vertex(VERTEX_TYPE_ORGANIZATION, Literals.string("infinidim"))
            .property("name", Literals.string("Infinidim Enterprises"))
            .property("industry", Literals.string("all-powerful conglomerates"))
            .build();
    protected static final Edge<Literal> EDGE_WORKSAT_1 = Graphs.edge(EDGE_TYPE_WORKSAT_A, Literals.string("ford-megadodo"),
                    VERTEX_PERSON_2.id, VERTEX_ORGANIZATION_1.id)
            .property("employeeStatus", Literals.string("current"))
            .property("effectiveAtUnixSeconds", Literals.uint32(252460800))
            .build();
    protected static final Edge<Literal> EDGE_FOUNDED_1 = Graphs.edge(EDGE_TYPE_FOUNDED, Literals.string("hoopy-founded"),
                    VERTEX_PERSON_3.id, VERTEX_ORGANIZATION_1.id)
            .property("ownershipPercentage", Literals.float32(51.0f))
            .build();
    protected static final Edge<Literal> EDGE_PARTOF_1 = Graphs.edge(EDGE_TYPE_PARTOF, Literals.string("megadodo-parent"),
                    VERTEX_ORGANIZATION_1.id, VERTEX_ORGANIZATION_2.id)
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

    protected static String showLiteralType(LiteralType type) {
      return type.accept(new LiteralType.Visitor<String>() {
          @Override
          public String visit(LiteralType.Binary instance) {
              return "binary";
          }

          @Override
          public String visit(LiteralType.Boolean_ instance) {
              return "boolean";
          }

          @Override
          public String visit(LiteralType.Float_ instance) {
              return "float:" + instance.value.accept(new FloatType.Visitor<String>() {
                  @Override
                  public String visit(FloatType.Bigfloat instance) {
                      return "bigfloat";
                  }

                  @Override
                  public String visit(FloatType.Float32 instance) {
                      return "float32";
                  }

                  @Override
                  public String visit(FloatType.Float64 instance) {
                      return "float64";
                  }
              });
          }

          @Override
          public String visit(LiteralType.Integer_ instance) {
              return "integer:" + instance.value.accept(new IntegerType.Visitor<String>() {
                  @Override
                  public String visit(IntegerType.Bigint instance) {
                      return "bigint";
                  }

                  @Override
                  public String visit(IntegerType.Int8 instance) {
                      return "int8";
                  }

                  @Override
                  public String visit(IntegerType.Int16 instance) {
                      return "int16";
                  }

                  @Override
                  public String visit(IntegerType.Int32 instance) {
                      return "int32";
                  }

                  @Override
                  public String visit(IntegerType.Int64 instance) {
                      return "int64";
                  }

                  @Override
                  public String visit(IntegerType.Uint8 instance) {
                      return "uint8";
                  }

                  @Override
                  public String visit(IntegerType.Uint16 instance) {
                      return "uint16";
                  }

                  @Override
                  public String visit(IntegerType.Uint32 instance) {
                      return "uint32";
                  }

                  @Override
                  public String visit(IntegerType.Uint64 instance) {
                      return "uint64";
                  }
              });
          }

          @Override
          public String visit(LiteralType.String_ instance) {
              return "string";
          }
      });
    }

    protected static String showLiteral(Literal value) {
        return value.accept(new Literal.Visitor<String>() {
            @Override
            public String visit(Literal.Binary instance) {
                return "binary:" + instance.value;
            }

            @Override
            public String visit(Literal.Boolean_ instance) {
                return "boolean:" + instance.value;
            }

            @Override
            public String visit(Literal.Float_ instance) {
                return "float:" + instance.value.accept(new FloatValue.Visitor<String>() {
                    @Override
                    public String visit(FloatValue.Bigfloat instance) {
                        return "bigfloat:" + instance.value;
                    }

                    @Override
                    public String visit(FloatValue.Float32 instance) {
                        return "float32:" + instance.value;
                    }

                    @Override
                    public String visit(FloatValue.Float64 instance) {
                        return "float64:" + instance.value;
                    }
                });
            }

            @Override
            public String visit(Literal.Integer_ instance) {
                return "integer:" + instance.value.accept(new IntegerValue.Visitor<String>() {
                    @Override
                    public String visit(IntegerValue.Bigint instance) {
                        return "bigint:" + instance.value;
                    }

                    @Override
                    public String visit(IntegerValue.Int16 instance) {
                        return "int16:" + instance.value;
                    }

                    @Override
                    public String visit(IntegerValue.Int32 instance) {
                        return "int32:" + instance.value;
                    }

                    @Override
                    public String visit(IntegerValue.Int64 instance) {
                        return "int64:" + instance.value;
                    }

                    @Override
                    public String visit(IntegerValue.Int8 instance) {
                        return "int8:" + instance.value;
                    }

                    @Override
                    public String visit(IntegerValue.Uint16 instance) {
                        return "uint16:" + instance.value;
                    }

                    @Override
                    public String visit(IntegerValue.Uint32 instance) {
                        return "uint32:" + instance.value;
                    }

                    @Override
                    public String visit(IntegerValue.Uint64 instance) {
                        return "uint64:" + instance.value;
                    }

                    @Override
                    public String visit(IntegerValue.Uint8 instance) {
                        return "uint8:" + instance.value;
                    }
                });
            }

            @Override
            public String visit(Literal.String_ instance) {
                return "string:\"" + instance.value + "\"";
            }
        });
    }
}

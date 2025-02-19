// Note: this is an automatically generated file. Do not edit.

package hydra.pg.graphson.syntax;

import java.io.Serializable;

public class Vertex implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.pg.graphson.syntax.Vertex");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_LABEL = new hydra.core.Name("label");
  
  public static final hydra.core.Name FIELD_NAME_IN_EDGES = new hydra.core.Name("inEdges");
  
  public static final hydra.core.Name FIELD_NAME_OUT_EDGES = new hydra.core.Name("outEdges");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTIES = new hydra.core.Name("properties");
  
  public final hydra.pg.graphson.syntax.Value id;
  
  public final hydra.util.Opt<hydra.pg.graphson.syntax.VertexLabel> label;
  
  public final java.util.Map<hydra.pg.graphson.syntax.EdgeLabel, java.util.List<hydra.pg.graphson.syntax.AdjacentEdge>> inEdges;
  
  public final java.util.Map<hydra.pg.graphson.syntax.EdgeLabel, java.util.List<hydra.pg.graphson.syntax.AdjacentEdge>> outEdges;
  
  public final java.util.Map<hydra.pg.graphson.syntax.PropertyKey, java.util.List<hydra.pg.graphson.syntax.VertexPropertyValue>> properties;
  
  public Vertex (hydra.pg.graphson.syntax.Value id, hydra.util.Opt<hydra.pg.graphson.syntax.VertexLabel> label, java.util.Map<hydra.pg.graphson.syntax.EdgeLabel, java.util.List<hydra.pg.graphson.syntax.AdjacentEdge>> inEdges, java.util.Map<hydra.pg.graphson.syntax.EdgeLabel, java.util.List<hydra.pg.graphson.syntax.AdjacentEdge>> outEdges, java.util.Map<hydra.pg.graphson.syntax.PropertyKey, java.util.List<hydra.pg.graphson.syntax.VertexPropertyValue>> properties) {
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((label));
    java.util.Objects.requireNonNull((inEdges));
    java.util.Objects.requireNonNull((outEdges));
    java.util.Objects.requireNonNull((properties));
    this.id = id;
    this.label = label;
    this.inEdges = inEdges;
    this.outEdges = outEdges;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Vertex)) {
      return false;
    }
    Vertex o = (Vertex) (other);
    return id.equals(o.id) && label.equals(o.label) && inEdges.equals(o.inEdges) && outEdges.equals(o.outEdges) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode() + 3 * label.hashCode() + 5 * inEdges.hashCode() + 7 * outEdges.hashCode() + 11 * properties.hashCode();
  }
  
  public Vertex withId(hydra.pg.graphson.syntax.Value id) {
    java.util.Objects.requireNonNull((id));
    return new Vertex(id, label, inEdges, outEdges, properties);
  }
  
  public Vertex withLabel(hydra.util.Opt<hydra.pg.graphson.syntax.VertexLabel> label) {
    java.util.Objects.requireNonNull((label));
    return new Vertex(id, label, inEdges, outEdges, properties);
  }
  
  public Vertex withInEdges(java.util.Map<hydra.pg.graphson.syntax.EdgeLabel, java.util.List<hydra.pg.graphson.syntax.AdjacentEdge>> inEdges) {
    java.util.Objects.requireNonNull((inEdges));
    return new Vertex(id, label, inEdges, outEdges, properties);
  }
  
  public Vertex withOutEdges(java.util.Map<hydra.pg.graphson.syntax.EdgeLabel, java.util.List<hydra.pg.graphson.syntax.AdjacentEdge>> outEdges) {
    java.util.Objects.requireNonNull((outEdges));
    return new Vertex(id, label, inEdges, outEdges, properties);
  }
  
  public Vertex withProperties(java.util.Map<hydra.pg.graphson.syntax.PropertyKey, java.util.List<hydra.pg.graphson.syntax.VertexPropertyValue>> properties) {
    java.util.Objects.requireNonNull((properties));
    return new Vertex(id, label, inEdges, outEdges, properties);
  }
}